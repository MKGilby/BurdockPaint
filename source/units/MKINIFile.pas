// Version info:

//  V1.00: Gilby
//     + Initial creation
//  V1.01: Gilby
//     + ReadRGB(section,ident:string;defr,defg,defb:word;var r,g,b:word) method
//     + WriteRGB(section,ident:string;r,g,b:word) method
//     * Comparisons changed to case insensitive
//  V1.02: Gilby
//     + ReadCoords(section:string;var x1,y1,x2,y2:word) method
//  V2.00: Gilby
//     * Complete rewrite
//     * Using classes instead of objects
//  V2.01: Gilby - 2009.01.30
//     + GetSectionType method added
//  V2.02: Gilby - 2009.02.12
//     + ReadRect method added
//  V2.03: Gilby - 2010.01.22
//     + CopySection method added
//  V2.04: Gilby - 2011.01.30
//     * fSectionList renamed to fSections
//     * fKeys is now stored as fSections.Objects
//  V2.05: Gilby - 2011.04.21
//     + Read methods now can have a second section parameter. If the data
//       cannot be found in the first section, it will be readed from the 
//       second. If still no data, then the default value is returned.
//       This is not applies to List type sections. 
//  V2.06: Gilby - 2011.07.06
//     + MKStream support added (I wonder why it was missing...)
//  V2.07: Gilby - 2012.08.17
//     * Destructor is now Destroy
//  V2.08: Gilby - 2013.04.30
//     * In list type sections empty lines are not skipped (except last one)
//  V2.09: Gilby - 2014.01.14
//     * Dec2Hex is changed to IntToHex
//  V2.10: Gilby - 2015.05.20
//     * Using MKStream instead of MKStream3
//  V2.11: Gilby - 2015.10.27
//     + LogContent added 
//  V2.11a: Gilby - 2015.12.09
//     * Fixes to suppress hints in Lazarus
//  V2.12: Gilby - 2016.09.16
//     + ReadFloat and WriteFloat added
//  V2.13: Gilby - 2018.04.05
//     * ReadString now detects bounding " chars and removes them
//  V2.14: Gilby - 2019.01.04
//     * Fixed a memory leak
//  V2.15: Gilby - 2019.02.27
//     * Opening a non-existing file caused error, now it considered open, so you
//       can add keys to it and save. (Apparently I never tested it that way. :))
//  V2.16: Gilby - 2019.03.14
//     * ReadInteger now recognizes numbers with $ prefixes as hexadecimal numbers
//  V2.16a: Gilby - 2019.04.26
//     * FIX: Create didn't recognize files in MAD4.
//  V2.17: Gilby - 2019.09.04
//     * Opening a non-existing file causes error again, but only if opened as
//       read-only.
//  V2.18: Gilby - 2023.01.16
//     + Added ReadUInt32 and WriteUInt32.
//  V2.19: Gilby - 2024.04.23
//     * Changed replace to StringReplace.

{$ifdef fpc}
//  {$smartlink on}
  {$mode delphi}
{$endif}

unit MKINIFile;

interface

uses Classes;

const
  stNotFound=-1;
  stSection=0;
  stList=1;

type

  { TIniFile }

  TIniFile = class
    fFileName : string;
    fSections : TStringList;
  public
    constructor Create(iFileName: string;iReadonly:boolean=true); overload;
    constructor Create(iSource:TStream); overload;
    destructor Destroy; override;
    procedure UpdateFile;
    procedure LogContent;

    function ReadString(Section, Ident, Default: string;Section2:string=''): string;
    function ReadInteger(Section, Ident: string; Default: integer;Section2:string=''): integer;
    function ReadUInt32(Section, Ident: string; Default: UInt32;Section2:string=''): UInt32;
    function ReadFloat(Section, Ident: string; Default: extended;Section2:string=''): extended;
    function ReadBool(Section, Ident: string; Default: Boolean;Section2:string=''): Boolean;
    procedure ReadRGB(section,ident:string;defr,defg,defb:word;var r,g,b:integer;Section2:string='');
    procedure ReadCoords(section:string;var x1,y1,x2,y2:word;Section2:string='');
    procedure ReadRect(section,ident:string;var x1,y1,x2,y2:integer;Section2:string='');

    procedure WriteString(Section, Ident, Value: String);
    procedure WriteInteger(Section, Ident: string; Value: integer);
    procedure WriteUInt32(Section, Ident: string; Value: UInt32);
    procedure WriteFloat(Section, Ident: string; Value: extended);
    procedure WriteBool(Section, Ident: string; Value: Boolean);
    procedure WriteRGB(section,ident:string;r,g,b:integer);

    function ReadListItem(Section:string;index:integer):string;
    procedure WriteListItem(Section,Value:string);
    procedure DeleteListItem(Section,Value:string); overload;
    procedure DeleteListItem(Section:string;index:integer); overload;
    function ListItemCount(Section:string):integer;

    procedure DeleteSection(Section: string);
    procedure DeleteKey(Section,Ident:String);
    function ValueExists(Section,Ident:string): Boolean; virtual;
    
    function GetSectionType(aSection: string): integer;
    procedure CopySection(aSource,aTarget: string);
  end;

implementation

uses SysUtils, MKToolBox, MKStream, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='2.19';

{ Constructor - Destructor - Reading/Writing physical file }

constructor TIniFile.Create(iFileName:string;iReadonly:boolean=true);
var Xs:TStream;
begin
  fSections:=nil;
  if MKStreamOpener.FileExists(iFileName) then begin
    Xs:=MKStreamOpener.OpenStream(iFileName);
    if Xs<>nil then begin
      Create(Xs);
      FreeAndNil(Xs);
    end;
  end else begin
    if iReadonly then raise Exception.Create('INI file not found! ('+iFileName+')');
    fSections:=TStringList.Create;
  end;
  if iReadonly then fFileName:=''
               else fFileName := iFileName;
end;

constructor TIniFile.Create(iSource:TStream);
var s,LastSect:string;
    i,j:integer;
begin
  fSections:=TStringList.Create;
  fFileName:='';
  LastSect:='';
  s:='';
  repeat
    ReadStreamString(iSource,s);
    // Don't skip empty lines in list mode = Skip empty lines in section mode
    if (length(alltrim(s))=0) and (fSections.Values[LastSect]='0') then continue;
    if pos('[',s)>0 then s:=alltrim(s);
    if (length(s)>0) and (s[1]=';') then continue;
    if (length(s)>0) and (s[1]='[') and (s[length(s)]=']') then begin
      // A section header found
      if s[2]='*' then begin
        LastSect:=copy(s,3,length(s)-3);
        fSections.AddObject(LastSect+'=1',TStringList.Create)
      end else begin
        LastSect:=copy(s,2,length(s)-2);
        fSections.AddObject(LastSect+'=0',TStringList.Create);
      end;
    end else begin // A key or list element found
      if fSections.Values[LastSect]='0' then begin
                       // A key found. If a key has no =value then it is
                       // considered as boolean with the value of TRUE (1).
        if pos('=',s)=0 then s:=s+'=1';
        TStringList(fSections.Objects[fSections.IndexOfName(LastSect)]).Add(s);
      end;
      if fSections.Values[LastSect]='1' then
                       // A list element found.
        TStringList(fSections.Objects[fSections.IndexOfName(LastSect)]).Add(s);
//        fKeys[length(fKeys)-1].Add(s);
    end;
  until iSource.Position=iSource.Size;
  for i:=0 to fSections.Count-1 do begin
    if fSections.ValueFromIndex[i]='1' then begin
      j:=TStringList(fSections.Objects[i]).Count-1;
      while (j>=0) and (TStringList(fSections.Objects[i])[j]='') do begin
        TStringList(fSections.Objects[i]).Delete(j);
        dec(j);
      end;
    end;
  end;
end;

destructor TIniFile.Destroy;
var i:integer;
begin
  UpdateFile;
  if fSections<>nil then begin
    for i:=fSections.Count-1 downto 0 do
      TStringList(fSections.Objects[i]).Free;
    FreeAndNil(fSections);
  end;
  inherited ;
end;

procedure TIniFile.UpdateFile;
var i,j:integer;t:text;
begin
  if fFileName>'' then begin
    assign(t,fFileName);
    rewrite(t);
    for i:=0 to fSections.Count-1 do begin
      if fSections.ValueFromIndex[i]='0' then
        writeln(t,'['+fSections.Names[i]+']')
      else
        writeln(t,'[*'+fSections.Names[i]+']');
      for j:=0 to TStringList(fSections.Objects[i]).Count-1 do
        writeln(t,TStringList(fSections.Objects[i])[j]);
      writeln(t);
    end;
    close(t);
  end;
end;

{ Reading values - Normal section}

function TIniFile.ReadString(Section, Ident, Default: string;Section2:string=''): string;
var s:String;
begin
  Result := Default;
  s:=fSections.Values[Section];
  if s='0' then begin
    s:=TStringList(fSections.Objects[fSections.IndexOfName(Section)]).Values[Ident];
    if s<>'' then 
      Result:=s 
    else if Section2>'' then begin
      s:=TStringList(fSections.Objects[fSections.IndexOfName(Section2)]).Values[Ident];
      if s<>'' then Result:=s
    end;   
  end;
  if (length(Result)>2) and (Result[1]='"') and (Result[length(Result)]='"') then
    Result:=copy(Result,2,length(Result)-2);
end;

function TIniFile.ReadInteger(Section, Ident: string; Default: integer;Section2:string=''): integer;
var
  s: string;
begin
  Result:=Default;
  s := ReadString(Section, Ident, '', Section2);
  if s > '' then begin
    if s[1]<>'$' then
      Result:=strtoint(s)
    else
      Result:=HexToInt(copy(s,2,length(s)-1));
  end;
end;

function TIniFile.ReadUInt32(Section,Ident:string; Default:UInt32;
  Section2:string):UInt32;
var
  s: string;
begin
  Result:=Default;
  s := ReadString(Section, Ident, '', Section2);
  if s > '' then begin
    if s[1]<>'$' then
      Result:=StrToUInt(s)
    else
      Result:=HexToInt(copy(s,2,length(s)-1));
  end;
end;

function TIniFile.ReadFloat(Section, Ident: string; Default: extended;Section2:string=''):extended;
var
  s: string;
begin
  Result:=Default;
  s := ReadString(Section, Ident, '', Section2);
  if s > '' then begin
    Result:=strtofloat(StringReplace(s,'.',FS.DecimalSeparator,[rfReplaceAll]),FS);
  end;
end;

function TIniFile.ReadBool(Section, Ident: string; Default: Boolean;Section2:string=''): Boolean;
var
  s: string;
begin
  Result := Default;
  s := ReadString(Section, Ident, '', Section2);
  if s > '' then
    if (UpperCase(s)='TRUE') or (UpperCase(s)='YES') or (s='1') then 
      Result:=true
    else 
      Result:=false;
end;

procedure TIniFile.ReadRGB(section,ident:string;defr,defg,defb:word;var r,g,b:integer;Section2:string='');
const Istr=Fstr+'TIniFile.ReadRGB';
var s:string;
begin
  s:=ReadString(Section,Ident,inttohex(defr,2)+inttohex(defg,2)+inttohex(defb,2),Section2);
  if length(s)<>6 then begin
    Log.LogWarning('Invalid RGB value! (Section: '+Section+', Key: '+ident+', Value: '+s+')',Istr);
    r:=defr;
    g:=defg;
    b:=defb;
  end else begin
    r:=HexToInt(copy(s,1,2));
    g:=HexToInt(copy(s,3,2));
    b:=HexToInt(copy(s,5,2));
  end;
end;

procedure TIniFile.ReadCoords(section:string;var x1,y1,x2,y2:word;Section2:string='');
const Istr=Fstr+'TIniFile.ReadCoords';
var x,wi:integer;s:String;
begin
  x:=ReadInteger(section,'X',0,Section2);
  wi:=ReadInteger(section,'Width',640,Section2);
  s:=UpperCase(ReadString(section,'AlignX','Left',Section2));

  if s='CENTER' then x1:=x-(wi shr 1)
  else if s='RIGHT' then x1:=x-wi+1
  else if s='LEFT' then x1:=x
  else Log.LogWarning('Invalid ALIGNX value ('+s+'), using LEFT instead.',Istr);
  x2:=x1+wi-1;

  x:=ReadInteger(section,'Y',0,Section2);
  wi:=ReadInteger(section,'Height',640,Section2);
  s:=UpperCase(ReadString(section,'AlignY','Top',Section2));

  if s='CENTER' then y1:=x-(wi shr 1)
  else if s='BOTTOM' then y1:=x-wi+1
  else if s='TOP' then y1:=x
  else Log.LogWarning('Invalid ALIGNY value ('+s+'), using TOP instead.',Istr);
  y2:=y1+wi-1;
end;

procedure TIniFile.ReadRect(section,ident:string;var x1,y1,x2,y2:integer;Section2:string='');
var s:string;
begin
  x1:=0;y1:=0;x2:=0;y2:=0;
  s:=ReadString(section,ident,'',Section2);
  if pos(',',s)>0 then begin
    try
      x1:=strtoint(copy(s,1,pos(',',s)-1));
    except
      on exception do exit;
    end;
    delete(s,1,pos(',',s));
  end else exit;
  if pos(',',s)>0 then begin
    try
      y1:=strtoint(copy(s,1,pos(',',s)-1));
    except
      on exception do exit;
    end;
    delete(s,1,pos(',',s));
  end else exit;
  if pos(',',s)>0 then begin
    try
      x2:=strtoint(copy(s,1,pos(',',s)-1));
    except
      on exception do exit;
    end;
    delete(s,1,pos(',',s));
  end else exit;
  try
    y2:=strtoint(s);
  except
    on exception do ;
  end;
end;

{ Writing values - Normal section }

procedure TIniFile.WriteString(Section, Ident, Value: String);
// WriteString is only allowed for sections where FType=stNormal
const Istr=Fstr+'TIniFile.WriteString';
var s,k:integer;
begin
  if (Section > '') and (Ident > '') then
    if Value > '' then begin
      s:=fSections.IndexOfName(Section);
      if s=-1 then begin  // Need to create section and key
        fSections.AddObject(Section+'=0',TStringList.Create);
        TStringList(fSections.Objects[fSections.IndexOfName(Section)]).Add(Ident+'='+Value);
      end else
        if fSections.ValueFromIndex[s]='0' then begin
          k:=TStringList(fSections.Objects[s]).IndexOfName(Ident);
          if k=-1 then TStringList(fSections.Objects[s]).Add(Ident+'='+Value)
                  else TStringList(fSections.Objects[s])[k]:=Ident+'='+Value;
        end else begin
          Log.LogWarning('WriteString is not allowed on List type Sections!',Istr);
          Log.LogWarning('Section: '+Section+', Key: '+Ident+', Value: '+Value,Istr);
        end;
    end else begin
      s:=fSections.IndexOfName(Section);
      if s>-1 then begin
        k:=TStringList(fSections.Objects[s]).IndexOfName(Ident);
        if k>-1 then TStringList(fSections.Objects[s]).Delete(k);
      end;
    end;

  UpdateFile;
end;

procedure TIniFile.WriteInteger(Section, Ident: string; Value: integer);
begin
  WriteString(Section, Ident, inttostr(Value));
end;

procedure TIniFile.WriteUInt32(Section,Ident:string; Value:UInt32);
begin
  WriteString(Section, Ident, inttostr(Value));
end;

procedure TIniFile.WriteFloat(Section, Ident: string; Value: extended);
begin
  WriteString(Section, Ident, StringReplace(floattostr(Value,FS),FS.DecimalSeparator,'.',[rfReplaceAll]));
end;

procedure TIniFile.WriteBool(Section, Ident: string; Value: Boolean);
begin
  if Value then WriteString(Section, Ident, 'Yes')
           else WriteString(Section, Ident, 'No');
end;

procedure TIniFile.WriteRGB(section,ident:string;r,g,b:integer);
begin
  WriteString(section,ident,inttohex(r,2)+inttohex(g,2)+inttohex(b,2));
end;

{ Working with LIST type sections }

function TIniFile.ReadListItem(Section:string;index:integer):string;
const Istr=Fstr+'TIniFile.ReadListItem';
var i:integer;
begin
  ReadListItem:='';
  i:=fSections.IndexOfName(Section);
  if i>-1 then
    if fSections.ValueFromIndex[i]='1' then begin
      if (index>-1) and (index<TStringList(fSections.Objects[i]).Count) then
        Result:=TStringList(fSections.Objects[i])[index]
      else
        Log.LogWarning('Index is out of range! ('+inttostr(index)+')',Istr)
    end else begin
      Log.LogWarning('ReadListItem is not allowed on stNormal type Sections!',Istr);
      Log.LogWarning('Section: '+Section+' Index: '+inttostr(index),Istr);
    end
  else
    Log.LogWarning('Section not found! ('+Section+')',Istr);
end;

procedure TIniFile.WriteListItem(Section,Value:string);
const Istr=Fstr+'TIniFile.WriteListItem';
var s:integer;
begin
  if (Section > '') and (Value > '') then begin
    s:=fSections.IndexOfName(Section);
    if s=-1 then begin  // Need to create section
      fSections.AddObject(Section+'=1',TStringList.Create);
      TStringList(fSections.Objects[fSections.IndexOfName(Section)]).Add(Value);
    end else
      if fSections.ValueFromIndex[s]='1' then
        TStringList(fSections.Objects[fSections.IndexOfName(Section)]).Add(Value)
      else begin
        Log.LogWarning('WriteListItem is not allowed on Normal type Sections!',Istr);
        Log.LogWarning('Section: '+Section+', Item: '+Value,Istr);
      end;
  end else begin
    Log.LogWarning('Section not found!',Istr);
    Log.LogWarning('Section: '+Section+', Item: '+Value,Istr);
  end;
end;

procedure TIniFile.DeleteListItem(Section,Value:string);
const Istr=Fstr+'TIniFile.DeleteListItem';
var s,k:integer;atm:TStringList;
begin
  s:=fSections.IndexOfName(Section);
  if s>-1 then begin
    if fSections.ValueFromIndex[s]='1' then begin
      atm:=TStringList(fSections.Objects[fSections.IndexOfName(Section)]);
      k:=atm.IndexOf(Value);
      if k>-1 then
        atm.Delete(k)
      else begin
        Log.LogWarning('Item not found!',Istr);
        Log.LogWarning('Section: '+Section+', Item: '+Value,Istr);
      end;
    end else begin
      Log.LogWarning('DeleteListItem is not allowed on Normal type Sections!',Istr);
      Log.LogWarning('Section: '+Section+', Item: '+Value,Istr);
    end;
  end else begin
    Log.LogWarning('Section not found!',Istr);
    Log.LogWarning('Section: '+Section+', Item: '+Value,Istr);
  end;
end;

procedure TIniFile.DeleteListItem(Section:string;index:integer);
const Istr=Fstr+'TIniFile.DeleteListItem';
var s:integer;
begin
  s:=fSections.IndexOfName(Section);
  if s>-1 then
    if fSections.ValueFromIndex[s]='1' then
      if (index>-1) and (index<TStringList(fSections.Objects[s]).Count) then
        TStringList(fSections.Objects[s]).Delete(index)
      else
        Log.LogWarning('Index out of range! (Section: '+Section+', Index: '+inttostr(index)+')',Istr)
    else begin
      Log.LogWarning('DeleteListItem is not allowed on Normal type Sections!',Istr);
      Log.LogWarning('Section: '+Section+', Index: '+inttostr(index),Istr);
    end
  else
    Log.LogWarning('Section not found! ('+section+')',Istr);
end;

function TIniFile.ListItemCount(Section:string):integer;
const Istr=Fstr+'TIniFile.ListItemCount';
var s:integer;
begin
  ListItemCount:=0;
  s:=fSections.IndexOfName(Section);
  if s>-1 then
    if fSections.ValueFromIndex[s]='1' then
      Result:=TStringList(fSections.Objects[s]).Count
    else
      Log.LogWarning('ListItemCount is only allowed on List type Sections! ('+section+')',Istr)
  else
    Log.LogWarning('Section not found! ('+section+')',Istr);
end;

{ Misc. }

function TIniFile.ValueExists(Section, Ident: string): Boolean;
begin
  Result:=( ReadString(Section,Ident,'')<>'' );
end;

procedure TIniFile.DeleteSection(Section: string);
var s:integer;
begin
  s:=fSections.IndexOfName(Section);
  if s>-1 then begin
    TStringList(fSections.Objects[s]).Free;
    fSections.Delete(s);
    UpdateFile;
  end;
end;

procedure TIniFile.DeleteKey(Section, Ident: String);
begin
  WriteString(Section, Ident, '');
end;

function TIniFile.GetSectionType(aSection: string): integer;
//const Istr=Fstr+'TIniFile.GetSectionType';
var i:integer;
begin
  i:=fSections.IndexOfName(aSection);
  if i>-1 then
    i:=ord(fSections.ValueFromIndex[i][1])-48;
  Result:=i;
end;

procedure TIniFile.CopySection(aSource,aTarget: string);
var i:integer;atm:TStringList;
begin
  i:=fSections.IndexOfName(UpperCase(aSource));
  if i>-1 then begin
    atm:=TStringList.Create;
    atm.AddStrings(TStringList(fSections.Objects[i]));
    fSections.AddObject(aTarget+'='+fSections.ValueFromIndex[i],atm);
  end;
end;

procedure TIniFile.LogContent;
const Istr=Fstr+'TIniFile.LogContent';
var i,j:integer;
begin
  Log.LogDebug('------------------',Istr);
  Log.LogDebug('INI file content log start ('+fFilename+'):',Istr);
  for i:=0 to fSections.Count-1 do begin
    if fSections.ValueFromIndex[i]='0' then
      Log.LogDebug('['+fSections.Names[i]+']',Istr)
    else
      Log.LogDebug('[*'+fSections.Names[i]+']',Istr);
    for j:=0 to TStringList(fSections.Objects[i]).Count-1 do
      Log.LogDebug(TStringList(fSections.Objects[i])[j],Istr);
    Log.LogDebug('',Istr);
  end;
  Log.LogDebug('------------------',Istr);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.


