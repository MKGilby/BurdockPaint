unit CodeGenerator;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, DialogTree;

type

  { TCodeGenerator }

  TCodeGenerator=class
    constructor Create(iDialog:TDialog);
    destructor Destroy; override;
    procedure Save(pFilename:String);
  private
    fDialog:TDialog;
//    fName:string;
    fUses:TStringList;
    fOnClicks:TStringList;
    fOnClickProcs:TStringList;
    fPrivates:TStringList;
    fCreateVar:TStringList;
    fCreateCode:TStringList;
    fRedrawCode:TStringList;
    fSaveCode:TStringList;
    fShowCode:TStringList;
  public
    property UsesList:TStringList read fUses;
    property OnClicks:TStringList read fOnClicks;
    property OnClickProcs:TStringList read fOnClickProcs;
    property Privates:TStringList read fPrivates;
    property CreateVar:TStringList read fCreateVar;
    property CreateCode:TStringList read fCreateCode;
    property RedrawCode:TStringList read fRedrawCode;
    property SaveCode:TStringList read fSaveCode;
    property ShowCode:TStringList read fShowCode;
  end;

implementation

uses MKToolbox;

{ TCodeGenerator }

constructor TCodeGenerator.Create(iDialog:TDialog);
begin
  fDialog:=iDialog;
  fUses:=TStringList.Create;
  fUses.Sorted:=true;
  fUses.Duplicates:=dupIgnore;
  fOnClicks:=TStringList.Create;
  fOnClickProcs:=TStringList.Create;
  fPrivates:=TStringList.Create;
  fPrivates.Sorted:=true;
  fPrivates.Duplicates:=dupIgnore;
  fCreateVar:=TStringList.Create;
  fCreateVar.Sorted:=true;
  fCreateVar.Duplicates:=dupIgnore;
  fCreateCode:=TStringList.Create;
  fRedrawCode:=TStringList.Create;
  fRedrawCode.Duplicates:=dupIgnore;
  fSaveCode:=TStringList.Create;
  fSaveCode.Sorted:=true;
  fSaveCode.Duplicates:=dupIgnore;
  fShowCode:=TStringList.Create;
  fShowCode.Sorted:=true;
  fShowCode.Duplicates:=dupIgnore;
end;

destructor TCodeGenerator.Destroy;
begin
  fShowCode.Free;
  fSaveCode.Free;
  fRedrawCode.Free;
  fCreateCode.Free;
  fCreateVar.Free;
  fPrivates.Free;
  fOnClickProcs.Free;
  fOnClicks.Free;
  fUses.Free;
  inherited Destroy;
end;

procedure TCodeGenerator.Save(pFilename:String);
var t,t2:textfile;i,j:integer;s:String;
begin
  {ifdef debug}
//  assignfile(t,'..\'+pFilename);
  {else}
  assignfile(t,pFilename);
  {endif}
  rewrite(t);

  try

    {ifdef debug}
//    assignfile(t2,'..\..\base_copyright_notice.txt');
    {else}
    assignfile(t2,'..\base_copyright_notice.txt');
    {endif}
    reset(t2);
    try
      while not eof(t2) do begin
        readln(t2,s);
        writeln(t,s);
      end;
    finally
      closefile(t2);
    end;
    writeln(t,'// Generated on ',datetostr(now,FS));
    writeln(t,'');
    writeln(t,'unit ',fDialog.Name,';');
    writeln(t);
    writeln(t,'{$mode delphi}');
    writeln(t);
    writeln(t,'interface');
    writeln(t);
    if fUses.Count>0 then begin
      writeln(t,'uses');
      write(t,'  SysUtils, BDPModalDialog');
      for i:=0 to fUses.Count-1 do begin
        write(t,', ');
        write(t,fUses[i]);
      end;
      writeln(t,';');
      writeln(t);
    end;
    writeln(t,'type');
    writeln(t);
    writeln(t,'  { ',fDialog.Class_Name,' }');
    writeln(t);
    writeln(t,'  ',fDialog.Class_Name,'=class(TBDModalDialog)');
    writeln(t,'    constructor Create;');
    writeln(t,'    procedure ReDraw; override;');
    if fPrivates.Count>0 then begin
      writeln(t,'  private');
      for i:=0 to fPrivates.Count-1 do
        writeln(t,'    ',fPrivates[i]);
    end;
    writeln(t,'    procedure SaveSettings;');
    writeln(t,'    procedure Show(Sender:TObject);');
    if fOnClicks.Count>0 then begin
      for i:=0 to fOnClicks.Count-1 do
        writeln(t,'    procedure ',fOnClicks[i],'Click(Sender:TObject;x,y,buttons:integer);');
    end;
    writeln(t,'  end;');
    writeln(t);
    writeln(t,'implementation');
    writeln(t);
    writeln(t,'uses BDPShared, MKMouse2;');
    writeln(t);
    writeln(t,'{ ',fDialog.Class_Name,' }');
    writeln(t);
    writeln(t,'constructor ',fDialog.Class_Name,'.Create;');
    if fCreateVar.Count>0 then begin
      write(t,'var ');
      for i:=0 to fCreateVar.Count-1 do
        write(t,fCreateVar[i]);
      writeln(t);
    end;
    writeln(t,'begin');
    if fCreateCode[fCreateCode.Count-1]='' then fCreateCode.Delete(fCreateCode.Count-1);
    for i:=0 to fCreateCode.Count-1 do writeln(t,fCreateCode[i]);
    writeln(t,'end;');
    writeln(t);
    writeln(t,'procedure ',fDialog.Class_Name,'.Redraw;');
    writeln(t,'begin');
    writeln(t,'  inherited ReDraw;');
    if fRedrawCode.Count>0 then begin
      for i:=0 to fRedrawCode.Count-1 do
        writeln(t,fRedrawCode[i]);
    end;
    writeln(t,'end;');
    writeln(t);
    writeln(t,'procedure ',fDialog.Class_Name,'.SaveSettings;');
    writeln(t,'begin');
    if fSaveCode.Count>0 then begin
      for i:=0 to fSaveCode.Count-1 do
        writeln(t,fSaveCode[i]);
    end;
    writeln(t,'end;');
    writeln(t);
    for i:=0 to fOnClicks.Count-1 do begin
      writeln(t,'procedure ',fDialog.Class_Name,'.',fOnClicks[i],'Click(Sender:TObject;x,y,buttons:integer);');
      writeln(t,'begin');
      for j:=0 to fOnClickProcs.Count-1 do
        if fOnClickProcs.Names[j]=fOnClicks[i] then writeln(t,'  ',fOnClickProcs.ValueFromIndex[j]);
      writeln(t,'end;');
      writeln(t);
    end;
    writeln(t,'procedure ',fDialog.Class_Name,'.Show(Sender:TObject);');
    writeln(t,'begin');
    for i:=0 to fShowCode.Count-1 do
      writeln(t,fShowCode[i]);
    writeln(t,'end;');
    writeln(t);
    writeln(t,'end.');

  finally
    closefile(t);
  end;

end;

end.

