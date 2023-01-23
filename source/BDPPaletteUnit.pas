unit BDPPaletteUnit;

{$mode Delphi}

interface

uses Classes;

type

  { TBDPalette }

  TBDPalette=class
    // Create with MAXPALETTEENTRIES entry count. (currently 2048)
    constructor Create; overload;

    // Create with specified entry count. Cannot be more than MAXPALETTEENTRIES.
    constructor Create(iMaxEntries:integer); overload;

    // Destructor.
    destructor Destroy; override;

    // Save palette to a file.
    procedure SaveToFile(pFilename:string);

    // Save palette to a stream.
    procedure SaveToStream(Target:TStream); overload;

    // Load palette from a file.
    procedure LoadFromFile(pFilename:string);

    // Identify palette file and call appropiate loader method.
    procedure LoadFromStream(Source:TStream);

    // Load palette from stream format V1 (see below).
    procedure LoadFromStreamV1(Source:TStream);

    // Load an entire .COL format palette (256 colors) from file.
    // Loading entry 0 to startindex, entry 1 to startindex+1 and so on.
    procedure LoadCOL(filename:string;startindex:integer); overload;

    // Load an entire .COL format palette (256 colors) from stream.
    // Loading entry 0 to startindex, entry 1 to startindex+1 and so on.
    procedure LoadCOL(Source:TStream;startindex:integer); overload;
  private
    fEntries:pointer;
    fMaxEntries:integer;
    function fGetColor(index:integer):uint32;
    function fGetColorA(index:integer):uint8;
    function fGetColorB(index:integer):uint8;
    function fGetColorG(index:integer):uint8;
    function fGetColorR(index:integer):uint8;
    procedure fSetColor(index:integer; value:uint32);

  public
    property Colors[index:integer]:uint32 read fGetColor write fSetColor; default;
    property ColorR[index:integer]:uint8 read fGetColorR;
    property ColorG[index:integer]:uint8 read fGetColorG;
    property ColorB[index:integer]:uint8 read fGetColorB;
    property ColorA[index:integer]:uint8 read fGetColorA;
    property Size:integer read fMaxEntries;
  end;

implementation

uses SysUtils, BDPSharedUnit, MKStream, Logger, MyZStreamUnit;

const
  PALETTEFOURCC='BDPP';
  PALETTESAVEVERSION=1;


{ TBDPalette }

constructor TBDPalette.Create;
begin
  Create(MAXPALETTEENTRIES);
end;

constructor TBDPalette.Create(iMaxEntries:integer);
begin
  if iMaxEntries>MAXPALETTEENTRIES then iMaxEntries:=MAXPALETTEENTRIES;
  fMaxEntries:=iMaxEntries;
  fEntries:=getmem(fMaxEntries*4);
  fillchar(fEntries^,fMaxEntries*4,0);
end;

destructor TBDPalette.Destroy;
begin
  freemem(fEntries);
  inherited Destroy;
end;

procedure TBDPalette.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  SaveToStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDPalette.SaveToStream(Target:TStream);
var s:string;i,curr:integer;
begin
  s:=PALETTEFOURCC;
  Target.Write(s[1],length(s));
  curr:=Target.Position;
  i:=0;
  Target.Write(i,4);
  i:=PALETTESAVEVERSION;
  Target.Write(i,1);
  i:=fMaxEntries;
  Target.Write(i,2);
  Compress(fEntries^,Target,i*4);
  Log.Trace(Target.Position);
  log.Trace(curr);
  i:=Target.Position-curr;
  Target.Position:=curr;
  Target.write(i,4);
  Target.Position:=Target.Position+i;
//  Target.Write(fEntries^,i*4);
end;

procedure TBDPalette.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=MKStreamOpener.OpenStream(pFilename);
  LoadFromStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDPalette.LoadFromStream(Source:TStream);
var s:string;i,size,curr:integer;
begin
  s:=#0#0#0#0;
  Source.Read(s[1],length(s));
  if s<>PALETTEFOURCC then raise Exception.Create('Not a palette file!');
  size:=0;
  Source.Read(Size,4);
  curr:=Source.Position;
  i:=0;
  Source.Read(i,1);
  if i=1 then LoadFromStreamV1(Source)
  else raise Exception.Create(Format('Unknown palette file version! (%d)',[i]));
  Source.Position:=curr+Size;
end;

procedure TBDPalette.LoadFromStreamV1(Source:TStream);
var count:integer;Xs:TMemoryStream;
begin
  count:=0;
  Source.Read(count,2);
  if count>MAXPALETTEENTRIES then begin
    Log.LogWarning(Format('Too many palette entries in file! Only the first %d entries will be loaded.',[MAXPALETTEENTRIES]));
    count:=MAXPALETTEENTRIES;
  end;
  Freemem(fEntries);
  fMaxEntries:=count;
  fEntries:=GetMem(fMaxEntries*4);
  Xs:=TMemoryStream.Create;
  UnCompressStream(Source,Xs);
  Log.Trace(Source.Position);
  Xs.Position:=0;
  Xs.Read(fEntries^,fMaxEntries*4);
  FreeAndNil(Xs);
end;

procedure TBDPalette.LoadCOL(filename:string; startindex:integer);
var Xs:TStream;
begin
  Xs:=MKStreamOpener.OpenStream(filename);
  LoadCOL(Xs,startindex);
  FreeAndNil(Xs);
end;

procedure TBDPalette.LoadCOL(Source:TStream; startindex:integer);
var buf:array[0..767] of byte;i:integer;p:pointer;
begin
  if (startindex+256<fMaxEntries) then begin
    buf[0]:=0;
    Source.Read(buf[0],768);
    for i:=0 to 767 do buf[i]:=buf[i]<<2;
    p:=fEntries+startindex*4;
    for i:=0 to 255 do begin
      byte(p^):=buf[i*3+2];
      inc(p);
      byte(p^):=buf[i*3+1];
      inc(p);
      byte(p^):=buf[i*3];
      inc(p,2);
    end;
  end;
end;

function TBDPalette.fGetColor(index:integer):uint32;
begin
  if (index>=0) and (index<fMaxEntries) then
    Result:=uint32((fEntries+index*4)^)
  else
    Result:=$ffffffff;
end;

function TBDPalette.fGetColorA(index:integer):uint8;
begin
  if (index>=0) and (index<fMaxEntries) then
    Result:=uint8((fEntries+index*4+3)^)
  else
    Result:=0;
end;

function TBDPalette.fGetColorR(index:integer):uint8;
begin
  if (index>=0) and (index<fMaxEntries) then
    Result:=uint8((fEntries+index*4+2)^)
  else
    Result:=0;
end;

function TBDPalette.fGetColorG(index:integer):uint8;
begin
  if (index>=0) and (index<fMaxEntries) then
    Result:=uint8((fEntries+index*4+1)^)
  else
    Result:=0;
end;

function TBDPalette.fGetColorB(index:integer):uint8;
begin
  if (index>=0) and (index<fMaxEntries) then
    Result:=uint8((fEntries+index*4)^)
  else
    Result:=0;
end;

procedure TBDPalette.fSetColor(index:integer; value:uint32);
begin
  if (index>=0) and (index<fMaxEntries) then
    uint32((fEntries+index*4)^):=value;
end;

end.

{
  Version 1
  ---------
    Name       Size   Content / Description
    FourCC      4     'PALE'
    Size        4     Size of Version+EntryCount+Colors
    Version     1     1
    EntryCount  2     Count of entries
    Colors      ???   ZLib compressed color data
}




