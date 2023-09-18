{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of BurdockPaint.

  BurdockPaint is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  BurdockPaint is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  BurdockPaint. If not, see <https://www.gnu.org/licenses/>.
}

unit BDPPalette;

{$mode Delphi}

interface

uses Classes, SysUtils{, BDPColorCluster};

type

  { TBDVibroColors }

  TBDVibroColors=class
    constructor Create(iColor1,iColor2:uint32);
    function GetColor:uint32;
  private
    R1,G1,B1,A1,R2,G2,B2,A2:integer;
  end;

  { TBDPalette }

  TBDPalette=class
    // Create with MAXPALETTEENTRIES entry count. (currently 2048)
    constructor Create; overload;

    // Create with specified entry count. Cannot be more than MAXPALETTEENTRIES.
    constructor Create(iMaxEntries:integer); overload;

    // Create from stream.
    constructor CreateFromStream(iStream:TStream);

    // Destructor.
    destructor Destroy; override;

    // Save palette to a file.
    procedure SaveToFile(pFilename:string);

    // Save palette to a stream.
    procedure SaveToStream(pStream:TStream); overload;

    // Load palette from a file.
    procedure LoadFromFile(pFilename:string);

    // Identify palette file and call appropiate loader method.
    procedure LoadFromStream(pStream:TStream);

    // Load an entire .COL format palette (256 colors) from file.
    // Loading entry 0 to startindex, entry 1 to startindex+1 and so on.
    procedure LoadCOL(filename:string;startindex:integer); overload;

    // Load an entire .COL format palette (256 colors) from stream.
    // Loading entry 0 to startindex, entry 1 to startindex+1 and so on.
    procedure LoadCOL(pStream:TStream;startindex:integer); overload;

    // Copies colors from the specified palette. Omit count to copy all
    // colors from SourceStart.
    procedure CopyColorsFrom(Source:TBDPalette;SourceStart,TargetStart:integer;count:integer=-1);

    // Resizes the palette, retaining color data that fits into new size.
    procedure Resize(newSize:integer);

    // Resize the palette to fit the colors
    procedure ResizeAndCopyColorsFrom(Source:TBDPalette;start:integer=0;count:integer=-1);

{    // Creates a color ramp from the start color to the end color.
    procedure Ramp(pColorCluster:TColorCluster);}

    // Returns the closest color to the specified r,g,b values.
    // Difference is calculated weighted: r*0.3, g*0.59 and b*0.11
    function GetClosestColor(r,g,b:byte):word;
  private
    fEntries:pointer;
    fMaxEntries:integer;
    fChanged:boolean;
    function fGetColor(index:integer):uint32;
    function fGetColorA(index:integer):uint8;
    function fGetColorR(index:integer):uint8;
    function fGetColorG(index:integer):uint8;
    function fGetColorB(index:integer):uint8;
    procedure fSetColor(index:integer; value:uint32);
    procedure fSetColorA(index:integer; value:uint8);
    procedure fSetColorR(index:integer; value:uint8);
    procedure fSetColorG(index:integer; value:uint8);
    procedure fSetColorB(index:integer; value:uint8);
    // Creates a compressed stream that contains the data in V1 format.
    function CreateDataV1:TStream;
    // Creates a compressed stream that contains the data in V2 format.
    function CreateDataV2:TStream;
    // Creates a compressed stream that contains the data in V3 format.
    function CreateDataV3:TStream;
    // Load palette from stream format V1 (see below).
    procedure LoadFromStreamV1(Source:TStream);
    // Load palette from stream format V2 (see below).
    procedure LoadFromStreamV2(Source:TStream);
    // Load palette from stream format V3 (see below).
    procedure LoadFromStreamV3(Source:TStream);
    //
    function CopySmallerStream(pTarget,pSource1,pSource2:TStream):integer;
  public
    property Colors[index:integer]:uint32 read fGetColor write fSetColor; default;
    property ColorR[index:integer]:uint8 read fGetColorR write fSetColorR;
    property ColorG[index:integer]:uint8 read fGetColorG write fSetColorG;
    property ColorB[index:integer]:uint8 read fGetColorB write fSetColorB;
    property ColorA[index:integer]:uint8 read fGetColorA write fSetColorA;
    property Size:integer read fMaxEntries;
    property Changed:boolean read fChanged write fChanged;
  end;

implementation

uses BDPShared, MKStream, Logger, MyZStreamUnit, CodingUnit;

const
  COLORBLOCKID='CLR';

{ TBDVibroColors }

constructor TBDVibroColors.Create(iColor1,iColor2:uint32);
begin
  A1:=(iColor1 and $FF000000)>>24;
  R1:=(iColor1 and $FF0000)>>16;
  G1:=(iColor1 and $FF00)>>8;
  B1:=(iColor1 and $FF);
  A2:=(iColor2 and $FF000000)>>24;
  R2:=(iColor2 and $FF0000)>>16;
  G2:=(iColor2 and $FF00)>>8;
  B2:=(iColor2 and $FF);
end;

function TBDVibroColors.GetColor:uint32;
var i:integer;
begin
  i:=GetTickCount64 mod 1000;
  if i>500 then i:=1000-i;
  Result:=
    ((A1+(A2-A1)*i div 500) and $FF)<<24+
    ((R1+(R2-R1)*i div 500) and $FF)<<16+
    ((G1+(G2-G1)*i div 500) and $FF)<<8+
    ((B1+(B2-B1)*i div 500) and $FF);
end;


{ TBDPalette }

constructor TBDPalette.Create;
begin
  Create(MAXPALETTEENTRIES);
  fChanged:=false;
end;

constructor TBDPalette.Create(iMaxEntries:integer);
begin
  if iMaxEntries>MAXPALETTEENTRIES then iMaxEntries:=MAXPALETTEENTRIES;
  fMaxEntries:=iMaxEntries;
  fEntries:=getmem(fMaxEntries*4);
  fillchar(fEntries^,fMaxEntries*4,0);
  fChanged:=false;
end;

constructor TBDPalette.CreateFromStream(iStream:TStream);
begin
  LoadFromStream(iStream);
  fChanged:=false;
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

procedure TBDPalette.SaveToStream(pStream:TStream);
var Xs,Ys:TStream;
begin
  Xs:=CreateDataV1;
  Ys:=CreateDataV2;
  if Xs.Size<=Ys.Size then begin
    FreeAndNil(Ys);
  end else begin
    FreeAndNil(Xs);
    Xs:=Ys;
  end;
  Ys:=CreateDataV3;
  if Xs.Size<=Ys.Size then begin
    pStream.CopyFrom(Xs,Xs.Size);
  end else begin
    pStream.CopyFrom(Ys,Ys.Size);
  end;
  FreeAndNil(Xs);
  FreeAndNil(Ys);
end;

function TBDPalette.CreateDataV1:TStream;
var Xs,Ys:TMemoryStream;i:integer;s:string;
begin
  Result:=TMemoryStream.Create;
  s:=COLORBLOCKID+#1;
  Result.Write(s[1],4);

  Xs:=TMemoryStream.Create;
  i:=fMaxEntries;
  Xs.Write(i,2);
  Xs.Write(fEntries^,i*4);
  Xs.Position:=0;
  Ys:=TMemoryStream.Create;
  CompressStream(Xs,Ys,Xs.Size);
  i:=CopySmallerStream(Result,Xs,Ys);
  FreeAndNil(Ys);
  FreeAndNil(Xs);
  if i=2 then begin
    s[1]:=chr(ord(s[1]) or $20);
    Result.Position:=0;
    Result.Write(s[1],4);
  end;
  Result.Position:=0;
end;

function TBDPalette.CreateDataV2:TStream;
var Xs,Ys:TMemoryStream;i,j:integer;s:string;
begin
  Result:=TMemoryStream.Create;
  s:=COLORBLOCKID+#2;
  Result.Write(s[1],4);

  Xs:=TMemoryStream.Create;
  i:=fMaxEntries;
  Xs.Write(i,2);
  for j:=0 to 3 do
    for i:=0 to fMaxEntries-1 do
      Xs.Write((fEntries+i*4+j)^,1);
  Xs.Position:=0;
  Ys:=TMemoryStream.Create;
  CompressStream(Xs,Ys,Xs.Size);
  i:=CopySmallerStream(Result,Xs,Ys);
  FreeAndNil(Ys);
  FreeAndNil(Xs);
  if i=2 then begin
    s[1]:=chr(ord(s[1]) or $20);
    Result.Position:=0;
    Result.Write(s[1],4);
  end;
  Result.Position:=0;
end;

function TBDPalette.CreateDataV3:TStream;
var Xs,Ys:TMemoryStream;i,j,b:integer;s:string;
begin
  Result:=TMemoryStream.Create;
  s:=COLORBLOCKID+#3;
  Result.Write(s[1],4);

  Xs:=TMemoryStream.Create;
  i:=fMaxEntries;
  Xs.Write(i,2);
  for j:=0 to 3 do begin
    b:=0;
    for i:=0 to fMaxEntries-1 do begin
      b:=byte((fEntries+i*4+j)^)-b;
      Xs.Write(b,1);
      b:=byte((fEntries+i*4+j)^);
    end;
  end;
  Xs.Position:=0;
  Ys:=TMemoryStream.Create;
  CompressStream(Xs,Ys,Xs.Size);
  i:=CopySmallerStream(Result,Xs,Ys);
  FreeAndNil(Ys);
  FreeAndNil(Xs);
  if i=2 then begin
    s[1]:=chr(ord(s[1]) or $20);
    Result.Position:=0;
    Result.Write(s[1],4);
  end;
  Result.Position:=0;
end;

procedure TBDPalette.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=MKStreamOpener.OpenStream(pFilename);
  LoadFromStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDPalette.LoadFromStream(pStream:TStream);
var size,curr:int64;b:byte;Xs:TMemoryStream;s:string;
begin
  s:=#0#0#0#0;
  pStream.Read(s[1],4);
  if uppercase(copy(s,1,3))<>COLORBLOCKID then
    raise Exception.Create(Format('Color block id expected, got %s.',[copy(s,1,3)]));

  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;

  Xs:=TMemoryStream.Create;
  if ord(s[1]) and $20<>0 then
    UnCompressStream(pStream,Xs)
  else
    Xs.CopyFrom(pStream,Size);
  Xs.Position:=0;

  b:=ord(s[4]);
  if b=1 then LoadFromStreamV1(Xs)
  else if b=2 then LoadFromStreamV2(Xs)
  else if b=3 then LoadFromStreamV3(Xs)
  else raise Exception.Create(Format('Unknown color block version! (%d)',[b]));
  FreeAndNil(Xs);
  pStream.Position:=curr+size;
end;

procedure TBDPalette.LoadFromStreamV1(Source:TStream);
var count:integer;
begin
  count:=0;
  Source.Read(count,2);
  if count>MAXPALETTEENTRIES then begin
    Log.LogWarning(Format('Too many palette entries in file! Only the first %d entries will be loaded.',[MAXPALETTEENTRIES]));
    count:=MAXPALETTEENTRIES;
  end;
  if count<>fMaxEntries then begin
    Freemem(fEntries);
    fMaxEntries:=count;
    fEntries:=GetMem(fMaxEntries*4);
  end;
  Source.Read(fEntries^,fMaxEntries*4);
end;

procedure TBDPalette.LoadFromStreamV2(Source:TStream);
var i,j,count:integer;
begin
  count:=0;
  Source.Read(count,2);
  if count>MAXPALETTEENTRIES then begin
    Log.LogWarning(Format('Too many palette entries in file! Only the first %d entries will be loaded.',[MAXPALETTEENTRIES]));
    count:=MAXPALETTEENTRIES;
  end;
  if count<>fMaxEntries then begin
    Freemem(fEntries);
    fMaxEntries:=count;
    fEntries:=GetMem(fMaxEntries*4);
  end;
  for j:=0 to 3 do
    for i:=0 to fMaxEntries-1 do
      Source.Read((fEntries+i*4+j)^,1);
end;

procedure TBDPalette.LoadFromStreamV3(Source:TStream);
var i,j,count:integer;b,b2:byte;
begin
  count:=0;
  Source.Read(count,2);
  if count>MAXPALETTEENTRIES then begin
    Log.LogWarning(Format('Too many palette entries in file! Only the first %d entries will be loaded.',[MAXPALETTEENTRIES]));
    count:=MAXPALETTEENTRIES;
  end;
  if count<>fMaxEntries then begin
    Freemem(fEntries);
    fMaxEntries:=count;
    fEntries:=GetMem(fMaxEntries*4);
  end;
  b2:=0;
  for j:=0 to 3 do begin
    b:=0;
    for i:=0 to fMaxEntries-1 do begin
      Source.Read(b2,1);
      b:=(b+b2) and $ff;
      byte((fEntries+i*4+j)^):=b;
    end;
  end;
end;

procedure TBDPalette.LoadCOL(filename:string; startindex:integer);
var Xs:TStream;
begin
  Xs:=MKStreamOpener.OpenStream(filename);
  LoadCOL(Xs,startindex);
  FreeAndNil(Xs);
end;

procedure TBDPalette.LoadCOL(pStream:TStream; startindex:integer);
var buf:array[0..767] of byte;i:integer;p:pointer;
begin
  if (startindex+256<=fMaxEntries) then begin
    buf[0]:=0;
    pStream.Read(buf[0],768);
    for i:=0 to 767 do buf[i]:=buf[i]<<2;
    p:=fEntries+startindex*4;
    for i:=0 to 255 do begin
      byte(p^):=buf[i*3+2];
      inc(p);
      byte(p^):=buf[i*3+1];
      inc(p);
      byte(p^):=buf[i*3];
      inc(p);
      byte(p^):=255;
      inc(p);
    end;
  end;
  fChanged:=true;
end;

procedure TBDPalette.CopyColorsFrom(Source:TBDPalette;SourceStart,TargetStart:integer; count:integer);
var i,c:Integer;
begin
  if (SourceStart>=0) and (SourceStart<Source.Size) and
     (TargetStart>=0) and (TargetStart<Size) then begin
    c:=count;
    if (c=-1) or (SourceStart+c>Source.Size) then c:=Source.Size-SourceStart;
    if (TargetStart+c>Size) then c:=Size-TargetStart;
    for i:=0 to c-1 do Colors[TargetStart+i]:=Source.Colors[SourceStart+i];
    fChanged:=true;
  end;
end;

procedure TBDPalette.Resize(newSize:integer);
var p:pointer;i:integer;
begin
  p:=fEntries;
  getmem(fEntries,newSize*4);
  if fMaxEntries>newSize then
    move(p^,fEntries^,newSize*4)
  else begin
    move(p^,fEntries^,fMaxEntries*4);
    for i:=fMaxEntries to newSize-1 do
      uint32((fEntries+i*4)^):=0;
  end;

  fMaxEntries:=newSize;
  Freemem(p);
  fChanged:=true;
end;

procedure TBDPalette.ResizeAndCopyColorsFrom(Source:TBDPalette; start:integer; count:integer);
var i,c:Integer;
begin
  if (start>=0) and (start<Source.Size) then begin
    c:=count;
    if (c=-1) or (start+c>Source.Size) then c:=Source.Size-start;
    if fMaxEntries<>c then begin
      Freemem(fEntries);
      fMaxEntries:=c;
      fEntries:=Getmem(fMaxEntries*4);
    end;
    for i:=0 to fMaxEntries do Colors[i]:=Source.Colors[start+i];
    fChanged:=true;
  end;  // Start is out of range so nothing to copy.
end;

function TBDPalette.GetClosestColor(r, g, b: byte): word;
var i,min,diff:integer;
begin
  Result:=0;min:=10000;
  for i:=0 to Size-1 do begin
    diff:=abs(fGetColorR(i)-r)*30+abs(fGetColorG(i)-g)*59+abs(fGetColorB(i)-b)*11;
    if diff<min then begin
      min:=diff;
      Result:=i;
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
  if (index>=0) and (index<fMaxEntries) then begin
    uint32((fEntries+index*4)^):=value;
    fChanged:=true;
  end;
end;

procedure TBDPalette.fSetColorA(index:integer; value:uint8);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint8((fEntries+index*4+3)^):=value;
    fChanged:=true;
  end;
end;

procedure TBDPalette.fSetColorR(index:integer; value:uint8);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint8((fEntries+index*4+2)^):=value;
    fChanged:=true;
  end;
end;

procedure TBDPalette.fSetColorG(index:integer; value:uint8);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint8((fEntries+index*4+1)^):=value;
    fChanged:=true;
  end;
end;

procedure TBDPalette.fSetColorB(index:integer; value:uint8);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint8((fEntries+index*4)^):=value;
    fChanged:=true;
  end;
end;

function TBDPalette.CopySmallerStream(pTarget, pSource1, pSource2: TStream): integer;
var i:integer;
begin
  if pSource1.Size<=pSource2.Size then begin
    Result:=1;
    pSource1.Position:=0;
    i:=pSource1.Size;
    pTarget.Write(i,4);
    pTarget.CopyFrom(pSource1,pSource1.Size);
  end else begin
    Result:=2;
    pSource2.Position:=0;
    i:=pSource2.Size;
    pTarget.Write(i,4);
    pTarget.CopyFrom(pSource2,pSource2.Size);
  end;
end;

end.

