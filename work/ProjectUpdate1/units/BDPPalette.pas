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

// This is a stripped version of the original BDPPalette!

unit BDPPalette;

{$mode Delphi}

interface

uses Classes, SysUtils;

type

  { TBDPalette }

  TBDPalette=class
    // Create from stream.
    constructor CreateFromStream(iStream:TStream);

    // Destructor.
    destructor Destroy; override;

    // Save palette to a file.
    procedure SaveToFile(pFilename:string);

    // Save palette to a stream.
    procedure SaveToStream(Target:TStream); overload;

    // Save palette to a stream. 3+1 byte id format.
    procedure SaveToStream2(Target:TStream); overload;

    // Load palette from a file.
    procedure LoadFromFile(pFilename:string);

    // Identify palette file and call appropiate loader method.
    procedure LoadFromStream(Source:TStream);
  private
    fEntries:pointer;
    fMaxEntries:integer;
    fChanged:boolean;
    function fGetColor(index:integer):uint32;
    // Creates a compressed stream that contains the data in V1 format.
    function CreateDataV1:TStream;
    // Creates a compressed stream that contains the data in V2 format.
    function CreateDataV2:TStream;
    // Creates a compressed stream that contains the data in V3 format.
    function CreateDataV3:TStream;
    // Creates a compressed stream that contains the data in V1 format.
    function CreateDataV1_2:TStream;
    // Creates a compressed stream that contains the data in V2 format.
    function CreateDataV2_2:TStream;
    // Creates a compressed stream that contains the data in V3 format.
    function CreateDataV3_2:TStream;
    // Load palette from stream format V1 (see below).
    procedure LoadFromStreamV1(Source:TStream);
    // Load palette from stream format V2 (see below).
    procedure LoadFromStreamV2(Source:TStream);
    // Load palette from stream format V3 (see below).
    procedure LoadFromStreamV3(Source:TStream);

    function CopySmallerStream(pTarget,pSource1,pSource2:TStream):integer;
  public
    property Colors[index:integer]:uint32 read fGetColor; default;
    property Size:integer read fMaxEntries;
  end;

implementation

uses MKStream, Logger, MyZStreamUnit, CodingUnit;

const
  PALETTEDATAID=$43;
  MAXPALETTEENTRIES=256;

  COLORBLOCKID='CLR';

  { TBDPalette }

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

procedure TBDPalette.SaveToStream(Target:TStream);
var i,curr:integer;Xs,Ys:TStream;
begin
  i:=PALETTEDATAID;
  Target.Write(i,1);
  curr:=Target.Position;
  i:=0;
  Target.Write(i,4);
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
    Target.CopyFrom(Xs,Xs.Size);
  end else begin
    Target.CopyFrom(Ys,Ys.Size);
  end;
  FreeAndNil(Xs);
  FreeAndNil(Ys);
  i:=Target.Position-curr-4;
  Target.Position:=curr;
  Target.write(i,4);
  Target.Position:=Target.Position+i;
end;

procedure TBDPalette.SaveToStream2(Target: TStream);
var Xs,Ys:TStream;
begin
  Xs:=CreateDataV1_2;
  Ys:=CreateDataV2_2;
  if Xs.Size<=Ys.Size then begin
    FreeAndNil(Ys);
  end else begin
    FreeAndNil(Xs);
    Xs:=Ys;
  end;
  Ys:=CreateDataV3_2;
  if Xs.Size<=Ys.Size then begin
    Target.CopyFrom(Xs,Xs.Size);
  end else begin
    Target.CopyFrom(Ys,Ys.Size);
  end;
  FreeAndNil(Xs);
  FreeAndNil(Ys);
end;

function TBDPalette.CreateDataV1:TStream;
var Xs:TMemoryStream;i:integer;
begin
  Result:=TMemoryStream.Create;
  Xs:=TMemoryStream.Create;
  i:=1;
  Xs.Write(i,1);
  i:=fMaxEntries;
  Xs.Write(i,2);
  Xs.Write(fEntries^,i*4);
  Xs.Position:=0;
  CompressStream(Xs,Result,Xs.Size);
  FreeAndNil(Xs);
  Result.Position:=0;
end;

function TBDPalette.CreateDataV2:TStream;
var Xs:TMemoryStream;i,j:integer;
begin
  Result:=TMemoryStream.Create;
  Xs:=TMemoryStream.Create;
  i:=2;
  Xs.Write(i,1);
  i:=fMaxEntries;
  Xs.Write(i,2);
  for j:=0 to 3 do
    for i:=0 to fMaxEntries-1 do
      Xs.Write((fEntries+i*4+j)^,1);
  Xs.Position:=0;
  CompressStream(Xs,Result,Xs.Size);
  FreeAndNil(Xs);
  Result.Position:=0;
end;

function TBDPalette.CreateDataV3:TStream;
var Xs:TMemoryStream;i,j,b:integer;
begin
  Result:=TMemoryStream.Create;
  Xs:=TMemoryStream.Create;
  i:=3;
  Xs.Write(i,1);
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
  CompressStream(Xs,Result,Xs.Size);
  FreeAndNil(Xs);
  Result.Position:=0;
end;

function TBDPalette.CreateDataV1_2:TStream;
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

function TBDPalette.CreateDataV2_2:TStream;
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

function TBDPalette.CreateDataV3_2:TStream;
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

procedure TBDPalette.LoadFromStream(Source:TStream);
var size,curr:int64;b:byte;Xs:TMemoryStream;
begin
  b:=0;
  Source.Read(b,1);
  if b<>PALETTEDATAID then raise Exception.Create(Format('ID is not for palette data! (%.2x)',[b]));
  size:=0;
  Source.Read(Size,4);
  curr:=Source.Position;
  Xs:=TMemoryStream.Create;
  UnCompressStream(Source,Xs);
  Xs.Position:=0;
  b:=0;
  Xs.Read(b,1);
  if b=1 then LoadFromStreamV1(Xs)
  else if b=2 then LoadFromStreamV2(Xs)
  else if b=3 then LoadFromStreamV3(Xs)
  else raise Exception.Create(Format('Unknown palette data version! (%d)',[b]));
  FreeAndNil(Xs);
  Source.Position:=curr+size;
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

function TBDPalette.fGetColor(index:integer):uint32;
begin
  if (index>=0) and (index<fMaxEntries) then
    Result:=uint32((fEntries+index*4)^)
  else
    Result:=$ffffffff;
end;

end.

