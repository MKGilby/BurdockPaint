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

// This is a stripped version of the original BDPColorCluster!

unit BDPColorCluster;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fgl;

type

  { TColorCluster }

  TColorCluster=class
    // Create the color cluster from stream (fileformats.txt - T-block)
    constructor CreateFromStream(iStream:TStream);

    // Save color cluster to a standalone file. (fileformats.txt - T-block)
    procedure SaveToFile(pFilename:string);

    // Save color cluster to the specified stream. (fileformats.txt - T-block)
    procedure SaveToStream(pStream:TStream);

    // Load color cluster from a standalone file. (fileformats.txt - T-block)
    procedure LoadFromFile(pFilename:string);

    // Load color cluster from the specified stream. (fileformats.txt - T-block)
    procedure LoadFromStream(pStream:TStream);
  private
    fColor1,fColor2:uint32;
    fR1,fG1,fB1,fA1,fR2,fG2,fB2,fA2:integer;
    fReversed,fPingpong:boolean;
    procedure fSetColor1(value:uint32);
    procedure fSetColor2(value:uint32);
    procedure LoadFromStreamV1(pStream:TStream);
    procedure LoadFromStreamV2(pStream:TStream);
  public
    property Color1:uint32 read fColor1 write fSetColor1;
    property Color2:uint32 read fColor2 write fSetColor2;
    property Reversed:boolean read fReversed write fReversed;
    property PingPong:boolean read fPingpong write fPingpong;
  end;

  { TColorClusters }

  TColorClusters=class(TFPGObjectList<TColorCluster>)
    // Creates the list from stream. (fileformats.txt - L-block)
    constructor CreateFromStream(iStream:TStream);
    procedure SaveToFile(pFilename:string);
    procedure SaveToStream(pStream:TStream);
    procedure LoadFromFile(pFilename:string);
    procedure LoadFromStream(pStream:TStream);
  private
    fActiveIndex:integer;
    procedure LoadFromStreamV1(pStream:TStream);
    procedure LoadFromStreamV2(pStream:TStream);
  end;

implementation

uses BDPShared, Logger;

const
  COLORCLUSTERID=$54;
  COLORCLUSTERSID=$4C;

{ TColorCluster }

constructor TColorCluster.CreateFromStream(iStream:TStream);
begin
  LoadFromStream(iStream);
end;

procedure TColorCluster.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  SaveToStream(Xs);
  FreeAndNil(Xs);
end;

procedure TColorCluster.SaveToStream(pStream:TStream);
var i:integer;curr:int64;flags:byte;
begin
  i:=COLORCLUSTERID;
  pStream.Write(i,1);
  curr:=pStream.Position;
  i:=0;
  pStream.Write(i,4);
  i:=2;
  pStream.Write(i,1);  // Version
  pStream.Write(fColor1,4);
  pStream.Write(fColor2,4);
  flags:=0;
  if fReversed then flags:=flags or 1;
  if fPingpong then flags:=flags or 2;
  pStream.Write(flags,1);
  i:=pStream.Position-curr-4;
  pStream.Position:=curr;
  pStream.write(i,4);
  pStream.Position:=pStream.Position+i;
end;

procedure TColorCluster.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead or fmShareDenyNone);
  SaveToStream(Xs);
  FreeAndNil(Xs);
end;

procedure TColorCluster.LoadFromStream(pStream:TStream);
var size,curr:int64;b:byte;
begin
  b:=0;
  pStream.Read(b,1);
  if b<>COLORCLUSTERID then raise Exception.Create(Format('ID is not for color cluster data! (%.2x)',[b]));
  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;
  pStream.Read(b,1);
  if b=1 then LoadFromStreamV1(pStream)
  else if b=2 then LoadFromStreamV2(pStream)
  else raise Exception.Create(Format('Unknow color cluster data version! (%d)',[b]));
  pStream.Position:=curr+size;
end;

procedure TColorCluster.fSetColor1(value:uint32);
begin
  if fColor1<>value then begin
    fColor1:=value;
    fA1:=(fColor1 and $FF000000)>>24;
    fR1:=(fColor1 and $FF0000)>>16;
    fG1:=(fColor1 and $FF00)>>8;
    fB1:=(fColor1 and $FF);
  end;
end;

procedure TColorCluster.fSetColor2(value:uint32);
begin
  if fColor2<>value then begin
    fColor2:=value;
    fA2:=(fColor2 and $FF000000)>>24;
    fR2:=(fColor2 and $FF0000)>>16;
    fG2:=(fColor2 and $FF00)>>8;
    fB2:=(fColor2 and $FF);
  end;
end;

procedure TColorCluster.LoadFromStreamV1(pStream:TStream);
var flags:byte;st,en:word;
begin
  if Assigned(GlobalV1Palette) then begin
    st:=0;en:=0;
    pStream.Read(st,2);
    pStream.Read(en,2);
    Color1:=GlobalV1Palette.Colors[st];
    Color2:=GlobalV1Palette.Colors[en];
  end else begin
    // Skip color indices
    Log.LogWarning('V1 color cluster to load. Colors will be reset to black to white!');
    pStream.Position:=pStream.Position+4;
    Color1:=$FF000000;
    Color2:=$FFFFFFFF;
  end;
  flags:=0;
  pStream.Read(flags,1);
  fReversed:=(flags and 1)<>0;
  fPingpong:=(flags and 2)<>0;
end;

procedure TColorCluster.LoadFromStreamV2(pStream:TStream);
var tmp:uint32;flags:byte;
begin
  pStream.Read(tmp,4);
  fSetColor1(tmp);
  pStream.Read(tmp,4);
  fSetColor2(tmp);
  flags:=0;
  pStream.Read(flags,1);
  fReversed:=(flags and 1)<>0;
  fPingpong:=(flags and 2)<>0;
end;

{ TColorClusters }

constructor TColorClusters.CreateFromStream(iStream:TStream);
begin
  inherited Create;
  LoadFromStream(iStream);
end;

procedure TColorClusters.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  SaveToStream(Xs);
  FreeAndNil(Xs);
end;

procedure TColorClusters.SaveToStream(pStream:TStream);
var i:integer;curr:int64;
begin
  i:=COLORCLUSTERSID;
  pStream.Write(i,1);
  curr:=pStream.Position;
  i:=0;
  pStream.Write(i,4);
  i:=2;
  pStream.Write(i,1);  // Version
  i:=Self.Count;
  pStream.Write(i,1);
  pStream.Write(fActiveIndex,1);
  for i:=0 to Count-1 do Items[i].SaveToStream(pStream);
  i:=pStream.Position-curr-4;
  pStream.Position:=curr;
  pStream.write(i,4);
  pStream.Position:=pStream.Position+i;
end;

procedure TColorClusters.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead or fmShareDenyNone);
  SaveToStream(Xs);
  FreeAndNil(Xs);
end;

procedure TColorClusters.LoadFromStream(pStream:TStream);
var size,curr:int64;b:byte;
begin
  b:=0;
  pStream.Read(b,1);
  if b<>COLORCLUSTERSID then raise Exception.Create(Format('ID is not for color clusters data! (%.2x)',[b]));
  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;
  pStream.Read(b,1);
  if b=1 then LoadFromStreamV1(pStream)
  else if b=2 then LoadFromStreamV2(pStream)
  else raise Exception.Create(Format('Unknow color clusters data version! (%d)',[b]));
  pStream.Position:=curr+size;
end;

procedure TColorClusters.LoadFromStreamV1(pStream:TStream);
var count:integer;tmp:TColorCluster;
begin
  count:=0;
  pStream.Read(count,1);
  Clear;
  while count>0 do begin
    tmp:=TColorCluster.CreateFromStream(pStream);
    Add(tmp);
    dec(count);
  end;
end;

procedure TColorClusters.LoadFromStreamV2(pStream:TStream);
var count:integer;tmp:TColorCluster;
begin
  count:=0;
  pStream.Read(count,1);
  fActiveIndex:=0;
  pStream.Read(fActiveIndex,1);
  Clear;
  while count>0 do begin
    tmp:=TColorCluster.CreateFromStream(pStream);
    Add(tmp);
    dec(count);
  end;
end;

end.

