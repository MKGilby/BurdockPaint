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

unit BDPColorCluster;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fgl, vcc2_VisibleControl, ARGBImageUnit, Font2Unit;

type

  { TColorCluster }

  TColorCluster=class
    // Create color cluster.
    constructor Create(iColor1,iColor2:uint32);

    // Create the color cluster from stream (see fileformats.txt - CCS-block)
    constructor CreateFromStream(iStream:TStream);

    // Gives back the colorindex in the cluster at pValue on a scale to 0..pInterval
    // Example: if you want to draw the color cluster on a control with a width
    //          of 240 pixels, you should use GetIndexAt(x,240) as each vertical
    //          line of the control.
    function GetColorAt(pValue,pInterval:integer):uint32;

    // Gives back the colorindex in the cluster at pValue on a scale to 0..pInterval
    // randomly modifying it by max +/-(pInterval*pDitherStrength/256)
    function GetColorAtDithered(pValue,pInterval,pDitherStrength:integer):uint32;

    // Save color cluster to the specified stream. (see fileformats.txt - CCS-block)
    procedure SaveToStream(pStream:TStream);

    // Load color cluster from the specified stream. (see fileformats.txt - CCS-block)
    procedure LoadFromStream(pStream:TStream);
  private
    fColor1,fColor2:uint32;
    fR1,fG1,fB1,fA1,fR2,fG2,fB2,fA2:integer;
    fReversed,fPingpong:boolean;
    procedure fSetColor1(value:uint32);
    procedure fSetColor2(value:uint32);
  public
    property Color1:uint32 read fColor1 write fSetColor1;
    property Color2:uint32 read fColor2 write fSetColor2;
    property Reversed:boolean read fReversed write fReversed;
    property PingPong:boolean read fPingpong write fPingpong;
  end;

  { TColorClusters }

  TColorClusters=class(TFPGObjectList<TColorCluster>)
    // Create the list with one default color cluster element.
    constructor Create;

    // Create the list from stream. (see fileformats.txt - CCS-block)
    constructor CreateFromStream(iStream:TStream);

    // Save the list to file. (see fileformats.txt - CCS-block)
    procedure SaveToFile(pFilename:string);

    // Save the list to stream. (see fileformats.txt - CCS-block)
    procedure SaveToStream(pStream:TStream);

    // Load the list from file. (see fileformats.txt - CCS-block)
    procedure LoadFromFile(pFilename:string);

    // Load the list from stream. (see fileformats.txt - CCS-block)
    procedure LoadFromStream(pStream:TStream);
  private
    fActiveIndex:integer;
    function fGetActiveColorCluster:TColorCluster;
    procedure fSetActiveIndex(value:integer);
  public
    property ActiveColorCluster:TColorCluster read fGetActiveColorCluster;
    property ActiveIndex:integer read fActiveIndex write fSetActiveIndex;
  end;

  { TBDSimpleColorCluster }

  TBDSimpleColorCluster=class(TVisibleControl)
    constructor Create(iLeft,iTop:integer;iColorCluster:TColorCluster);
  protected
    procedure ReDraw; override;
  private
    fColorCluster:TColorCluster;
    fColorsWidth:integer;
    function fGetColorsWidth:integer;
  public
    property ColorsWidth:integer read fGetColorsWidth;
  end;

implementation

uses BDPShared, Logger;

const
  COLORCLUSTERSBLOCKID='CCS';

{ TColorCluster }

constructor TColorCluster.Create(iColor1,iColor2:uint32);
begin
  Color1:=iColor1;
  Color2:=iColor2;
  fReversed:=false;
  fPingpong:=false;
end;

constructor TColorCluster.CreateFromStream(iStream:TStream);
begin
  LoadFromStream(iStream);
end;

function TColorCluster.GetColorAt(pValue, pInterval: integer): uint32;
begin
  if fPingpong then begin
    pValue:=pValue*2;
    if pValue>pInterval-1 then begin
      pValue:=(pInterval*2)-pValue;
    end;
  end;
  if pValue<0 then pValue:=0
  else if pValue>=pInterval then pValue:=pInterval-1;
  if not fReversed then begin
    Result:=
      ((fA1+(fA2-fA1)*pValue div (pInterval-1)) and $ff) << 24+
      ((fR1+(fR2-fR1)*pValue div (pInterval-1)) and $ff) << 16+
      ((fG1+(fG2-fG1)*pValue div (pInterval-1)) and $ff) << 8+
      ((fB1+(fB2-fB1)*pValue div (pInterval-1)) and $ff)
  end else begin
    Result:=
      ((fA2+(fA1-fA2)*pValue div (pInterval-1)) and $ff) << 24+
      ((fR2+(fR1-fR2)*pValue div (pInterval-1)) and $ff) << 16+
      ((fG2+(fG1-fG2)*pValue div (pInterval-1)) and $ff) << 8+
      ((fB2+(fB1-fB2)*pValue div (pInterval-1)) and $ff)
  end;
end;

function TColorCluster.GetColorAtDithered(pValue, pInterval,
  pDitherStrength: integer): uint32;
var dith:integer;
begin
  dith:=pInterval*pDitherStrength div 256;
  if dith>0 then pValue+=random(2*dith)-dith;
  Result:=GetColorAt(pValue,pInterval);
end;

procedure TColorCluster.SaveToStream(pStream:TStream);
var flags:byte;
begin
  pStream.Write(fColor1,4);
  pStream.Write(fColor2,4);
  flags:=0;
  if fReversed then flags:=flags or 1;
  if fPingpong then flags:=flags or 2;
  pStream.Write(flags,1);
end;

procedure TColorCluster.LoadFromStream(pStream:TStream);
var tmp:uint32;flags:byte;
begin
  tmp:=0;
  pStream.Read(tmp,4);
  fSetColor1(tmp);
  pStream.Read(tmp,4);
  fSetColor2(tmp);
  flags:=0;
  pStream.Read(flags,1);
  fReversed:=(flags and 1)<>0;
  fPingpong:=(flags and 2)<>0;
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


{ TColorClusters }

constructor TColorClusters.Create;
begin
  inherited Create;
  Add(TColorCluster.Create($FF000000,$FFFFFFFF));
end;

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
  Xs.Free;
end;

procedure TColorClusters.SaveToStream(pStream:TStream);
var i:integer;curr:int64;s:String;
begin
  s:=COLORCLUSTERSBLOCKID+#1;
  pStream.Write(s[1],4);
  curr:=pStream.Position;
  i:=0;
  pStream.Write(i,4);
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
  Xs.Free;
end;

procedure TColorClusters.LoadFromStream(pStream:TStream);
var size,curr:int64;count:integer;s:string;
    tmp:TColorCluster;
begin
  s:=#0#0#0#0;
  pStream.Read(s[1],4);
  if uppercase(copy(s,1,3))<>COLORCLUSTERSBLOCKID then raise
    Exception.Create(Format('ColorClusters block ID expected, got %s)',[copy(s,1,3)]));
  if ord(s[1]) and $20<>0 then Exception.Create('ColorClusters block cannot be compressed!');

  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;

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
  pStream.Position:=curr+size;
end;

function TColorClusters.fGetActiveColorCluster:TColorCluster;
begin
  Result:=Self[fActiveIndex];
end;

procedure TColorClusters.fSetActiveIndex(value:integer);
begin
  if (value>=0) and (value<Count) then fActiveIndex:=value;
end;


{ TBDSimpleColorCluster }

constructor TBDSimpleColorCluster.Create(iLeft,iTop:integer;iColorCluster:TColorCluster);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  fColorCluster:=iColorCluster;
  Width:=COLORCLUSTERWIDTH;
  Height:=COLORCLUSTERHEIGHT;
  fNeedRedraw:=true;
//  OnClick:=Click;
end;

procedure TBDSimpleColorCluster.ReDraw;
const XWidth=27;
var i:integer;XLeft:integer;
begin
  if Assigned(fTexture) then begin
    fColorsWidth:=Width-3-XWidth;
    XLeft:=Width-XWidth;
    with fTexture.ARGBImage do begin
      // Outer border
      if Selected then i:=SYSTEMCOLORHIGHLIGHT else i:=SYSTEMCOLORDARK;
      Bar(0,0,Width,3,SystemPalette[i]);
      Bar(0,Height-3,Width,3,SystemPalette[i]);
      Bar(0,3,3,Height-6,SystemPalette[i]);
      Bar(Width-3,3,3,Height-6,SystemPalette[i]);
      // X left border and background
      Bar(XLeft,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      if not Selected then
        Bar(XLeft+3,3,XWidth-6,Height-6,SystemPalette[SYSTEMCOLORMID])
      else
        Bar(XLeft+3,3,XWidth-6,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      // Color cluster bar
      if Assigned(fColorCluster) then begin
        for i:=0 to fColorsWidth-1 do
          VLine(i+3,3,Height-6,fColorCluster.GetColorAt(i,fColorsWidth-1));
      end;
      // X
      MM.Fonts['Black'].OutText(fTexture.ARGBImage,'X',XLeft+XWidth div 2,9,1);
    end;
    fTexture.Update;
  end;
end;

function TBDSimpleColorCluster.fGetColorsWidth: integer;
begin
  Result:=fColorsWidth+6;
end;

end.

