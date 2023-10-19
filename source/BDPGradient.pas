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

unit BDPGradient;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fgl, vcc2_VisibleControl, ARGBImageUnit, Font2Unit;

type

  { TGradient }

  TGradient=class
    // Create gradient.
    constructor Create(iColor1,iColor2:uint32);

    // Create the gradient from stream (see fileformats.txt - CCS-block)
    constructor CreateFromStream(iStream:TStream);

    // Gives back the color in the gradient at pValue on a scale to 0..pInterval
    // Example: if you want to draw the gradient on a control with a width
    //          of 240 pixels, you should use GetColorAt(x,240) as each vertical
    //          line of the control.
    function GetColorAt(pValue,pInterval:integer):uint32;

    // Gives back the color in the gradient at pValue on a scale to 0..pInterval
    // randomly modifying it by max +/-(pInterval*pDitherStrength/256)
    function GetColorAtDithered(pValue,pInterval,pDitherStrength:integer):uint32;

    // Save gradient to the specified stream. (see fileformats.txt - CCS-block)
    procedure SaveToStream(pStream:TStream);

    // Load gradient from the specified stream. (see fileformats.txt - CCS-block)
    procedure LoadFromStream(pStream:TStream);
  private
    fColors:array[1..5] of uint32;
    fColorPositions:array[3..5] of double;
    fR,fG,fB,fA:array[1..5] of integer;
//    fColor1,fColor2,fColor3,fColor4,fColor5:uint32;
//    fR1,fG1,fB1,fA1,fR2,fG2,fB2,fA2:integer;
    fReversed,fPingpong:boolean;
    procedure fSetColor(index:integer;value:uint32);
    function fGetColor(index:integer):uint32;
  public
    property Colors[index:integer]:uint32 read fGetColor write fSetColor;

    property Reversed:boolean read fReversed write fReversed;
    property PingPong:boolean read fPingpong write fPingpong;
  end;

  { TGradientList }

  TGradientList=class(TFPGObjectList<TGradient>)
    // Create the list with one default gradient element.
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
    function fGetActiveGradient:TGradient;
    procedure fSetActiveIndex(value:integer);
  public
    property ActiveGradient:TGradient read fGetActiveGradient;
    property ActiveIndex:integer read fActiveIndex write fSetActiveIndex;
  end;


implementation

uses BDPShared, Logger;

const
  GRADIENTSBLOCKID='CCS';

{ TGradient }

constructor TGradient.Create(iColor1,iColor2:uint32);
begin
  fSetColor(1,iColor1);
  fSetColor(2,iColor2);
  fColorPositions[3]:=0;
  fColorPositions[4]:=0;
  fColorPositions[5]:=0;
  fReversed:=false;
  fPingpong:=false;
end;

constructor TGradient.CreateFromStream(iStream:TStream);
begin
  LoadFromStream(iStream);
end;

function TGradient.GetColorAt(pValue, pInterval: integer): uint32;
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
      ((fA[1]+(fA[2]-fA[1])*pValue div (pInterval-1)) and $ff) << 24+
      ((fR[1]+(fR[2]-fR[1])*pValue div (pInterval-1)) and $ff) << 16+
      ((fG[1]+(fG[2]-fG[1])*pValue div (pInterval-1)) and $ff) << 8+
      ((fB[1]+(fB[2]-fB[1])*pValue div (pInterval-1)) and $ff)
  end else begin
    Result:=
      ((fA[2]+(fA[1]-fA[2])*pValue div (pInterval-1)) and $ff) << 24+
      ((fR[2]+(fR[1]-fR[2])*pValue div (pInterval-1)) and $ff) << 16+
      ((fG[2]+(fG[1]-fG[2])*pValue div (pInterval-1)) and $ff) << 8+
      ((fB[2]+(fB[1]-fB[2])*pValue div (pInterval-1)) and $ff)
  end;
end;

function TGradient.GetColorAtDithered(pValue, pInterval,
  pDitherStrength: integer): uint32;
var dith:integer;
begin
  dith:=pInterval*pDitherStrength div 256;
  if dith>0 then pValue+=random(2*dith)-dith;
  Result:=GetColorAt(pValue,pInterval);
end;

procedure TGradient.SaveToStream(pStream:TStream);
var flags:byte;
begin
  pStream.Write(fColors[1],4);
  pStream.Write(fColors[2],4);
  flags:=0;
  if fReversed then flags:=flags or 1;
  if fPingpong then flags:=flags or 2;
  pStream.Write(flags,1);
end;

procedure TGradient.LoadFromStream(pStream:TStream);
var tmp:uint32;flags:byte;
begin
  tmp:=0;
  pStream.Read(tmp,4);
  fSetColor(1,tmp);
  pStream.Read(tmp,4);
  fSetColor(2,tmp);
  flags:=0;
  pStream.Read(flags,1);
  fReversed:=(flags and 1)<>0;
  fPingpong:=(flags and 2)<>0;
end;

procedure TGradient.fSetColor(index:integer; value:uint32);
begin
  if index in [1..5] then begin
    if fColors[index]<>value then begin
      fColors[index]:=value;
      fA[index]:=(fColors[index] and $FF000000)>>24;
      fR[index]:=(fColors[index] and $FF0000)>>16;
      fG[index]:=(fColors[index] and $FF00)>>8;
      fB[index]:=(fColors[index] and $FF);
    end;
  end;
end;

function TGradient.fGetColor(index:integer):uint32;
begin
  if index in [1..5] then
    Result:=fColors[index]
  else
    Result:=0;
end;


{ TGradientList }

constructor TGradientList.Create;
begin
  inherited Create;
  Add(TGradient.Create($FF000000,$FFFFFFFF));
end;

constructor TGradientList.CreateFromStream(iStream:TStream);
begin
  inherited Create;
  LoadFromStream(iStream);
end;

procedure TGradientList.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  SaveToStream(Xs);
  Xs.Free;
end;

procedure TGradientList.SaveToStream(pStream:TStream);
var i:integer;curr:int64;s:String;
begin
  s:=GRADIENTSBLOCKID+#1;
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

procedure TGradientList.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead or fmShareDenyNone);
  SaveToStream(Xs);
  Xs.Free;
end;

procedure TGradientList.LoadFromStream(pStream:TStream);
var size,curr:int64;count:integer;s:string;
    tmp:TGradient;
begin
  s:=#0#0#0#0;
  pStream.Read(s[1],4);
  if uppercase(copy(s,1,3))<>GRADIENTSBLOCKID then raise
    Exception.Create(Format('Gradients block ID expected, got %s)',[copy(s,1,3)]));
  if ord(s[1]) and $20<>0 then Exception.Create('Gradients block cannot be compressed!');

  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;

  count:=0;
  pStream.Read(count,1);
  fActiveIndex:=0;
  pStream.Read(fActiveIndex,1);
  Clear;
  while count>0 do begin
    tmp:=TGradient.CreateFromStream(pStream);
    Add(tmp);
    dec(count);
  end;
  pStream.Position:=curr+size;
end;

function TGradientList.fGetActiveGradient:TGradient;
begin
  Result:=Self[fActiveIndex];
end;

procedure TGradientList.fSetActiveIndex(value:integer);
begin
  if (value>=0) and (value<Count) then fActiveIndex:=value;
end;

end.

