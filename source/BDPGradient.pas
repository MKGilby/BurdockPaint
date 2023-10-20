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
    constructor CreateFromStreamCCS1(iStream:TStream);

    // Create the gradient from stream (see fileformats.txt - GDT-block)
    constructor CreateFromStreamGDT1(iStream:TStream);

    // Gives back the color in the gradient at pValue on a scale to 0..1
    function GetColorAt(pValue:double):uint32;

    // Gives back the color in the gradient at pValue on a scale to 0..1
    // randomly modifying it by max +/- Settings.RealDitherStrength (0..1)
    function GetColorAtDithered(pValue:double):uint32;

    // Save gradient to the specified stream. (see fileformats.txt - GDT-block)
    procedure SaveToStream(pStream:TStream);

    // Load gradient from the specified stream. (see fileformats.txt - CCS-block)
    procedure LoadFromStreamCCS1(pStream:TStream);

    // Load gradient from the specified stream. (see fileformats.txt - GDT-block)
    procedure LoadFromStreamGDT1(pStream:TStream);

    // Log contents
    procedure LogContents;

  private
    fColors:array[1..5] of uint32;
    fColorPositions:array[1..5] of double;
    fUsed:array[1..5] of boolean;
    fR,fG,fB,fA:array[1..5] of integer;

    fOrder:array of integer;

    fReversed,fPingpong:boolean;
    procedure fSetColor(index:integer;value:uint32);
    function fGetColor(index:integer):uint32;
    procedure fSetColorPos(index:integer;value:double);
    function fGetColorPos(index:integer):double;
    function fGetColorUsed(index:integer):boolean;
    procedure fSetColorUsed(index:integer;value:boolean);
    procedure SortColors;
  public
    property Colors[index:integer]:uint32 read fGetColor write fSetColor;
    property ColorPositions[index:integer]:double read fGetColorPos write fSetColorPos;
    property ColorUsed[index:integer]:boolean read fGetColorUsed write fSetColorUsed;
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
  COLORCLUSTERSBLOCKID='CCS';
  GRADIENTSBLOCKID='GDT';

  FLAGS_REVERSED=1;
  FLAGS_PINGPONG=2;
  FLAGS_COLOR3_USED=4;
  FLAGS_COLOR4_USED=8;
  FLAGS_COLOR5_USED=16;

{ TGradient }

constructor TGradient.Create(iColor1,iColor2:uint32);
var i:integer;
begin
  fUsed[1]:=true;
  fUsed[2]:=true;
  fUsed[3]:=false;
  fUsed[4]:=false;
  fUsed[5]:=false;
  fColorPositions[1]:=0;
  fColorPositions[2]:=1;
  fColorPositions[3]:=0.25;
  fColorPositions[4]:=0.5;
  fColorPositions[5]:=0.75;
  fSetColor(1,iColor1);
  fSetColor(2,iColor2);
  fReversed:=false;
  fPingpong:=false;
end;

constructor TGradient.CreateFromStreamCCS1(iStream:TStream);
begin
  Create(0,0);
  LoadFromStreamCCS1(iStream);
end;

constructor TGradient.CreateFromStreamGDT1(iStream:TStream);
begin
  Create(0,0);
  LoadFromStreamGDT1(iStream);
end;

function TGradient.GetColorAt(pValue:double):uint32;
var i:integer;
begin
  if pValue<0 then pValue:=0
  else if pValue>1 then pValue:=1;
  if fPingpong then begin
    pValue:=pValue*2;
    if pValue>1 then pValue:=2-pValue;
  end;
  if fReversed then pValue:=1-pValue;
  Result:=0;
  for i:=0 to length(fOrder)-2 do
    if (pValue>=fColorPositions[fOrder[i]]) and
       (pValue<=fColorPositions[fOrder[i+1]]) then begin
     Result:=
       (round(fA[fOrder[i]]+(fA[fOrder[i+1]]-fA[fOrder[i]])*(pValue-fColorPositions[fOrder[i]])/(fColorPositions[fOrder[i+1]]-fColorPositions[fOrder[i]])) and $ff) << 24+
       (round(fR[fOrder[i]]+(fR[fOrder[i+1]]-fR[fOrder[i]])*(pValue-fColorPositions[fOrder[i]])/(fColorPositions[fOrder[i+1]]-fColorPositions[fOrder[i]])) and $ff) << 16+
       (round(fG[fOrder[i]]+(fG[fOrder[i+1]]-fG[fOrder[i]])*(pValue-fColorPositions[fOrder[i]])/(fColorPositions[fOrder[i+1]]-fColorPositions[fOrder[i]])) and $ff) << 8+
       (round(fB[fOrder[i]]+(fB[fOrder[i+1]]-fB[fOrder[i]])*(pValue-fColorPositions[fOrder[i]])/(fColorPositions[fOrder[i+1]]-fColorPositions[fOrder[i]])) and $ff);
     exit;
   end;
end;

function TGradient.GetColorAtDithered(pValue:double):uint32;
begin
  pValue+=random*Settings.RealDitherStrength*2-Settings.RealDitherStrength;
  Result:=GetColorAt(pValue);
end;

procedure TGradient.SaveToStream(pStream:TStream);
var flags:byte;
begin
  pStream.Write(fColors[1],4);
  pStream.Write(fColors[2],4);
  flags:=0;
  if fReversed then flags:=flags or FLAGS_REVERSED;
  if fPingpong then flags:=flags or FLAGS_PINGPONG;
  if fUsed[3] then flags:=flags or FLAGS_COLOR3_USED;
  if fUsed[4] then flags:=flags or FLAGS_COLOR4_USED;
  if fUsed[5] then flags:=flags or FLAGS_COLOR5_USED;
  pStream.Write(flags,1);
  pStream.Write(fColors[3],4);
  pStream.Write(fColorPositions[3],8);
  pStream.Write(fColors[4],4);
  pStream.Write(fColorPositions[4],8);
  pStream.Write(fColors[5],4);
  pStream.Write(fColorPositions[5],8);
end;

procedure TGradient.LoadFromStreamCCS1(pStream:TStream);
var tmp:uint32;flags:byte;
begin
  tmp:=0;
  pStream.Read(tmp,4);
  fSetColor(1,tmp);
  pStream.Read(tmp,4);
  fSetColor(2,tmp);
  flags:=0;
  pStream.Read(flags,1);
  fReversed:=(flags and FLAGS_REVERSED)<>0;
  fPingpong:=(flags and FLAGS_PINGPONG)<>0;
end;

procedure TGradient.LoadFromStreamGDT1(pStream:TStream);
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
  pStream.Read(tmp,4);
  fSetColor(3,tmp);
  pStream.Read(fColorPositions[3],8);
  if fColorPositions[3]<=0 then fColorPositions[3]:=0.01
  else if fColorPositions[3]>=1 then fColorPositions[3]:=0.99;
  fUsed[3]:=(flags and FLAGS_COLOR3_USED)=FLAGS_COLOR3_USED;
  pStream.Read(tmp,4);
  fSetColor(4,tmp);
  pStream.Read(fColorPositions[4],8);
  if fColorPositions[4]<=0 then fColorPositions[4]:=0.01
  else if fColorPositions[4]>=1 then fColorPositions[4]:=0.99;
  fUsed[4]:=(flags and FLAGS_COLOR4_USED)=FLAGS_COLOR4_USED;
  pStream.Read(tmp,4);
  fSetColor(5,tmp);
  pStream.Read(fColorPositions[5],8);
  if fColorPositions[5]<=0 then fColorPositions[5]:=0.01
  else if fColorPositions[5]>=1 then fColorPositions[5]:=0.99;
  fUsed[5]:=(flags and FLAGS_COLOR5_USED)=FLAGS_COLOR5_USED;
end;

procedure TGradient.LogContents;
var c:char;i:integer;
begin
  Log.LogStatus('-------------------------------');
  Log.LogStatus('Gradient content loggint starts');
  Log.LogStatus('');
  Log.LogStatus(Format('Start color: %.8x',[fColors[1]]));
  Log.LogStatus(Format('End color  : %.8x',[fColors[2]]));
  for i:=3 to 5 do begin
    if fUsed[i] then c:='X' else c:=' ';
    Log.LogStatus(Format('[%s] Color %d: %.8x, Position: %4.2f',[c,i,fColors[i],fColorPositions[i]]));
  end;
  Log.LogStatus('Gradient content loggint ends');
  Log.LogStatus('-----------------------------');
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
      SortColors;
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

procedure TGradient.fSetColorPos(index:integer; value:double);
begin
  if (index in [3..5]) and (value>0) and (value<1) then
    if fColorPositions[index]<>value then begin
      fColorPositions[index]:=value;
      SortColors;
    end;
end;

function TGradient.fGetColorPos(index:integer):double;
begin
  if index in [1..5] then
    Result:=fColorPositions[index]
  else
    Result:=-1;
end;

function TGradient.fGetColorUsed(index:integer):boolean;
begin
  if index in [1..5] then
    Result:=fUsed[index]
  else
    Result:=false;
end;

procedure TGradient.fSetColorUsed(index:integer; value:boolean);
begin
  if index in [3..5] then
    if fUsed[index]<>value then begin
      fUsed[index]:=value;
      SortColors;
    end;
end;

procedure TGradient.SortColors;
var i,j,k:integer;
begin
  SetLength(fOrder,0);
  for i:=1 to 5 do if fUsed[i] then begin
    j:=0;
    while (j<length(fOrder)) and (fColorPositions[fOrder[j]]<fColorPositions[i]) do inc(j);
    if j=length(fOrder) then begin  // Add at end
      SetLength(fOrder,length(fOrder)+1);
      fOrder[length(fOrder)-1]:=i;
    end else begin  // Insert at j
      SetLength(fOrder,length(fOrder)+1);
      for k:=length(fOrder)-1 downto j+1 do
        fOrder[k]:=fOrder[k-1];
      fOrder[j]:=i;
    end;
  end;
  Log.Trace('Order:');
  for i:=0 to length(fOrder)-1 do
    Log.Trace(Format('  %d. %d',[i+1,fOrder[i]]));
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
    tmp:TGradient;mode:(mCCS1,mGDT1);
begin
  s:=#0#0#0#0;
  pStream.Read(s[1],4);
  mode:=mGDT1;
  if UpperCase(copy(s,1,3))=COLORCLUSTERSBLOCKID then mode:=mCCS1
  else if UpperCase(copy(s,1,3))=GRADIENTSBLOCKID then mode:=mGDT1
  else raise Exception.Create(Format('Gradients block ID expected, got %s)',[copy(s,1,3)]));
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
    if mode=mCCS1 then tmp:=TGradient.CreateFromStreamCCS1(pStream)
    else if mode=mGDT1 then tmp:=TGradient.CreateFromStreamGDT1(pStream);
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

