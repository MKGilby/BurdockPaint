{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPGradient;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fgl, ARGBImageUnit, Font2Unit;

type

  { TGradient }

  TGradient=class
    // Create gradient.
    constructor Create(iColor1,iColor2:uint32);

    // Create the gradient from stream (see fileformats.txt - GRD-block)
    constructor CreateFromStream(iStream:TStream);

    // Create the gradient from stream (see fileformats.txt - CCS-block)
    constructor CreateFromStreamCCS1(iStream:TStream);

    // Create the gradient from stream (see fileformats.txt - GDT-block)
    constructor CreateFromStreamGDT1(iStream:TStream);

    // Gives back the color in the gradient at pValue on a scale to 0..1
    function GetColorAt(pValue:double):uint32;

    // Gives back the color in the gradient at pValue on a scale to 0..1
    // randomly modifying it by max +/- Settings.RealDitherStrength (0..1)
    function GetColorAtDithered(pValue:double):uint32;

    // Gives back the color in the gradient at pValue on a scale to 0..1
    // discarding reverse or pingpong flags.
    function GetColorAtRaw(pValue:double):uint32;

    // Copies gradient data from the given gradient.
    procedure CopyFrom(pGradient:TGradient);

    // Save gradient to the specified stream. (see fileformats.txt - GRD-block)
    procedure SaveToStream(pStream:TStream);

    // Load gradient from the specified stream. (see fileformats.txt - GRD-block)
    procedure LoadFromStream(pStream:TStream);

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
    fDithered:boolean;
    fDitherStrength:integer;
    fColorBanding:boolean;
    fColorBandCount:integer;

    fOrder:array of integer;

    fReversed,fPingpong:boolean;
    procedure fSetColor(index:integer;value:uint32);
    function fGetColor(index:integer):uint32;
    procedure fSetColorPos(index:integer;value:double);
    function fGetColorPos(index:integer):double;
    function fGetColorUsed(index:integer):boolean;
    procedure fSetColorUsed(index:integer;value:boolean);
    procedure fSetDitherStrength(value:integer);
    procedure fSetColorBandCount(value:integer);
    procedure SortColors;
    procedure LoadFromStreamV1(pStream:TStream);
    procedure LoadFromStreamV2(pStream:TStream);
  public
    property Colors[index:integer]:uint32 read fGetColor write fSetColor;
    property ColorPositions[index:integer]:double read fGetColorPos write fSetColorPos;
    property ColorUsed[index:integer]:boolean read fGetColorUsed write fSetColorUsed;
    property Reversed:boolean read fReversed write fReversed;
    property PingPong:boolean read fPingpong write fPingpong;
    property Dithered:boolean read fDithered write fDithered;
    property DitherStrength:integer read fDitherStrength write fSetDitherStrength;
    property ColorBanding:boolean read fColorBanding write fColorBanding;
    property ColorBandCount:integer read fColorBandCount write fSetColorBandCount;
  end;

  { TGradientList }

  TGradientList=class(TFPGObjectList<TGradient>)
    // Create the list with one default gradient element.
    constructor Create;

    // Create the list from stream. (see fileformats.txt - GDT-block)
    constructor CreateFromStream(iStream:TStream);

    // Save the list to file. (see fileformats.txt - GDT-block)
    procedure SaveToFile(pFilename:string);

    // Save the list to stream. (see fileformats.txt - GDT-block)
    procedure SaveToStream(pStream:TStream);

    // Load the list from file. (see fileformats.txt - GDT-block)
    procedure LoadFromFile(pFilename:string);

    // Load the list from stream. (see fileformats.txt - GDT-block)
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

uses BDPShared, Logger, MyZStreamUnit, BDPInternalFileFormat;

const
  COLORCLUSTERSBLOCKID='CCS';
  GRADIENTSBLOCKID='GDT';
  GRADIENTBLOCKID='GDR';

  FLAGS_REVERSED=1;
  FLAGS_PINGPONG=2;
  FLAGS_COLOR3_USED=4;
  FLAGS_COLOR4_USED=8;
  FLAGS_COLOR5_USED=16;
  FLAGS_DITHERED=32;
  FLAGS_COLORBANDING=64;

{ TGradient }

constructor TGradient.Create(iColor1,iColor2:uint32);
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
  fSetColor(3,0);
  fSetColor(4,0);
  fSetColor(5,0);
  fReversed:=false;
  fPingpong:=false;
  fDithered:=false;
  fDitherStrength:=0;
  fColorBanding:=false;
  fColorBandCount:=2;
end;

constructor TGradient.CreateFromStream(iStream:TStream);
begin
  Create(0,0);
  LoadFromStream(iStream);
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
  if fColorBanding then
    pValue:=round(pValue*fColorBandCount)/fColorBandCount;
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
  pValue+=random*(fDitherStrength/100)*2-(fDitherStrength/100);
  Result:=GetColorAt(pValue);
end;

function TGradient.GetColorAtRaw(pValue:double):uint32;
var i:integer;
begin
  if pValue<0 then pValue:=0
  else if pValue>1 then pValue:=1;
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

procedure TGradient.CopyFrom(pGradient:TGradient);
var i:integer;
begin
  for i:=1 to 5 do begin
    Colors[i]:=pGradient.Colors[i];
    if i>=3 then begin
      ColorUsed[i]:=pGradient.ColorUsed[i];
      ColorPositions[i]:=pGradient.ColorPositions[i];
    end;
    fReversed:=pGradient.Reversed;
    fPingpong:=pGradient.PingPong;
    fDithered:=pGradient.Dithered;
    fDitherStrength:=pGradient.DitherStrength;
    fColorBanding:=pGradient.ColorBanding;
    fColorBandCount:=pGradient.ColorBandCount;
  end;
end;

procedure TGradient.SaveToStream(pStream:TStream);
var Xs:TStream;flags:byte;
begin
  Xs:=TMemoryStream.Create;
  try
    Xs.Write(fColors[1],4);
    Xs.Write(fColors[2],4);
    flags:=0;
    if fReversed then flags:=flags or FLAGS_REVERSED;
    if fPingpong then flags:=flags or FLAGS_PINGPONG;
    if fUsed[3] then flags:=flags or FLAGS_COLOR3_USED;
    if fUsed[4] then flags:=flags or FLAGS_COLOR4_USED;
    if fUsed[5] then flags:=flags or FLAGS_COLOR5_USED;
    if fDithered then flags:=flags or FLAGS_DITHERED;
    if fColorBanding then flags:=flags or FLAGS_COLORBANDING;
    Xs.Write(flags,1);
    Xs.Write(fColors[3],4);
    Xs.Write(fColorPositions[3],8);
    Xs.Write(fColors[4],4);
    Xs.Write(fColorPositions[4],8);
    Xs.Write(fColors[5],4);
    Xs.Write(fColorPositions[5],8);
    Xs.Write(fDitherStrength,1);
    Xs.Write(fColorBandCount,1);
    TInternalFileFormat.WriteBlock(pStream,GRADIENTBLOCKID,2,Xs,false);
  finally
    Xs.Free;
  end;
end;

procedure TGradient.LoadFromStream(pStream:TStream);
var tmp:TInternalBlock;
begin
  tmp:=TInternalFileFormat.ReadBlock(pStream);
  try
    if tmp.BlockID<>GRADIENTBLOCKID then raise Exception.Create(Format('Gradient block expected, got %s!',[copy(tmp.BlockID,1,3)]));
    if tmp.Version=1 then LoadFromStreamV1(tmp.Data)
    else if tmp.Version=2 then LoadFromStreamV2(tmp.Data)
    else raise Exception.Create(Format('Unknown gradient data version! (%d)',[tmp.Version]));
  finally
    tmp.Free;
  end;
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
  SortColors;
end;

procedure TGradient.LogContents;
var c:char;i:integer;s:string;
begin
  Log.LogStatus('-------------------------------');
  Log.LogStatus('Gradient content logging starts');
  Log.LogStatus('');
  Log.LogStatus(Format('Start color: %.8x',[fColors[1]]));
  Log.LogStatus(Format('End color  : %.8x',[fColors[2]]));
  for i:=3 to 5 do begin
    if fUsed[i] then c:='X' else c:=' ';
    Log.LogStatus(Format('[%s] Color %d: %.8x, Position: %4.2f',[c,i,fColors[i],fColorPositions[i]]));
  end;
  Log.LogStatus('');
  s:='    ';
  if fReversed then s[1]:='X';
  if fPingpong then s[2]:='X';
  if fDithered then s[3]:='X';
  if fColorBanding then s[4]:='X';
  Log.LogStatus(Format('[%s] Reversed  [%s] Pingpong  [%s] Dithered  [%s] Banding',[s[1],s[2],s[3],s[4]]));
  Log.LogStatus('');
  Log.LogStatus(Format('Dither strength : %d',[fDitherStrength]));
  Log.LogStatus(Format('Color band count: %d',[fColorBandCount]));
  Log.LogStatus('');
  Log.LogStatus('Gradient content logging ends');
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

procedure TGradient.fSetDitherStrength(value:integer);
begin
  if value<0 then value:=0
  else if value>100 then value:=100;
  if value<>fDitherStrength then fDitherStrength:=value;
end;

procedure TGradient.fSetColorBandCount(value:integer);
begin
  if value<2 then value:=2
  else if value>64 then value:=64;
  if value<>fColorBandCount then fColorBandCount:=value;
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
//  Log.Trace('Order:');
//  for i:=0 to length(fOrder)-1 do
//    Log.Trace(Format('  %d. %d',[i+1,fOrder[i]]));
end;

procedure TGradient.LoadFromStreamV1(pStream:TStream);
begin
  // Since internal format is the same we can use GDT1 loader to load V1 GDR.
  LoadFromStreamGDT1(pStream);
end;

procedure TGradient.LoadFromStreamV2(pStream:TStream);
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
  fDithered:=(flags and 32)<>0;
  fColorBanding:=(flags and 64)<>0;
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
  SortColors;
  tmp:=0;
  pStream.Read(tmp,1);
  DitherStrength:=tmp;  // The setter does a range check.
  pStream.Read(tmp,1);
  ColorBandCount:=tmp;  // The setter does a range check.
end;


{ TGradientList }

constructor TGradientList.Create;
begin
  inherited Create;
  Add(TGradient.Create($FF000000,$FFFFFFFF));
  fActiveIndex:=0;
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
var i:integer;Xs:TStream;
begin
  Xs:=TMemoryStream.Create;
  try
    i:=Self.Count;
    Xs.Write(i,1);
    Xs.Write(fActiveIndex,1);
    for i:=0 to Count-1 do Items[i].SaveToStream(Xs);
    TInternalFileFormat.WriteBlock(pStream,GRADIENTSBLOCKID,2,Xs);
  finally
    Xs.Free;
  end;
end;

procedure TGradientList.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead or fmShareDenyNone);
  SaveToStream(Xs);
  Xs.Free;
end;

procedure TGradientList.LoadFromStream(pStream:TStream);
var count:integer;
    mode:(mCCS1,mGDT1,mGDT2);
    tmp:TInternalBlock;
begin
  tmp:=TInternalFileFormat.ReadBlock(pStream);
  try
    if tmp.BlockID=COLORCLUSTERSBLOCKID then begin
      if tmp.Version=1 then
        mode:=mCCS1
      else
        raise Exception.Create(Format('Unknown CCS block version! (%d)',[tmp.Version]));
    end
    else if tmp.BlockID=GRADIENTSBLOCKID then begin
      if tmp.Version=1 then mode:=mGDT1
      else if tmp.Version=2 then mode:=mGDT2
      else
        raise Exception.Create(Format('Unknown GDT block version! (%d)',[tmp.Version]));
    end
    else raise Exception.Create(Format('Gradients block expected, got %s)',[tmp.BlockID]));
    count:=0;
    tmp.Data.Read(count,1);
    fActiveIndex:=0;
    tmp.Data.Read(fActiveIndex,1);
    Clear;
    while count>0 do begin
      if mode=mCCS1 then Add(TGradient.CreateFromStreamCCS1(tmp.Data))
      else if mode=mGDT1 then Add(TGradient.CreateFromStreamGDT1(tmp.Data))
      else if mode=mGDT2 then Add(TGradient.CreateFromStream(tmp.Data));
      dec(count);
    end;
  finally
    tmp.Free;
  end;
end;

function TGradientList.fGetActiveGradient:TGradient;
begin
  Result:=Self[fActiveIndex];
end;

procedure TGradientList.fSetActiveIndex(value:integer);
begin
  if (value>=0) and (value<Count) and (value<>fActiveIndex) then begin
    fActiveIndex:=value;
    MessageQueue.AddMessage(MSG_ACTIVEGRADIENTCHANGED,fActiveIndex);
  end;
end;

end.

