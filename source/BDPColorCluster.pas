unit BDPColorCluster;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fgl, vcc2_VisibleControl, ARGBImageUnit, Font2Unit;

type

  { TColorCluster }

  TColorCluster=class
    // Create color cluster.
    constructor Create(iStart,iEnd:integer);

    // Create the color cluster from stream (fileformats.txt - T-block)
    constructor CreateFromStream(iStream:TStream);

    // Gives back the colorindex in the cluster at pValue on a scale to 0..pInterval
    // Example: if you want to draw the color cluster on a control with a width
    //          of 240 pixels, you should use GetIndexAt(x,240) as each vertical
    //          line of the control.
    function GetIndexAt(pValue,pInterval:integer):word;

    // Gives back the colorindex in the cluster at pValue on a scale to 0..pInterval
    // randomly modifying it by max +/-(pInterval*pDitherStrength/256)
    function GetIndexAtDithered(pValue,pInterval,pDitherStrength:integer):word;

    // Save color cluster to a standalone file. (fileformats.txt - T-block)
    procedure SaveToFile(pFilename:string);

    // Save color cluster to the specified stream. (fileformats.txt - T-block)
    procedure SaveToStream(pStream:TStream);

    // Load color cluster from a standalone file. (fileformats.txt - T-block)
    procedure LoadFromFile(pFilename:string);

    // Load color cluster from the specified stream. (fileformats.txt - T-block)
    procedure LoadFromStream(pStream:TStream);
  private
    fStart,fEnd:integer;
    fReversed,fPingpong:boolean;
    fRealStart,fRealEnd,fDirection,fSize:integer;
    procedure fSetStart(value:integer);
    procedure fSetEnd(value:integer);
    procedure fSetReversed(value:boolean);
    procedure SetReal;
    procedure LoadFromStreamV1(pStream:TStream);
  public
    property StartIndex:integer read fStart write fSetStart;
    property EndIndex:integer read fEnd write fSetEnd;
    property Reversed:boolean read fReversed write fSetReversed;
    property PingPong:boolean read fPingpong write fPingpong;
    property Size:integer read fSize;
  end;

  { TColorClusters }

  TColorClusters=class(TFPGObjectList<TColorCluster>)
    // Creates the list with one default color cluster element.
    constructor Create;
    // Creates the list from stream. (fileformats.txt - L-block)
    constructor CreateFromStream(iStream:TStream);
    procedure SaveToFile(pFilename:string);
    procedure SaveToStream(pStream:TStream);
    procedure LoadFromFile(pFilename:string);
    procedure LoadFromStream(pStream:TStream);
  private
    procedure LoadFromStreamV1(pStream:TStream);
  end;

  { TBDColorCluster }

  TBDColorCluster=class(TVisibleControl)
    constructor Create(iLeft,iTop:integer;iColorCluster:TColorCluster);
    procedure Refresh;
  protected
    procedure ReDraw; override;
  private
    fTRImage,fBRImage:TARGBImage;
    fColorCluster:TColorCluster;
    fFont:TFont;
    procedure fSetColorCluster(value:TColorCluster);
  public
    property ColorCluster:TColorCluster read fColorCluster write fSetColorCluster;
  end;

implementation

uses BDPShared;

const
  COLORCLUSTERID=$54;
  COLORCLUSTERSID=$4C;

{ TColorCluster }

constructor TColorCluster.Create(iStart,iEnd:integer);
begin
  fStart:=iStart;
  fEnd:=iEnd;
  fReversed:=false;
  fPingpong:=false;
  fRealStart:=fStart;
  fRealEnd:=fEnd;
end;

constructor TColorCluster.CreateFromStream(iStream:TStream);
begin
  LoadFromStream(iStream);
end;

function TColorCluster.GetIndexAt(pValue,pInterval:integer):word;
begin
  if pValue<0 then pValue:=0
  else if pValue>=pInterval then pValue:=pInterval-1;
  Result:=fRealStart+(fRealEnd-fRealStart+1)*pValue div pInterval;
end;

function TColorCluster.GetIndexAtDithered(pValue,pInterval,pDitherStrength:integer):word;
var dith:integer;
begin
  dith:=pInterval*pDitherStrength div 256;
  if dith>0 then pValue+=random(2*dith)-dith;
  Result:=GetIndexAt(pValue,pInterval);
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
  i:=1;
  pStream.Write(i,1);  // Version
  pStream.Write(fStart,2);
  pStream.Write(fEnd,2);
  flags:=0;
  if fReversed then flags:=flags or 1;
  if fPingpong then flags:=flags or 2;
  pStream.Write(i,1);
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
  else raise Exception.Create(Format('Unknow color cluster data version! (%d)',[b]));
  pStream.Position:=curr+size;
  SetReal;
end;

procedure TColorCluster.fSetStart(value:integer);
begin
  if (value>=0) and (value<Project.CurrentImage.Palette.Size) then begin
    fStart:=value;
    SetReal;
  end;
end;

procedure TColorCluster.fSetEnd(value:integer);
begin
  if (value>=0) and (value<Project.CurrentImage.Palette.Size) then begin
    fEnd:=value;
    SetReal;
  end;
end;

procedure TColorCluster.fSetReversed(value:boolean);
begin
  if value<>fReversed then begin
    fReversed:=value;
    SetReal;
  end;
end;

procedure TColorCluster.SetReal;
begin
  if (fStart<=fEnd) or ((fStart>fEnd) and fReversed) then begin
    fRealStart:=fStart;
    fRealEnd:=fEnd;
    fDirection:=1;
  end else begin
    fRealStart:=fEnd;
    fRealEnd:=fStart;
    fDirection:=-1;
  end;
end;

procedure TColorCluster.LoadFromStreamV1(pStream:TStream);
var flags:byte;
begin
  pStream.Read(fStart,2);
  pStream.Read(fEnd,2);
  flags:=0;
  pStream.Read(flags,1);
  fReversed:=(flags and 1)<>0;
  fPingpong:=(flags and 2)<>0;
end;

{ TColorClusters }

constructor TColorClusters.Create;
begin
  inherited Create;
  Add(TColorCluster.Create(16,31));
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
  i:=1;
  pStream.Write(i,1);  // Version
  i:=Self.Count;
  pStream.Write(i,1);
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
    tmp:=TColorCluster.Create(0,16);
    tmp.LoadFromStream(pStream);
    Add(tmp);
    dec(count);
  end;
end;

{ TBDColorCluster }

constructor TBDColorCluster.Create(iLeft, iTop: integer;iColorCluster: TColorCluster);
begin
  fLeft:=iLeft;
  fTop:=iTop;
  fColorCluster:=iColorCluster;
  Width:=COLORCLUSTERWIDTH;
  Height:=COLORCLUSTERHEIGHT;
  fNeedRedraw:=true;
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  fFont:=MM.Fonts['Black'];
end;

procedure TBDColorCluster.Refresh;
begin
  fNeedRedraw:=true;
end;

procedure TBDColorCluster.ReDraw;
var i,fonttop:integer;colorindex:word;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      Bar(0,0,Width,Height,SystemPalette[3]);
      Bar(0,0,Width-8,3,SystemPalette[2]);
      Bar(0,Height-3,Width-8,3,SystemPalette[2]);
      Bar(0,3,3,Height-6,SystemPalette[2]);
      Bar(Width-3,8,3,Height-16,SystemPalette[2]);
      Bar(Width-Height-3,3,3,Height-6,SystemPalette[2]);
      if Assigned(fTRImage) then
        fTRImage.CopyTo(0,0,fTRImage.Width,fTRImage.Height,fWidth-8,0,fTexture.ARGBImage,true);
      if Assigned(fBRImage) then
        fBRImage.CopyTo(0,0,fBRImage.Width,fBRImage.Height,fWidth-8,fHeight-8,fTexture.ARGBImage,true);
      if Assigned(fColorCluster) then begin
        for i:=0 to Width-Height-6-1 do begin
          colorindex:=fColorCluster.GetIndexAt(i,Width-Height-6);
          VLine(3+i,3,Height-6,Project.CurrentImage.Palette[colorindex]);
          if colorindex=Settings.ActiveColorIndex then begin
            VLine(3+i,Height div 2-3,3,SystemPalette[4]);
            VLine(3+i,Height div 2,3,SystemPalette[1]);
          end;
        end;
      end;
      if Assigned(fFont) then begin
        fonttop:=(Height-15) div 2;
        fFont.OutText(fTexture.ARGBImage,#130,Width-33 div 2,fonttop,1);
      end;
    end;
    fTexture.Update;
  end;
end;

procedure TBDColorCluster.fSetColorCluster(value:TColorCluster);
begin
  fColorCluster:=value;
  fNeedRedraw:=true;
end;

end.

