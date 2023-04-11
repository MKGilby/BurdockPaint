unit BDPColorClusterUnit;

{$mode Delphi}

interface

uses
  Classes, fgl, vcc2_VisibleControl;

type

  { TColorCluster }

  TColorCluster=class
    constructor Create(iStart,iEnd:integer);
    // Gives back the colorindex in the cluster at pValue on a scale to 0..pInterval
    // Example: if you want to draw the color cluster on a control with a width
    //          of 240 pixels, you should use GetIndexAt(x,239) as each vertical
    //          line of the control.
    function GetIndexAt(pValue,pInterval:integer):word;
    procedure SaveToFile(pFilename:string);
    procedure SaveToStream(pStream:TStream);
  private
    fStart,fEnd:integer;
    fReversed,fPingpong:boolean;
    fRealStart,fRealEnd,fDirection,fSize:integer;
    procedure fSetStart(value:integer);
    procedure fSetEnd(value:integer);
    procedure fSetReversed(value:boolean);
    procedure SetReal;
  public
    property StartIndex:integer read fStart write fSetStart;
    property EndIndex:integer read fEnd write fSetEnd;
    property Reversed:boolean read fReversed write fSetReversed;
    property PingPong:boolean read fPingpong write fPingpong;
    property Size:integer read fSize;
  end;

  { TColorClusters }

  TColorClusters=class(TFPGObjectList<TColorCluster>)
    procedure SaveToFile(pFilename:string);
    procedure SaveToStream(pStream:TStream);
  end;

  { TBDColorCluster }

  TBDColorCluster=class(TVisibleControl)
    procedure Create(iLeft,iTop:integer;iColorCluster:TColorCluster);
  protected
    procedure ReDraw; override;
  private
    fColorCluster:TColorCluster;
  end;

implementation

uses SysUtils, BDPSharedUnit;

const
  COLORCLUSTERID=$4C;
  COLORCLUSTERSID=$6C;

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

function TColorCluster.GetIndexAt(pValue,pInterval:integer):word;
begin
  Result:=fRealStart+(fRealEnd-fRealStart)*pValue div pInterval;
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
  i:=1;
  pStream.Write(i,1);  // Version
  curr:=pStream.Position;
  i:=0;
  pStream.Write(i,4);
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

procedure TColorCluster.fSetStart(value:integer);
begin
  if (value>=0) and (value<MainImage.Palette.Size) then begin
    fStart:=value;
    SetReal;
  end;
end;

procedure TColorCluster.fSetEnd(value:integer);
begin
  if (value>=0) and (value<MainImage.Palette.Size) then begin
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

{ TColorClusters }

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
  i:=1;
  pStream.Write(i,1);  // Version
  curr:=pStream.Position;
  i:=0;
  pStream.Write(i,4);
  i:=Self.Count;
  pStream.Write(i,1);
  for i:=0 to Count-1 do Items[i].SaveToStream(pStream);
  i:=pStream.Position-curr-4;
  pStream.Position:=curr;
  pStream.write(i,4);
  pStream.Position:=pStream.Position+i;
end;

{ TBDColorCluster }

procedure TBDColorCluster.Create(iLeft,iTop:integer;iColorCluster:TColorCluster);
begin
  fLeft:=iLeft;
  fTop:=iTop;
  fColorCluster:=iColorCluster;
  Width:=COLORCLUSTERWIDTH;
  Height:=COLORCLUSTERHEIGHT;
  fNeedRedraw:=true;
end;

procedure TBDColorCluster.ReDraw;
var i:integer;colorindex:word;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      Bar(0,0,Width,3,OverlayImage.Palette[2]);
      Bar(0,3,3,Height-6,OverlayImage.Palette[2]);
      Bar(0,Height-3,Width,3,OverlayImage.Palette[2]);
      Bar(Width-3,3,3,Height-6,OverlayImage.Palette[2]);
      if Assigned(fColorCluster) then begin
        for i:=0 to Width-6-1 do begin
          colorindex:=fColorCluster.GetIndexAt(i,Width-7);
          VLine(3+i,3,Height-6,MainImage.Palette[colorindex]);
          if colorindex=Settings.ActiveColorIndex then begin
            VLine(3+i,Height div 2-3,3,OverlayImage.Palette[4]);
            VLine(3+i,Height div 2,3,OverlayImage.Palette[1]);
          end;
        end;
      end;
    end;
    fTexture.Update;
  end;
end;

end.

