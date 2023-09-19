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

    // Create the color cluster from stream (fileformats.txt - T-block)
    constructor CreateFromStream(iStream:TStream);

    // Gives back the colorindex in the cluster at pValue on a scale to 0..pInterval
    // Example: if you want to draw the color cluster on a control with a width
    //          of 240 pixels, you should use GetIndexAt(x,240) as each vertical
    //          line of the control.
    function GetColorAt(pValue,pInterval:integer):uint32;

    // Gives back the colorindex in the cluster at pValue on a scale to 0..pInterval
    // randomly modifying it by max +/-(pInterval*pDitherStrength/256)
    function GetColorAtDithered(pValue,pInterval,pDitherStrength:integer):uint32;

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
  public
    property Color1:uint32 read fColor1 write fSetColor1;
    property Color2:uint32 read fColor2 write fSetColor2;
    property Reversed:boolean read fReversed write fReversed;
    property PingPong:boolean read fPingpong write fPingpong;
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
    fActiveIndex:integer;
    function fGetActiveColorCluster:TColorCluster;
    procedure fSetActiveIndex(value:integer);
  public
    property ActiveColorCluster:TColorCluster read fGetActiveColorCluster;
    property ActiveIndex:integer read fActiveIndex write fSetActiveIndex;
  end;

  { TBDColorCluster }

  TBDColorCluster=class(TVisibleControl)
    constructor Create(iLeft,iTop:integer;iColorCluster:TColorCluster;iRMBPicks:boolean);
    procedure Refresh; override;
    procedure Click(Sender:TObject;x,y,button:integer);
    procedure Draw; override;
  protected
    procedure ReDraw; override;
    procedure fSetWidth(value:integer); override;
  private
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    fColorCluster:TColorCluster;
    fFont,fFont2:TFont;
    fPingpongSwitchLeft,fReverseSwitchLeft,
    fColorsLeft,fColorsWidth,fArrowLeft:integer;
    fRMBPicks:boolean;
    fPicking:boolean;
    procedure fSetColorCluster(value:TColorCluster);
  public
    property ColorCluster:TColorCluster read fColorCluster write fSetColorCluster;
    property Width:integer read fWidth write fSetWidth;
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

uses BDPShared, SDL2, Logger;

const
  COLORCLUSTERSBLOCKID='CCS';

  PINGPONGSWITCHWIDTH=27;
  REVERSESWITCHWIDTH=27;
  ARROWWIDTH=30;

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

procedure TColorCluster.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  SaveToStream(Xs);
  FreeAndNil(Xs);
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

procedure TColorCluster.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead or fmShareDenyNone);
  SaveToStream(Xs);
  FreeAndNil(Xs);
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
  FreeAndNil(Xs);
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

{ TBDColorCluster }

constructor TBDColorCluster.Create(iLeft,iTop:integer;iColorCluster:TColorCluster;
  iRMBPicks:boolean);
begin
  fLeft:=iLeft;
  fTop:=iTop;
  fColorCluster:=iColorCluster;
  Width:=COLORCLUSTERWIDTH;
  Height:=COLORCLUSTERHEIGHT;
  fNeedRedraw:=true;
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  fFont:=MM.Fonts['Black'];
  fFont2:=MM.Fonts['Red'];
  fPingpongSwitchLeft:=8;
  fReverseSwitchLeft:=fPingpongSwitchLeft+PINGPONGSWITCHWIDTH+3;
  fColorsLeft:=fReverseSwitchLeft+REVERSESWITCHWIDTH+3;
  fArrowLeft:=Width-ARROWWIDTH-3;
  fColorsWidth:=fArrowLeft-fColorsLeft;
  OnClick:=Click;
  fRMBPicks:=iRMBPicks;
  fPicking:=false;
end;

procedure TBDColorCluster.Refresh;
begin
  inherited Refresh;
  fPicking:=false;
end;

procedure TBDColorCluster.Click(Sender:TObject; x,y,button:integer);
begin
  if Assigned(fColorCluster) then begin
    x-=Left;
    y-=Top;
    if (x>=fPingpongSwitchLeft) and (x<fReverseSwitchLeft) then begin
      fColorCluster.PingPong:=not fColorCluster.PingPong;
      fNeedRedraw:=true;
    end else
    if (x>=fReverseSwitchLeft) and (x<fColorsLeft) then begin
      fColorCluster.Reversed:=not fColorCluster.Reversed;
      fNeedRedraw:=true;
    end else
    if (x>=fColorsLeft+3) and (x<fArrowLeft) then begin
      if button=SDL_BUTTON_LEFT then
        Settings.ActiveColor:=fColorCluster.GetColorAt(x-fColorsLeft-3,fColorsWidth-3)
      else if (button=SDL_BUTTON_RIGHT) then begin
        if fRMBPicks then begin
          fPicking:=true;
//          MessageQueue.AddMessage(MSG_ACTIVATEPICKCOLORCLUSTER);
        end else begin
          MessageQueue.AddMessage(MSG_ACTIVATEPALETTEEDITOR);
        end;
      end;
    end else
    if (x>=fArrowLeft) then begin
      MessageQueue.AddMessage(MSG_OPENCOLORCLUSTERDIALOG,fLeft+(fTop<<11)+(fWidth<<22));
    end;
  end;
end;

procedure TBDColorCluster.Draw;
begin
  if fPicking then fNeedRedraw:=true;
  inherited Draw;
end;

procedure TBDColorCluster.ReDraw;
var i,fonttop:integer;color32:uint32;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      // Background
      Bar(0,0,fPingpongSwitchLeft,Height,SystemPalette[SYSTEMCOLORMID]);
      if Assigned(fColorCluster) then begin
        if fColorCluster.PingPong then
          Bar(fPingpongSwitchLeft+3,3,PINGPONGSWITCHWIDTH,Height-6,SystemPalette[SYSTEMCOLORLIGHT])
        else
          Bar(fPingpongSwitchLeft+3,3,PINGPONGSWITCHWIDTH,Height-6,SystemPalette[SYSTEMCOLORMID]);
        if fColorCluster.Reversed then
          Bar(fReverseSwitchLeft+3,3,REVERSESWITCHWIDTH,Height-6,SystemPalette[SYSTEMCOLORLIGHT])
        else
          Bar(fReverseSwitchLeft+3,3,REVERSESWITCHWIDTH,Height-6,SystemPalette[SYSTEMCOLORMID]);
      end;
      Bar(fArrowLeft,0,ARROWWIDTH,Height,SystemPalette[SYSTEMCOLORMID]);
      // Outer border
      Bar(8,0,Width-16,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(8,Height-3,Width-16,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(0,8,3,Height-16,SystemPalette[SYSTEMCOLORDARK]);
      Bar(Width-3,8,3,Height-16,SystemPalette[SYSTEMCOLORDARK]);
      // Vertical separator lines
      Bar(fPingpongSwitchLeft,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      Bar(fReverseSwitchLeft,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      Bar(fColorsLeft,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      Bar(fArrowLeft,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      // Corners
      if Assigned(fTLImage) then
        fTLImage.CopyTo(0,0,fTLImage.Width,fTLImage.Height,0,0,fTexture.ARGBImage,true);
      if Assigned(fTRImage) then
        fTRImage.CopyTo(0,0,fTRImage.Width,fTRImage.Height,fWidth-8,0,fTexture.ARGBImage,true);
      if Assigned(fBLImage) then
        fBLImage.CopyTo(0,0,fBLImage.Width,fBLImage.Height,0,fHeight-8,fTexture.ARGBImage,true);
      if Assigned(fBRImage) then
        fBRImage.CopyTo(0,0,fBRImage.Width,fBRImage.Height,fWidth-8,fHeight-8,fTexture.ARGBImage,true);
      // Flashing if picking
      if fPicking then
        Bar(fColorsLeft,0,fColorsWidth+3,Height,VibroColors.GetColor);
      // Color cluster bar
      if Assigned(fColorCluster) then begin
        for i:=0 to fColorsWidth-1-3 do begin
          color32:=fColorCluster.GetColorAt(i,fColorsWidth-3);
          VLine(fColorsLeft+i+3,3,Height-6,color32);
          if color32=Settings.ActiveColor then begin
            VLine(fColorsLeft+i+3,Height div 2-3,3,SystemPalette[SYSTEMCOLORLIGHT]);
            VLine(fColorsLeft+i+3,Height div 2,3,SystemPalette[SYSTEMCOLORBLACK]);
          end;
        end;
      end;
      // Letters and arrow
      if Assigned(fFont) and Assigned(fFont2) then begin
        fonttop:=(Height-15) div 2;
        if (Assigned(fColorCluster) and fColorCluster.PingPong) then
          fFont2.OutText(fTexture.ARGBImage,'P',fPingpongSwitchLeft+PINGPONGSWITCHWIDTH div 2+3,fonttop,1)
        else
          fFont.OutText(fTexture.ARGBImage,'P',fPingpongSwitchLeft+PINGPONGSWITCHWIDTH div 2+3,fonttop,1);
        if (Assigned(fColorCluster) and fColorCluster.Reversed) then
          fFont2.OutText(fTexture.ARGBImage,'R',fReverseSwitchLeft+REVERSESWITCHWIDTH div 2+3,fonttop,1)
        else
          fFont.OutText(fTexture.ARGBImage,'R',fReverseSwitchLeft+REVERSESWITCHWIDTH div 2+3,fonttop,1);
        fFont.OutText(fTexture.ARGBImage,#130,fArrowLeft+ARROWWIDTH div 2+1,fonttop,1);
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

procedure TBDColorCluster.fSetWidth(value:integer);
begin
  inherited fSetWidth(value);
  fArrowLeft:=Width-ARROWWIDTH-3;
  fColorsWidth:=fArrowLeft-fColorsLeft;
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

