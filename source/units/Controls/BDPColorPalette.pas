{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPColorPalette;

{$mode Delphi}

interface

uses
  vcc2_VisibleControlStatic, BDPPalette;

type

  { TBDColorPalette }

  TBDColorPalette=class(TVisibleControlStatic)
    constructor Create(iLeft,iTop,iWidth,iHeight,iHorizontalEntryCount,iVerticalEntryCount:integer);
    procedure ReDraw; override;
    procedure SetColor(pColorIndex:integer;pColor:uint32);
  private
    fEntryWidth,fEntryHeight:integer;
    fPalette:TBDPalette;
    fHorizontalEntryCount,
    fVerticalEntryCount:integer;
    procedure fSetPalette(pPalette:TBDPalette);
    procedure Click(Sender:TObject;x,y,buttons:integer);
  public
    property Palette:TBDPalette write fSetPalette;
  end;

implementation

uses BDPShared, sdl2;

//const
//  PALETTEHORIZONTALENTRYCOUNT=16;
//  PALETTEVERTICALENTRYCOUNT=16;

{ TBDColorPalette }

constructor TBDColorPalette.Create(iLeft,iTop,iWidth,iHeight,
  iHorizontalEntryCount,iVerticalEntryCount:integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=iWidth;
  Height:=iHeight;
  fHorizontalEntryCount:=iHorizontalEntryCount;
  fVerticalEntryCount:=iVerticalEntryCount;
  fEntryWidth:=(fWidth-3) div fHorizontalEntryCount;
  fEntryHeight:=(fHeight-3) div fVerticalEntryCount;
  fPalette:=nil;
  fNeedRedraw:=true;
  OnClick:=Click;
end;

procedure TBDColorPalette.ReDraw;
var i,j:integer;
begin
  fImage.Bar(0,0,Width,Height,SystemPalette.Colors[SYSTEMCOLORDARK]);
  if Assigned(fPalette) then begin
    for j:=0 to fVerticalEntryCount-1 do
      for i:=0 to fHorizontalEntryCount-1 do begin
        fImage.Bar(i*fEntryWidth+3,j*fEntryHeight+3,fEntryWidth-3,fEntryHeight-3,
          fPalette.Colors[j*fHorizontalEntryCount+i]);
      end;
  end;
end;

procedure TBDColorPalette.SetColor(pColorIndex: integer; pColor: uint32);
begin
  if (pColorIndex>=0) and (pColorIndex<fPalette.Size) then
    fPalette[pColorIndex]:=pColor;
  Refresh;
end;

procedure TBDColorPalette.fSetPalette(pPalette:TBDPalette);
begin
  fPalette:=pPalette;
  if fPalette.Size<(fHorizontalEntryCount*fVerticalEntryCount) then
    fPalette.Resize(fHorizontalEntryCount*fVerticalEntryCount);
  Refresh;
end;

procedure TBDColorPalette.Click(Sender: TObject; x, y, buttons: integer);
begin
  x-=fLeft;
  y-=fTop;
  if buttons=SDL_BUTTON_LEFT then begin
    Settings.ActiveColor:=fPalette.Colors[y div fEntryHeight*fHorizontalEntryCount+x div fEntryWidth];
  end
  else if buttons=SDL_BUTTON_RIGHT then begin
    MessageQueue.AddMessage(MSG_PALETTEREQUESTCOLOR,y div fEntryHeight*fHorizontalEntryCount+x div fEntryWidth);
  end;
end;

end.

