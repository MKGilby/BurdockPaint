{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPColorPalette2;

{$mode Delphi}

interface

uses SysUtils, vcc2_ContainerStatic, mk_sdl2;

// This is the colorpalette tool at the right side of the window.

type

  { TBDColorPalette2 }

  TBDColorPalette2=class(TContainerStatic)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
    procedure Draw; override;
  protected
    procedure ReDraw; override;
    procedure fSetHeight(value:integer); override;
  private
    fEntryHeight:integer;
    fPage:integer;
    fCheckered:TTexture;
    procedure Click(Sender:TObject;x,y,button:integer);
    procedure MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer);
    procedure MouseMove(Sender:TObject;x,y:integer);
    procedure MouseLeave(Sender:TObject);
  public
    property Height:integer read fHeight write fSetHeight;
  end;


implementation

uses BDPShared, MKMouse2, sdl2;

{ TBDColorPalette2 }

constructor TBDColorPalette2.Create(iLeft, iTop, iWidth, iHeight: integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=iWidth;
  Height:=iHeight;
  fNeedRedraw:=true;
  ZIndex:=LEVEL1CONTROLS_ZINDEX+10;
  fName:='ColorPalette2';
  OnClick:=Click;
  OnMouseWheel:=MouseWheel;
  OnMouseMove:=MouseMove;
  MouseObjects.Add(Self);
  fEntryHeight:=(fHeight-(12+22+3)) div 16;
  fPage:=0;
  fCheckered:=MM.Textures.ItemByName['AlphaBack'];
end;

procedure TBDColorPalette2.Draw;
begin
  bar(fLeft,fTop,fWidth,fHeight,fCheckered);
  inherited Draw;
end;

procedure TBDColorPalette2.ReDraw;
var i:integer;
begin
  fImage.Bar(0,0,Width,Height,SystemPalette.Colors[SYSTEMCOLORDARK]);
  fImage.Bar(3,3,Width-6,Height-6,SystemPalette.Colors[SYSTEMCOLORMID]);
  fImage.Bar(6,19,Width-12,fEntryHeight*16+3,SystemPalette.Colors[SYSTEMCOLORDARK]);
  if Assigned(Project.CurrentPalette) then begin
    for i:=0 to 15 do begin
      if Project.CurrentPalette.Colors[fPage*16+i]=Settings.ActiveColor then
        fImage.Bar(6,19+i*fEntryHeight,fWidth-12,fEntryHeight+3,SystemPalette.Colors[SYSTEMCOLORHIGHLIGHT]);
      fImage.Bar(9,19+i*fEntryHeight+3,fWidth-18,fEntryHeight-3,
          Project.CurrentPalette.Colors[fPage*16+i]);
    end;
  end;
  MM.Fonts['SmallBlack'].OutText(fImage,inttostr(fPage+1)+'/16',Width div 2,6,1);
end;

procedure TBDColorPalette2.fSetHeight(value: integer);
begin
  inherited fSetHeight(value);
end;

procedure TBDColorPalette2.Click(Sender: TObject; x, y, button: integer);
begin
  x-=Left;
  y-=Top;
  if button=SDL_BUTTON_LEFT then begin
    if (x>=9) and (x<Width-9) then begin
      if (y>=19) and (y<19+fEntryHeight*16) then begin
        Settings.ActiveColor:=Project.CurrentPalette.Colors[fPage*16+((y-19) div fEntryHeight)];
        Refresh;
      end;
    end;
  end;
end;

procedure TBDColorPalette2.MouseWheel(Sender: TObject; x, y, wheelx, wheely: integer);
var pre:integer;
begin
  pre:=fPage;
  fPage-=wheely;
  if fPage<0 then fPage:=0
  else if fPage>15 then fPage:=15;
  if (pre<>fPage) then begin
    MouseMove(Sender,x,y);
    fNeedRedraw:=true;
  end;
end;

procedure TBDColorPalette2.MouseMove(Sender:TObject; x,y:integer);
begin
  x-=Left;
  y-=Top;
  if (x>=9) and (x<Width-9) and (y>=19) and (y<19+fEntryHeight*16) then
    ColorUnderMouse:=Project.CurrentPalette.Colors[fPage*16+((y-19) div fEntryHeight)]
  else
    ColorUnderMouse:=POSTPROCESSCOLOR;
end;

procedure TBDColorPalette2.MouseLeave(Sender:TObject);
begin
  ColorUnderMouse:=POSTPROCESSCOLOR;
end;

end.

