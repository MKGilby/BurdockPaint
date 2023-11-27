{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPCheckBox;

{$mode Delphi}

interface

uses SysUtils, vcc2_ButtonStatic, ARGBImageUnit, BDPMessage, MKMouse2, Font2Unit;

type

  { TBDCheckBox }

  TBDCheckBox=class(TButton)
    constructor Create(iX,iY,iWidth,iHeight:integer;iCaption,iHint:string); overload;
    constructor Create; overload;
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
    procedure Click(Sender:TObject;x, y, buttons: integer);
  protected
    procedure ReDraw; override;
  private
    fHint:string;
    fTickFont:TFont;
    fAssignedObject:TObject;
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    fMessage:TMessage;
  published
    property Hint:string read fHint write fHint;
    property AssignedObject:TObject read fAssignedObject write fAssignedObject;
  public
    OnChange:TSimpleEvent;
    property Message:TMessage read fMessage write fMessage;
  end;

implementation

uses BDPShared, sdl2, mk_sdl2, math;

const
  CHECKBOXBOXSIZE=27;
  CHECKBOXMINHEIGHT=CHECKBOXBOXSIZE;

{ TBDCheckBox }

constructor TBDCheckBox.Create(iX,iY,iWidth,iHeight:integer;iCaption,iHint:string);
begin
  Create;
  Left:=iX;
  Top:=iY;
  Width:=iWidth;
  Height:=min(iHeight,CHECKBOXMINHEIGHT);
  fCaption:=iCaption;
  fHint:=iHint;
  fMessage:=TMessage.Init(MSG_NONE,0,0);
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  if not Assigned(fTLImage) or not Assigned(fTRImage) or
     not Assigned(fBLImage) or not Assigned(fBRImage) then
    raise Exception.Create('TBDCheckbox: GUI element not found!');
  fNeedRedraw:=true;
  fTickFont:=MM.Fonts['Red'];
  Font:=MM.Fonts['Black'];
end;

constructor TBDCheckBox.Create;
begin
  inherited Create;
  fSelected:=false;
  OnMouseEnter:=Self.MouseEnter;
  OnMouseLeave:=Self.MouseLeave;
  OnClick:=Self.Click;
  OnChange:=nil;
end;

procedure TBDCheckBox.MouseEnter(Sender:TObject);
begin
  InfoBar.ShowText(fHint);
end;

procedure TBDCheckBox.MouseLeave(Sender:TObject);
begin
  InfoBar.ShowText('');
end;

procedure TBDCheckBox.Click(Sender:TObject; x,y,buttons:integer);
begin
  case buttons of
    SDL_BUTTON_LEFT:begin  // Left click
      Selected:=not Selected;
      if Assigned(OnChange) then OnChange(Self);
      if fMessage.TypeID<>MSG_NONE then MessageQueue.AddMessage(fMessage);
    end;
  end;
end;

procedure TBDCheckBox.ReDraw;
var boxtop,boxleft,texttop:integer;

  // Later we can do right aligned checkbox if needed...
  procedure DrawBox(x,y:integer);
  begin
    with fImage do begin
      Bar(x+8,y,CHECKBOXBOXSIZE-16,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(x+8,y+(CHECKBOXBOXSIZE-3),CHECKBOXBOXSIZE-16,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(x,y+8,3,CHECKBOXBOXSIZE-16,SystemPalette[SYSTEMCOLORDARK]);
      Bar(x+(CHECKBOXBOXSIZE-3),y+8,3,CHECKBOXBOXSIZE-16,SystemPalette[SYSTEMCOLORDARK]);
      if fSelected then
        Bar(x+3,y+3,CHECKBOXBOXSIZE-6,CHECKBOXBOXSIZE-6,SystemPalette[SYSTEMCOLORLIGHT])
      else begin
        if fEnabled then
          Bar(x+3,y+3,CHECKBOXBOXSIZE-6,CHECKBOXBOXSIZE-6,SystemPalette[SYSTEMCOLORMID])
        else
          Bar(x+3,y+3,CHECKBOXBOXSIZE-6,CHECKBOXBOXSIZE-6,SystemPalette[SYSTEMCOLORDARK])
      end;
    end;
  end;

begin
  fImage.Bar(0,0,Width,Height,SystemPalette[SYSTEMCOLORTRANSPARENT]);
  boxtop:=(Height-CHECKBOXBOXSIZE) div 2;
  if Assigned(fFont) then
    boxleft:=(Width-(CHECKBOXBOXSIZE+9+fFont.TextWidth(fCaption))) div 2
  else
    boxleft:=(Width-CHECKBOXBOXSIZE) div 2;
  DrawBox(boxleft,boxtop);
  fTLImage.CopyTo(0,0,fTLImage.Width,fTLImage.Height,boxleft,boxtop,fImage,true);
  fTRImage.CopyTo(0,0,fTRImage.Width,fTRImage.Height,boxleft+CHECKBOXBOXSIZE-8,boxtop,fImage,true);
  fBLImage.CopyTo(0,0,fBLImage.Width,fBLImage.Height,boxleft,boxtop+CHECKBOXBOXSIZE-8,fImage,true);
  fBRImage.CopyTo(0,0,fBRImage.Width,fBRImage.Height,boxleft+CHECKBOXBOXSIZE-8,boxtop+CHECKBOXBOXSIZE-8,fImage,true);
  texttop:=(Height-fFont.Height) div 2;
  if fSelected and Assigned(fTickFont) then
    fTickFont.OutText(fImage,#134,boxleft+CHECKBOXBOXSIZE div 2,texttop+3,1);
  if Assigned(fFont) and (fCaption<>'') then
    fFont.OutText(fImage,fCaption,boxleft+CHECKBOXBOXSIZE+9,texttop,0);
end;

end.

