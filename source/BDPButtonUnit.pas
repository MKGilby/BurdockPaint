unit BDPButtonUnit;

{$mode Delphi}{$H+}

interface

uses vcc2_Button, Font2Unit, ARGBImageUnit, BDPMessageUnit;

type

  { TBDButton }

  TBDButton=class(TButton)
    constructor Create(iX,iY,iWidth:integer;iCaption,iHint:string;
          iClickMessage:TMessage;iAssignedobject:TObject=nil); overload;
    constructor Create; overload;
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
    procedure Click(Sender:TObject;x, y, buttons: integer);
  protected
    procedure ReDraw; override;
  private
    fHint:string;
    fAssignedObject:TObject;
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    fFont2:TFont;
    fMessage:TMessage;
  published
    property Hint:string read fHint write fHint;
    property AssignedObject:TObject read fAssignedObject write fAssignedObject;
  end;

implementation

uses BDPSharedUnit, mk_sdl2;

{ TBDButton }

constructor TBDButton.Create(iX,iY,iWidth:integer;
  iCaption,iHint:string; iClickMessage:TMessage; iAssignedobject:TObject);
begin
  Create;
  Left:=iX;
  Top:=iY;
  Width:=iWidth;
  Height:=NORMALBUTTONHEIGHT;
  TextAlignX:=mjCenter;
  TextOffsetY:=2;
  Font:=MM.Fonts['Black'];
  fFont2:=MM.Fonts['Red'];
  fCaption:=iCaption;
  fName:=fCaption;
  fHint:=iHint;
  fAssignedObject:=iAssignedobject;
  fMessage:=iClickMessage;
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  ReDraw;
end;

constructor TBDButton.Create;
begin
  inherited Create;
  fSelected:=false;
  OnMouseEnter:=Self.MouseEnter;
  OnMouseLeave:=Self.MouseLeave;
  OnClick:=Self.Click;
end;

procedure TBDButton.MouseEnter(Sender:TObject);
begin
  InfoBar.ShowText(fHint);
end;

procedure TBDButton.MouseLeave(Sender:TObject);
begin
  InfoBar.ShowText('');
end;

procedure TBDButton.Click(Sender:TObject; x,y,buttons:integer);
begin
  case buttons of
    1:begin  // Left click
        if fMessage.TypeID<>MSG_NONE then MessageQueue.AddMessage(fMessage);
      end;
  end;
end;

procedure TBDButton.ReDraw;
begin
  with fTexture.ARGBImage do begin
    Bar(8,0,Width-16,3,OverlayImage.Palette[2]);
    Bar(8,Height-3,fWidth-16,3,OverlayImage.Palette[2]);
    Bar(0,8,3,Height-16,OverlayImage.Palette[2]);
    Bar(Width-3,8,3,Height-16,OverlayImage.Palette[2]);
    if fSelected then
      Bar(3,3,Width-6,Height-6,OverlayImage.Palette[4])
    else begin
      if fEnabled then
        Bar(3,3,Width-6,Height-6,OverlayImage.Palette[3])
      else
        Bar(3,3,Width-6,Height-6,OverlayImage.Palette[2]);
    end;
  end;
  if Assigned(fTLImage) then
    fTLImage.CopyTo(0,0,fTLImage.Width,fTLImage.Height,0,0,fTexture.ARGBImage,true);
  if Assigned(fTRImage) then
    fTRImage.CopyTo(0,0,fTRImage.Width,fTRImage.Height,fWidth-8,0,fTexture.ARGBImage,true);
  if Assigned(fBLImage) then
    fBLImage.CopyTo(0,0,fBLImage.Width,fBLImage.Height,0,fHeight-8,fTexture.ARGBImage,true);
  if Assigned(fBRImage) then
    fBRImage.CopyTo(0,0,fBRImage.Width,fBRImage.Height,fWidth-8,fHeight-8,fTexture.ARGBImage,true);
  if not fSelected then begin
    if Assigned(fFont) then
      fFont.OutText(fTexture.ARGBImage,fCaption,fTextAlignPointX-fLeft,fTextAlignPointY+fTextOffsetY-fTop,fTextAlignX)
  end else begin
    if Assigned(fFont2) then
      fFont2.OutText(fTexture.ARGBImage,fCaption,fTextAlignPointX-fLeft,fTextAlignPointY+fTextOffsetY-fTop,fTextAlignX);
  end;
  fTexture.Update;
end;

{procedure TBDButton.fSetSelected(value:boolean);
begin
  if fSelected<>value then begin
    fSelected:=value;
    ReDraw;
  end;
end;

procedure TBDButton.fSetEnabled(value:boolean);
begin
  if fEnabled<>value then begin
    fSelected:=value;
    ReDraw;
  end;
end;}

end.

