unit BDPButtonUnit;

{$mode Delphi}{$H+}

interface

uses vcc_ARGBButton, Font2Unit, ARGBImageUnit, BDPMessageUnit;

type

  { TBDButton }

  TBDButton=class(TARGBButton)
    constructor Create(iTarget:TARGBImage;iX,iY,iWidth:integer;iCaption,iHint:string;
          iClickMessage:TMessage;iAssignedobject:TObject=nil); overload;
    constructor Create(iTarget:TARGBImage); overload;
    destructor Destroy; override;
    procedure Draw; override;
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
    function Click(Sender:TObject;x, y, buttons: integer): boolean;
  private
    fParentX,fParentY:integer;
    fHint:string;
    fAssignedObject:TObject;
    fLeftImage,fRightImage:TARGBImage;
    fFont2:TFont;
    fMessage:TMessage;
  published
    property Hint:string read fHint write fHint;
    property AssignedObject:TObject read fAssignedObject write fAssignedObject;
    property ParentX:integer read fParentX write fParentX;
    property ParentY:integer read fParentY write fParentY;
  end;

implementation

uses BDPSharedUnit;

{ TBDButton }

constructor TBDButton.Create(iTarget:TARGBImage; iX,iY,iWidth:integer;
  iCaption,iHint:string; iClickMessage:TMessage; iAssignedobject:TObject);
begin
  Create(iTarget);
  fLeft:=iX;
  fTop:=iY;
  fWidth:=iWidth;
  fHeight:=NORMALBUTTONHEIGHT;
  TextAlignX:=mjCenter;
  TextOffsetY:=2;
  Font:=MM.Fonts['Black'];
  fFont2:=MM.Fonts['Red'];
  fCaption:=iCaption;
  fName:=fCaption;
  fHint:=iHint;
  fAssignedObject:=iAssignedobject;
  fLeftImage:=MM.Images.ItemByName['ButtonLeft'];
  fRightImage:=MM.Images.ItemByName['ButtonRight'];
  fParentX:=0;
  fParentY:=0;
  fMessage:=iClickMessage;
end;

constructor TBDButton.Create(iTarget:TARGBImage);
begin
  inherited Create(iTarget);
  fSelected:=false;
  OnMouseEnter:=Self.MouseEnter;
  OnMouseLeave:=Self.MouseLeave;
  OnClick:=Self.Click;
end;

destructor TBDButton.Destroy;
begin
  inherited Destroy;
end;

procedure TBDButton.Draw;
begin
  fTarget.Bar(fLeft+8-fParentX,fTop-fParentY,fWidth-16,3,OverlayImage.Palette[2]);
  if fSelected then
    fTarget.Bar(fLeft-fParentX+3,fTop-fParentY+3,fWidth-6,21,OverlayImage.Palette[4])
  else begin
    if fEnabled then
      fTarget.Bar(fLeft-fParentX+3,fTop-fParentY+3,fWidth-6,21,OverlayImage.Palette[3])
    else
      fTarget.Bar(fLeft-fParentX+3,fTop-fParentY+3,fWidth-6,21,OverlayImage.Palette[2]);
  end;
  fTarget.Bar(fLeft-fParentX+8,fTop-fParentY+24,fWidth-16,3,OverlayImage.Palette[2]);
  fLeftImage.CopyTo(0,0,fLeftImage.Width,fLeftImage.Height,fLeft-fParentX,fTop-fParentY,fTarget,true);
  fRightImage.CopyTo(0,0,fRightImage.Width,fRightImage.Height,fLeft-fParentX+fWidth-8,fTop-fParentY,fTarget,true);
//  Log.LogDebug(fCaption);
//  Log.LogDebug(Format('AlignPointX: %d, TextOffsetX: %d, AlignPointY: %d, TextOffsetY: %d',[fAlignPointX,fTextOffsetX,fAlignPointY,fTextOffsetY]));
  if not fSelected then
    fFont.OutText(fTarget,fCaption,fTextAlignPointX-fParentX,fTextAlignPointY+fTextOffsetY-fParentY,fTextAlignX)
  else
    fFont2.OutText(fTarget,fCaption,fTextAlignPointX-fParentX,fTextAlignPointY+fTextOffsetY-fParentY,fTextAlignX);
end;

procedure TBDButton.MouseEnter(Sender:TObject);
begin
  InfoBar.ShowText(fHint);
end;

procedure TBDButton.MouseLeave(Sender:TObject);
begin
  InfoBar.ShowText('');
end;

function TBDButton.Click(Sender:TObject; x,y,buttons:integer):boolean;
begin
  case buttons of
    1:begin  // Left click
        if fMessage.TypeID<>MSG_NONE then MessageQueue.AddMessage(fMessage);
      end;
  end;
  Result:=true;
end;

end.

