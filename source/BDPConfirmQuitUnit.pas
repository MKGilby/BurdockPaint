unit BDPConfirmQuitUnit;

{$mode Delphi}

interface

uses
  vcc2_Container, mk_sdl2, MKMouse2;

type

  { TConfirmQuitDialog }

  TConfirmQuitDialog=class(TContainer)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    function KeyDownYes(Sender:TObject;key:integer):boolean;
    function KeyDownNo(Sender:TObject;key:integer):boolean;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function Click(Sender:TObject;x,y,buttons: integer):boolean;
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseUp(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer):boolean;
    function KeyDown(Sender:TObject;key:integer):boolean;
    function KeyUp(Sender:TObject;key:integer):boolean;
  private
    fTexture:TStreamingTexture;
    fWindowLeft,fWindowTop:integer;
  end;

implementation

uses
  SysUtils, BDPSharedUnit, BDPButtonUnit, BDPMessageUnit,
  BDPKeyMappingUnit;

const
  QUITWINDOWWIDTH=480;
  QUITWINDOWHEIGHT=96;

{ TConfirmQuitDialog }

constructor TConfirmQuitDialog.Create;
var atmB:TBDButton;msg:TMessage;
begin
  inherited Create;
  SetBoundsWH(0,0,WINDOWWIDTH,WINDOWHEIGHT);
  fName:='ConfirmQuit';
  fWindowLeft:=(WINDOWWIDTH-QUITWINDOWWIDTH) div 2;
  fWindowTop:=(WINDOWHEIGHT-QUITWINDOWHEIGHT) div 2;
  fTexture:=TStreamingTexture.Create(QUITWINDOWWIDTH,QUITWINDOWHEIGHT);
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(0,0,3,fTexture.ARGBImage.Height,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(fTexture.ARGBImage.Width-3,0,3,fTexture.ARGBImage.Height,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(0,fTexture.ARGBImage.Height-3,fTexture.ARGBImage.Width,3,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,OverlayImage.Palette[3]);
  MM.Fonts['Black'].OutText(fTexture.ARGBImage,'EXIT BURDOCK PAINT?',QUITWINDOWWIDTH div 2,16,1);
  fTexture.Update;
  msg.TypeID:=MSG_QUIT;
  msg.DataInt:=1;
  atmB:=TBDButton.Create(
    fWindowLeft+QUITWINDOWWIDTH div 4-NORMALBUTTONWIDTH div 2,
    fWindowTop+48,
    NORMALBUTTONWIDTH,
    'YES','',msg);
  atmB.ZIndex:=MaxLongint;
  atmB.OnKeyDown:=KeyDownYes;
  AddChild(atmB);
  msg.DataInt:=0;
  atmB:=TBDButton.Create(
    fWindowLeft+QUITWINDOWWIDTH div 4*3-NORMALBUTTONWIDTH div 2,
    fWindowTop+48,
    NORMALBUTTONWIDTH,
    'NO','',msg);
  atmB.ZIndex:=MaxLongint;
  atmB.OnKeyDown:=KeyDownNo;
  AddChild(atmB);
  OnMouseMove:=MouseMove;
  OnMouseDown:=MouseDown;
  OnMouseUp:=MouseUp;
  OnMouseWheel:=MouseWheel;
  OnClick:=Click;
  OnKeyDown:=KeyDown;
  OnKeyUp:=KeyUp;
end;

destructor TConfirmQuitDialog.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TConfirmQuitDialog.Draw;
begin
  if fVisible then
    PutTexture(fWindowLeft,fWindowTop,fTexture);
end;

function TConfirmQuitDialog.KeyDownYes(Sender:TObject; key:integer):boolean;
begin
  if key=KeyMap[KEY_YES] then begin
    MessageQueue.AddMessage(MSG_QUIT,1);
    Result:=true;
  end else Result:=false;
end;

function TConfirmQuitDialog.KeyDownNo(Sender:TObject; key:integer):boolean;
begin
  if key=KeyMap[KEY_NO] then begin
    MessageQueue.AddMessage(MSG_QUIT,0);
    Result:=true;
  end else Result:=false;
end;

function TConfirmQuitDialog.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  Result:=true;
end;

function TConfirmQuitDialog.Click(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TConfirmQuitDialog.MouseDown(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TConfirmQuitDialog.MouseUp(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TConfirmQuitDialog.MouseWheel(Sender:TObject; x,y,wheelx,wheely:integer):boolean;
begin
  Result:=true;
end;

function TConfirmQuitDialog.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

function TConfirmQuitDialog.KeyUp(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

end.

