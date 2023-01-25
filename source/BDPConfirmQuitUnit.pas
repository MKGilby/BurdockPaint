unit BDPConfirmQuitUnit;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  vcc_Container2, mk_sdl2, MKMouse2;

type

  { TInvisibleClickCatcher }

  TInvisibleClickCatcher=class(TMouseObject)
    constructor Create;
    procedure Draw; override;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function Click(Sender:TObject;x,y,buttons: integer):boolean;
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseUp(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer):boolean;
    function KeyDown(Sender:TObject;key:integer):boolean;
    function KeyUp(Sender:TObject;key:integer):boolean;
  end;

  { TConfirmQuitWindow }

  TConfirmQuitWindow=class(TContainer)
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

{ TInvisibleClickCatcher }

constructor TInvisibleClickCatcher.Create;
begin
  SetBoundsWH(0,0,WINDOWWIDTH,WINDOWHEIGHT);
  fVisible:=true;
  fName:='ClickCatcher';
  OnMouseMove:=MouseMove;
  OnMouseDown:=MouseDown;
  OnMouseUp:=MouseUp;
  OnMouseWheel:=MouseWheel;
  OnClick:=Click;
  OnKeyDown:=KeyDown;
  OnKeyUp:=KeyUp;
end;

procedure TInvisibleClickCatcher.Draw;
begin
  ;
end;

function TInvisibleClickCatcher.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  Result:=true;
end;

function TInvisibleClickCatcher.Click(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TInvisibleClickCatcher.MouseDown(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TInvisibleClickCatcher.MouseUp(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TInvisibleClickCatcher.MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer):boolean;
begin
  Result:=true;
end;

function TInvisibleClickCatcher.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

function TInvisibleClickCatcher.KeyUp(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

{ TConfirmQuitWindow }

constructor TConfirmQuitWindow.Create;
var atmB:TBDButton;msg:TMessage;CC:TInvisibleClickCatcher;
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
  msg.TypeID:=MSG_QUIT;
  msg.DataInt:=1;
  atmB:=TBDButton.Create(
    fTexture.ARGBImage,
    fWindowLeft+QUITWINDOWWIDTH div 4-NORMALBUTTONWIDTH div 2,
    fWindowTop+48,
    NORMALBUTTONWIDTH,
    'YES','',msg);
  atmB.ParentX:=fWindowLeft;
  atmB.Parenty:=fWindowTop;
  atmB.ZIndex:=MaxLongint;
  atmB.OnKeyDown:=KeyDownYes;
  AddChild(atmB);
  msg.DataInt:=0;
  atmB:=TBDButton.Create(
    fTexture.ARGBImage,
    fWindowLeft+QUITWINDOWWIDTH div 4*3-NORMALBUTTONWIDTH div 2,
    fWindowTop+48,
    NORMALBUTTONWIDTH,
    'NO','',msg);
  atmB.ParentX:=fWindowLeft;
  atmB.Parenty:=fWindowTop;
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

destructor TConfirmQuitWindow.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TConfirmQuitWindow.Draw;
begin
  if fVisible then begin
    inherited Draw;
    fTexture.Update;
    PutTexture(fWindowLeft,fWindowTop,fTexture);
  end;
end;

function TConfirmQuitWindow.KeyDownYes(Sender:TObject; key:integer):boolean;
begin
  if key=KeyMap[KEY_YES] then begin
    MessageQueue.AddMessage(MSG_QUIT,'',1);
    Result:=true;
  end else Result:=false;
end;

function TConfirmQuitWindow.KeyDownNo(Sender:TObject; key:integer):boolean;
begin
  if key=KeyMap[KEY_NO] then begin
    MessageQueue.AddMessage(MSG_QUIT,'',0);
    Result:=true;
  end else Result:=false;
end;

function TConfirmQuitWindow.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  Result:=true;
end;

function TConfirmQuitWindow.Click(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TConfirmQuitWindow.MouseDown(Sender:TObject; x,y,buttons:integer
  ):boolean;
begin
  Result:=true;
end;

function TConfirmQuitWindow.MouseUp(Sender:TObject; x,y,buttons:integer
  ):boolean;
begin
  Result:=true;
end;

function TConfirmQuitWindow.MouseWheel(Sender:TObject; x,y,wheelx,wheely:integer
  ):boolean;
begin
  Result:=true;
end;

function TConfirmQuitWindow.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

function TConfirmQuitWindow.KeyUp(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

end.

