unit BDPRotateDialogUnit;

{$mode Delphi}

interface

uses vcc2_Container, mk_sdl2, BDPButtonUnit;

type

  { TRotateCELDialog }

  TRotateCELDialog=class(TContainer)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function Click(Sender:TObject;x,y,buttons: integer):boolean;
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseUp(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer):boolean;
    function KeyDown(Sender:TObject;key:integer):boolean;
    function KeyUp(Sender:TObject;key:integer):boolean;
    function RotateButtonClick(Sender:TObject;x,y,buttons:integer):boolean;
    function OKButtonClick(Sender:TObject;x,y,buttons:integer):boolean;
  private
    fTexture:TStreamingTexture;
    fRotateButtons:array[0..2] of TBDButton;
    fWindowLeft,fWindowTop:integer;
  end;


implementation

uses SysUtils, BDPSharedUnit, BDPMessageUnit, BDPKeyMappingUnit, SDL2;

const
  ROTATEDIALOGWIDTH=512;
  ROTATEDIALOGHEIGHT=128;

{ TRotateCELDialog }

constructor TRotateCELDialog.Create;
const ROTATES:array[0..2] of integer=(90,180,270);
var atmb:TBDButton;msg:TMessage;i:integer;
begin
  inherited Create;
  SetBoundsWH(0,0,WINDOWWIDTH,WINDOWHEIGHT);
  fName:='Rotate CEL';
  fWindowLeft:=(WINDOWWIDTH-ROTATEDIALOGWIDTH) div 2;
  fWindowTop:=(WINDOWHEIGHT-ROTATEDIALOGHEIGHT) div 2;
  fTexture:=TStreamingTexture.Create(ROTATEDIALOGWIDTH,ROTATEDIALOGHEIGHT);
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(0,0,3,fTexture.ARGBImage.Height,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(fTexture.ARGBImage.Width-3,0,3,fTexture.ARGBImage.Height,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(0,fTexture.ARGBImage.Height-3,fTexture.ARGBImage.Width,3,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,OverlayImage.Palette[3]);
  MM.Fonts['Black'].OutText(fTexture.ARGBImage,'ROTATE CEL',ROTATEDIALOGWIDTH div 2,16,1);
  fTexture.Update;
  msg.TypeID:=MSG_NONE;
  for i:=0 to 2 do begin
    fRotateButtons[i]:=TBDButton.Create(
      fWindowLeft+(ROTATEDIALOGWIDTH div 4)*(i+1)-NORMALBUTTONWIDTH div 2,
      fWindowTop+48,
      NORMALBUTTONWIDTH,
      inttostr(ROTATES[i]),'',msg);
    fRotateButtons[i].ZIndex:=MaxLongint;
    fRotateButtons[i].Tag:=i;
    fRotateButtons[i].OnClick:=RotateButtonClick;
    if i=0 then fRotateButtons[i].Selected:=true;
    AddChild(fRotateButtons[i]);
  end;
  atmB:=TBDButton.Create(
    fWindowLeft+ROTATEDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH,
    'OK','ROTATE CEL',msg);
  atmb.ZIndex:=MaxLongint;
  atmb.OnClick:=OKButtonClick;
  AddChild(atmB);
  msg.TypeID:=MSG_ROTATECEL;
  msg.DataInt:=0;
  atmB:=TBDButton.Create(
    fWindowLeft+(ROTATEDIALOGWIDTH div 3*2)-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH,
    'CANCEL','DON''T ROTATE CEL',msg);
  atmb.ZIndex:=MaxLongint;
  AddChild(atmB);
  OnKeyDown:=KeyDown;
end;

destructor TRotateCELDialog.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TRotateCELDialog.Draw;
begin
  if fVisible then
    PutTexture(fWindowLeft,fWindowTop,fTexture);
end;

function TRotateCELDialog.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  Result:=true;
end;

function TRotateCELDialog.Click(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TRotateCELDialog.MouseDown(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TRotateCELDialog.MouseUp(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TRotateCELDialog.MouseWheel(Sender:TObject; x,y,wheelx,wheely:integer):boolean;
begin
  Result:=true;
end;

function TRotateCELDialog.KeyDown(Sender:TObject; key:integer):boolean;
begin
  if key=SDL_SCANCODE_1 then begin
    fRotateButtons[0].Selected:=true;
    fRotateButtons[1].Selected:=false;
    fRotateButtons[2].Selected:=false;
  end else
  if key=SDL_SCANCODE_2 then begin
    fRotateButtons[0].Selected:=false;
    fRotateButtons[1].Selected:=true;
    fRotateButtons[2].Selected:=false;
  end else
  if key=SDL_SCANCODE_3 then begin
    fRotateButtons[0].Selected:=false;
    fRotateButtons[1].Selected:=false;
    fRotateButtons[2].Selected:=true;
  end else
  if key=SDL_SCANCODE_ESCAPE then begin
    MessageQueue.AddMessage(MSG_ROTATECEL,0);
  end else
  if key=SDL_SCANCODE_RETURN then begin
    if fRotateButtons[0].Selected then
      MessageQueue.AddMessage(MSG_ROTATECEL,1)
    else if fRotateButtons[1].Selected then
      MessageQueue.AddMessage(MSG_ROTATECEL,2)
    else if fRotateButtons[2].Selected then
      MessageQueue.AddMessage(MSG_ROTATECEL,3);
  end;
  Result:=true;
end;

function TRotateCELDialog.KeyUp(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

function TRotateCELDialog.RotateButtonClick(Sender:TObject; x,y,buttons:integer):boolean;
var i:integer;
begin
  for i:=0 to 2 do fRotateButtons[i].Selected:=false;
  (Sender as TBDButton).Selected:=true;
  Result:=true;
end;

function TRotateCELDialog.OKButtonClick(Sender:TObject; x,y,buttons:integer):boolean;
begin
  if fRotateButtons[0].Selected then
    MessageQueue.AddMessage(MSG_ROTATECEL,1)
  else if fRotateButtons[1].Selected then
    MessageQueue.AddMessage(MSG_ROTATECEL,2)
  else if fRotateButtons[2].Selected then
    MessageQueue.AddMessage(MSG_ROTATECEL,3);
  Result:=true;
end;

end.

