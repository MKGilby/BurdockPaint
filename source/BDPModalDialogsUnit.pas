unit BDPModalDialogsUnit;

{$mode Delphi}

interface

uses
  BDPModalDialogUnit, BDPButtonUnit;

type

  { TConfirmQuitDialog }

  TConfirmQuitDialog=class(TModalDialog)
    constructor Create;
    function KeyDown(Sender:TObject;key:integer):boolean;
  end;

  { TMagnifyCELDialog }

  TMagnifyCELDialog=class(TModalDialog)
    constructor Create;
    function KeyDown(Sender:TObject;key:integer):boolean;
    function MagnifyButtonClick(Sender:TObject;x,y,buttons:integer):boolean;
    function OKButtonClick(Sender:TObject;x,y,buttons:integer):boolean;
  private
    fMagnifyButtons:array[0..2] of TBDButton;
  end;

  { TRotateCELDialog }

  TRotateCELDialog=class(TModalDialog)
    constructor Create;
    function KeyDown(Sender:TObject;key:integer):boolean;
    function RotateButtonClick(Sender:TObject;x,y,buttons:integer):boolean;
    function OKButtonClick(Sender:TObject;x,y,buttons:integer):boolean;
  private
    fRotateButtons:array[0..2] of TBDButton;
  end;

implementation

uses
  SysUtils, BDPSharedUnit, BDPMessageUnit, BDPKeyMappingUnit, sdl2;

const
  QUITDIALOGWIDTH=480;
  QUITDIALOGHEIGHT=96;
  MAGNIFYDIALOGWIDTH=480;
  MAGNIFYDIALOGHEIGHT=128;
  XBUTTONWIDTH=63;
  ROTATEDIALOGWIDTH=512;
  ROTATEDIALOGHEIGHT=128;

{ TConfirmQuitDialog }

constructor TConfirmQuitDialog.Create;
var atmB:TBDButton;msg:TMessage;
begin
  inherited Create(QUITDIALOGWIDTH,QUITDIALOGHEIGHT);
  fName:='ConfirmQuit';
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(0,0,3,fTexture.ARGBImage.Height,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(fTexture.ARGBImage.Width-3,0,3,fTexture.ARGBImage.Height,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(0,fTexture.ARGBImage.Height-3,fTexture.ARGBImage.Width,3,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,OverlayImage.Palette[3]);
  MM.Fonts['Black'].OutText(fTexture.ARGBImage,'EXIT BURDOCK PAINT?',QUITDIALOGWIDTH div 2,16,1);
  fTexture.Update;
  msg.TypeID:=MSG_QUIT;
  msg.DataInt:=1;
  atmB:=TBDButton.Create(
    fWindowLeft+QUITDIALOGWIDTH div 4-NORMALBUTTONWIDTH div 2,
    fWindowTop+48,
    NORMALBUTTONWIDTH,
    'YES','',msg);
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);
  msg.DataInt:=0;
  atmB:=TBDButton.Create(
    fWindowLeft+QUITDIALOGWIDTH div 4*3-NORMALBUTTONWIDTH div 2,
    fWindowTop+48,
    NORMALBUTTONWIDTH,
    'NO','',msg);
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);
  OnKeyDown:=KeyDown;
end;

function TConfirmQuitDialog.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
  if key=KeyMap[KEY_YES] then
    MessageQueue.AddMessage(MSG_QUIT,1)
  else if key=KeyMap[KEY_NO] then
    MessageQueue.AddMessage(MSG_QUIT,0);
end;

{ TMagnifyCELDialog }

constructor TMagnifyCELDialog.Create;
const MAGNIFIES:array[0..2] of integer=(2,3,5);
var atmb:TBDButton;msg:TMessage;i:integer;
begin
  inherited Create(MAGNIFYDIALOGWIDTH,MAGNIFYDIALOGHEIGHT);
  fName:='Magnify CEL';
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(0,0,3,fTexture.ARGBImage.Height,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(fTexture.ARGBImage.Width-3,0,3,fTexture.ARGBImage.Height,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(0,fTexture.ARGBImage.Height-3,fTexture.ARGBImage.Width,3,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,OverlayImage.Palette[3]);
  MM.Fonts['Black'].OutText(fTexture.ARGBImage,'MAGNIFY CEL',MAGNIFYDIALOGWIDTH div 2,16,1);
  fTexture.Update;
  msg.TypeID:=MSG_NONE;
  for i:=0 to 2 do begin
    fMagnifyButtons[i]:=TBDButton.Create(
      fWindowLeft+(MAGNIFYDIALOGWIDTH div 4)*(i+1)-XBUTTONWIDTH div 2,
      fWindowTop+48,
      XBUTTONWIDTH,
      inttostr(MAGNIFIES[i])+'X','',msg);
    fMagnifyButtons[i].ZIndex:=MODALDIALOG_ZINDEX+1;
    fMagnifyButtons[i].Tag:=i;
    fMagnifyButtons[i].OnClick:=MagnifyButtonClick;
    if i=0 then fMagnifyButtons[i].Selected:=true;
    AddChild(fMagnifyButtons[i]);
  end;
  atmB:=TBDButton.Create(
    fWindowLeft+MAGNIFYDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH,
    'OK','MAGNIFY CEL',msg);
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  atmb.OnClick:=OKButtonClick;
  AddChild(atmB);
  msg.TypeID:=MSG_MAGNIFYCEL;
  msg.DataInt:=0;
  atmB:=TBDButton.Create(
    fWindowLeft+(MAGNIFYDIALOGWIDTH div 3*2)-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH,
    'CANCEL','DON''T MAGNIFY CEL',msg);
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);
  OnKeyDown:=KeyDown;
end;

function TMagnifyCELDialog.KeyDown(Sender:TObject; key:integer):boolean;
begin
  if key=SDL_SCANCODE_2 then begin
    fMagnifyButtons[0].Selected:=true;
    fMagnifyButtons[1].Selected:=false;
    fMagnifyButtons[2].Selected:=false;
  end else
  if key=SDL_SCANCODE_3 then begin
    fMagnifyButtons[0].Selected:=false;
    fMagnifyButtons[1].Selected:=true;
    fMagnifyButtons[2].Selected:=false;
  end else
  if key=SDL_SCANCODE_5 then begin
    fMagnifyButtons[0].Selected:=false;
    fMagnifyButtons[1].Selected:=false;
    fMagnifyButtons[2].Selected:=true;
  end else
  if key=SDL_SCANCODE_ESCAPE then begin
    MessageQueue.AddMessage(MSG_MAGNIFYCEL,0);
  end else
  if key=SDL_SCANCODE_RETURN then begin
    if fMagnifyButtons[0].Selected then
      MessageQueue.AddMessage(MSG_MAGNIFYCEL,2)
    else if fMagnifyButtons[1].Selected then
      MessageQueue.AddMessage(MSG_MAGNIFYCEL,3)
    else if fMagnifyButtons[2].Selected then
      MessageQueue.AddMessage(MSG_MAGNIFYCEL,5);
  end;
  Result:=true;
end;

function TMagnifyCELDialog.MagnifyButtonClick(Sender:TObject;x,y,buttons:integer):boolean;
var i:integer;
begin
  for i:=0 to 2 do fMagnifyButtons[i].Selected:=false;
  (Sender as TBDButton).Selected:=true;
  Result:=true;
end;

function TMagnifyCELDialog.OKButtonClick(Sender:TObject; x,y,buttons:integer):boolean;
begin
  if fMagnifyButtons[0].Selected then
    MessageQueue.AddMessage(MSG_MAGNIFYCEL,2)
  else if fMagnifyButtons[1].Selected then
    MessageQueue.AddMessage(MSG_MAGNIFYCEL,3)
  else if fMagnifyButtons[2].Selected then
    MessageQueue.AddMessage(MSG_MAGNIFYCEL,5);
  Result:=true;
end;

{ TRotateCELDialog }

constructor TRotateCELDialog.Create;
const ROTATES:array[0..2] of integer=(90,180,270);
var atmb:TBDButton;msg:TMessage;i:integer;
begin
  inherited Create(ROTATEDIALOGWIDTH,ROTATEDIALOGHEIGHT);
  fName:='Rotate CEL';
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
    fRotateButtons[i].ZIndex:=MODALDIALOG_ZINDEX+1;
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
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  atmb.OnClick:=OKButtonClick;
  AddChild(atmB);
  msg.TypeID:=MSG_ROTATECEL;
  msg.DataInt:=0;
  atmB:=TBDButton.Create(
    fWindowLeft+(ROTATEDIALOGWIDTH div 3*2)-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH,
    'CANCEL','DON''T ROTATE CEL',msg);
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);
  OnKeyDown:=KeyDown;
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

