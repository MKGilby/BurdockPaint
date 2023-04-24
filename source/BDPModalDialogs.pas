unit BDPModalDialogs;

{$mode Delphi}

interface

uses
  SysUtils, mk_sdl2, vcc2_Container, BDPBasicControls;

type

  { TBDModalDialog }

  TBDModalDialog=class(TContainer)
    constructor Create(iWidth,iHeight:integer);
    destructor Destroy; override;
    procedure Draw; override;
    function KeyDown(Sender:TObject;key:integer):boolean;
    function KeyUp(Sender:TObject;key:integer):boolean;
  protected
    fTexture:TStreamingTexture;
    fWindowLeft,fWindowTop:integer;
  end;

  { TBDConfirmQuitDialog }

  TBDConfirmQuitDialog=class(TBDModalDialog)
    constructor Create;
    function KeyDown(Sender:TObject;key:integer):boolean;
  end;

  { TBDMagnifyCELDialog }

  TBDMagnifyCELDialog=class(TBDModalDialog)
    constructor Create;
    function KeyDown(Sender:TObject;key:integer):boolean;
    procedure MagnifyButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
  private
    fMagnifyButtons:array[0..2] of TBDButton;
  end;

  { TBDRotateCELDialog }

  TBDRotateCELDialog=class(TBDModalDialog)
    constructor Create;
    function KeyDown(Sender:TObject;key:integer):boolean;
    procedure RotateButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
  private
    fRotateButtons:array[0..2] of TBDButton;
  end;

  { TBDAboutDialog }

  TBDAboutDialog=class(TBDModalDialog)
    constructor Create;
  end;

  { TBDMessageBox }

  TBDMessageBox=class(TBDModalDialog)
    constructor Create(iMessage:string;iButtons:string='OK');
    function Run:integer;
  end;

function MessageBox(iMessage:string;iButtons:string='OK'):integer;

implementation

uses
  sdl2, BDPShared, BDPMessage, BDPKeyMapping, MKMouse2, MKToolbox;

const
  QUITDIALOGWIDTH=480;
  QUITDIALOGHEIGHT=96;
  MAGNIFYDIALOGWIDTH=480;
  MAGNIFYDIALOGHEIGHT=128;
  XBUTTONWIDTH=63;
  ROTATEDIALOGWIDTH=512;
  ROTATEDIALOGHEIGHT=128;
  SPLASHSCREENWIDTH=640;
  SPLASHSCREENHEIGHT=160;
  MESSAGEBOXHEIGHT=96;

function MessageBox(iMessage:string; iButtons:string):integer;
var MessageBox:TBDMessageBox;j:integer;
begin
  MessageBox:=TBDMessageBox.Create(uppercase(iMessage),uppercase(iButtons));
  Result:=MessageBox.Run;
  j:=MouseObjects.IndexOf(MessageBox);
  if j>-1 then MouseObjects.Delete(j);
  FreeAndNil(MessageBox);
end;


{ TBDModalDialog }

constructor TBDModalDialog.Create(iWidth,iHeight:integer);
begin
  inherited Create;
  SetBoundsWH(0,0,WINDOWWIDTH,WINDOWHEIGHT);
  fWindowLeft:=(WINDOWWIDTH-iWidth) div 2;
  fWindowTop:=(WINDOWHEIGHT-iHeight) div 2;
  fTexture:=TStreamingTexture.Create(iWidth,iHeight);
  OnKeyDown:=KeyDown;
  OnKeyUp:=KeyUp;
  ZIndex:=MODALDIALOG_ZINDEX;
  fVisible:=false;
end;

destructor TBDModalDialog.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TBDModalDialog.Draw;
begin
  PutTexture(fWindowLeft,fWindowTop,fTexture);
end;

function TBDModalDialog.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

function TBDModalDialog.KeyUp(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

{ TBDConfirmQuitDialog }

constructor TBDConfirmQuitDialog.Create;
var atmB:TBDButton;msg:TMessage;
begin
  inherited Create(QUITDIALOGWIDTH,QUITDIALOGHEIGHT);
  fName:='ConfirmQuit';
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,SystemPalette[2]);
  fTexture.ARGBImage.Bar(0,0,3,fTexture.ARGBImage.Height,SystemPalette[2]);
  fTexture.ARGBImage.Bar(fTexture.ARGBImage.Width-3,0,3,fTexture.ARGBImage.Height,SystemPalette[2]);
  fTexture.ARGBImage.Bar(0,fTexture.ARGBImage.Height-3,fTexture.ARGBImage.Width,3,SystemPalette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,SystemPalette[3]);
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
  MouseObjects.Add(Self);
end;

function TBDConfirmQuitDialog.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
  if key=KeyMap[KEY_YES] then
    MessageQueue.AddMessage(MSG_QUIT,1)
  else if key=KeyMap[KEY_NO] then
    MessageQueue.AddMessage(MSG_QUIT,0);
end;

{ TBDMagnifyCELDialog }

constructor TBDMagnifyCELDialog.Create;
const MAGNIFIES:array[0..2] of integer=(2,3,5);
var atmb:TBDButton;msg:TMessage;i:integer;
begin
  inherited Create(MAGNIFYDIALOGWIDTH,MAGNIFYDIALOGHEIGHT);
  fName:='Magnify CEL';
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,SystemPalette[2]);
  fTexture.ARGBImage.Bar(0,0,3,fTexture.ARGBImage.Height,SystemPalette[2]);
  fTexture.ARGBImage.Bar(fTexture.ARGBImage.Width-3,0,3,fTexture.ARGBImage.Height,SystemPalette[2]);
  fTexture.ARGBImage.Bar(0,fTexture.ARGBImage.Height-3,fTexture.ARGBImage.Width,3,SystemPalette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,SystemPalette[3]);
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
  msg.TypeID:=MSG_MAGNIFYCELRESP;
  msg.DataInt:=0;
  atmB:=TBDButton.Create(
    fWindowLeft+(MAGNIFYDIALOGWIDTH div 3*2)-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH,
    'CANCEL','DON''T MAGNIFY CEL',msg);
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);
  OnKeyDown:=KeyDown;
  MouseObjects.Add(Self);
end;

function TBDMagnifyCELDialog.KeyDown(Sender:TObject; key:integer):boolean;
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
    MessageQueue.AddMessage(MSG_MAGNIFYCELRESP,0);
  end else
  if key=SDL_SCANCODE_RETURN then begin
    if fMagnifyButtons[0].Selected then
      MessageQueue.AddMessage(MSG_MAGNIFYCELRESP,2)
    else if fMagnifyButtons[1].Selected then
      MessageQueue.AddMessage(MSG_MAGNIFYCELRESP,3)
    else if fMagnifyButtons[2].Selected then
      MessageQueue.AddMessage(MSG_MAGNIFYCELRESP,5);
  end;
  Result:=true;
end;

procedure TBDMagnifyCELDialog.MagnifyButtonClick(Sender:TObject;x,y,buttons:integer);
var i:integer;
begin
  for i:=0 to 2 do fMagnifyButtons[i].Selected:=false;
  (Sender as TBDButton).Selected:=true;
end;

procedure TBDMagnifyCELDialog.OKButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  if fMagnifyButtons[0].Selected then
    MessageQueue.AddMessage(MSG_MAGNIFYCELRESP,2)
  else if fMagnifyButtons[1].Selected then
    MessageQueue.AddMessage(MSG_MAGNIFYCELRESP,3)
  else if fMagnifyButtons[2].Selected then
    MessageQueue.AddMessage(MSG_MAGNIFYCELRESP,5);
end;

{ TBDRotateCELDialog }

constructor TBDRotateCELDialog.Create;
const ROTATES:array[0..2] of integer=(90,180,270);
var atmb:TBDButton;msg:TMessage;i:integer;
begin
  inherited Create(ROTATEDIALOGWIDTH,ROTATEDIALOGHEIGHT);
  fName:='Rotate CEL';
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,SystemPalette[2]);
  fTexture.ARGBImage.Bar(0,0,3,fTexture.ARGBImage.Height,SystemPalette[2]);
  fTexture.ARGBImage.Bar(fTexture.ARGBImage.Width-3,0,3,fTexture.ARGBImage.Height,SystemPalette[2]);
  fTexture.ARGBImage.Bar(0,fTexture.ARGBImage.Height-3,fTexture.ARGBImage.Width,3,SystemPalette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,SystemPalette[3]);
  MM.Fonts['Black'].OutText(fTexture.ARGBImage,'ROTATE CEL',ROTATEDIALOGWIDTH div 2,16,1);
  fTexture.Update;
  msg.TypeID:=MSG_NONE;
  for i:=0 to 2 do begin
    fRotateButtons[i]:=TBDButton.Create(
      fWindowLeft+(ROTATEDIALOGWIDTH div 4)*(i+1)-NORMALBUTTONWIDTH div 2,
      fWindowTop+48,
      NORMALBUTTONWIDTH,
      inttostr(ROTATES[i])+'°','',msg);
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
  msg.TypeID:=MSG_ROTATECELRESP;
  msg.DataInt:=0;
  atmB:=TBDButton.Create(
    fWindowLeft+(ROTATEDIALOGWIDTH div 3*2)-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH,
    'CANCEL','DON''T ROTATE CEL',msg);
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);
  OnKeyDown:=KeyDown;
  MouseObjects.Add(Self);
end;

function TBDRotateCELDialog.KeyDown(Sender:TObject; key:integer):boolean;
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
    MessageQueue.AddMessage(MSG_ROTATECELRESP,0);
  end else
  if key=SDL_SCANCODE_RETURN then begin
    if fRotateButtons[0].Selected then
      MessageQueue.AddMessage(MSG_ROTATECELRESP,1)
    else if fRotateButtons[1].Selected then
      MessageQueue.AddMessage(MSG_ROTATECELRESP,2)
    else if fRotateButtons[2].Selected then
      MessageQueue.AddMessage(MSG_ROTATECELRESP,3);
  end;
  Result:=true;
end;

procedure TBDRotateCELDialog.RotateButtonClick(Sender:TObject; x,y,buttons:integer);
var i:integer;
begin
  for i:=0 to 2 do fRotateButtons[i].Selected:=false;
  (Sender as TBDButton).Selected:=true;
end;

procedure TBDRotateCELDialog.OKButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  if fRotateButtons[0].Selected then
    MessageQueue.AddMessage(MSG_ROTATECELRESP,1)
  else if fRotateButtons[1].Selected then
    MessageQueue.AddMessage(MSG_ROTATECELRESP,2)
  else if fRotateButtons[2].Selected then
    MessageQueue.AddMessage(MSG_ROTATECELRESP,3);
end;

{ TBDAboutDialog }

constructor TBDAboutDialog.Create;
var atmB:TBDButton;
begin
  inherited Create(SPLASHSCREENWIDTH,SPLASHSCREENHEIGHT);
  fName:='SplashScreen';
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height,SystemPalette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,SystemPalette[3]);
  MM.Fonts['LogoFont'].OutText(fTexture.ARGBImage,'BURDoCK PAINT',80,24,0);
  MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,'CODE: GILBY/MKSZTSZ',80,56,0);
  MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,'LICENSED UNDER THE UNLICENSE',80,88,0);
  MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,'HUNGARY - 2023',80,120,0);
  MM.Images.ItemByName['Burdock'].CopyTo(0,0,46,52,16,(SPLASHSCREENHEIGHT-52) div 2,fTexture.ARGBImage,true);
  fTexture.Update;
  atmB:=TBDButton.Create(
    fWindowLeft+SPLASHSCREENWIDTH-NORMALBUTTONWIDTH-6-16,
    fWindowTop+SPLASHSCREENHEIGHT-44,
    NORMALBUTTONWIDTH,
    'OK','CLOSE DIALOG',TMessage.Init(MSG_ABOUTRESP,0));
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);
  Visible:=false;
  MouseObjects.Add(Self);
end;

{ TBDMessageBox }

constructor TBDMessageBox.Create(iMessage:string; iButtons:string);
var atmB:TBDButton;i,x,buttoncount:integer;
begin
  if length(iButtons)=0 then iButtons:='OK';
  if iButtons[length(iButtons)]<>';' then iButtons+=';';
  buttoncount:=CountChar(';',iButtons);
  inherited Create(max((length(iMessage)+2)*18,buttoncount*(NORMALBUTTONWIDTH+18)),MESSAGEBOXHEIGHT);
  fName:='MessageBox';
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height,SystemPalette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,SystemPalette[3]);
  MM.Fonts['Black'].OutText(fTexture.ARGBImage,iMessage,fTexture.Width div 2,24,1);
  fTexture.Update;
  ZIndex:=MODALDIALOG_ZINDEX;
  MouseObjects.Add(Self);
  x:=(fTexture.Width div 2)-(buttoncount*(NORMALBUTTONWIDTH+18)) div 2;
  i:=0;
  while length(iButtons)>0 do begin
    atmB:=TBDButton.Create(
      fWindowLeft+x,
      fWindowTop+MESSAGEBOXHEIGHT-44,
      NORMALBUTTONWIDTH,
//      'OKX','CLOSE DIALOG',TMessage.Init(MSG_MESSAGEBOXRESP,i));
      copy(iButtons,1,pos(';',iButtons)-1),'',TMessage.Init(MSG_MESSAGEBOXRESP,i));
    atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
    AddChild(atmB);
    delete(iButtons,1,pos(';',iButtons));
    inc(i);
    inc(x,NORMALBUTTONWIDTH+18);
  end;
  Visible:=true;
end;

function TBDMessageBox.Run:integer;
var msg:TMessage;
begin
//  MouseObjects.List;
  MouseObjects.ListVisible;
  Result:=-1;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,48,12,24,255);
    SDL_RenderClear(PrimaryWindow.Renderer);
    MouseObjects.Draw;
    FlipNoLimit;
    HandleMessages;
    while MessageQueue.HasNewMessage do begin
      msg:=MessageQueue.GetNextMessage;
      case msg.TypeID of
        MSG_MESSAGEBOXRESP:Result:=msg.DataInt;
      end;
    end;
  until Result<>-1;
end;

end.

