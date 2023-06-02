{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of BurdockPaint.

  BurdockPaint is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  BurdockPaint is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  BurdockPaint. If not, see <https://www.gnu.org/licenses/>.
}

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

  { TBDMagnifyCELDialog }

  TBDMagnifyCELDialog=class(TBDModalDialog)
    constructor Create;
    function KeyDown(Sender:TObject;key:integer):boolean;
    procedure MagnifyButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure CancelButtonClick(Sender:TObject;x,y,buttons:integer);
  private
    fMagnifyButtons:array[0..2] of TBDButton;
  end;

  { TBDRotateCELDialog }

  TBDRotateCELDialog=class(TBDModalDialog)
    constructor Create;
    function KeyDown(Sender:TObject;key:integer):boolean;
    procedure RotateButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure CancelButtonClick(Sender:TObject;x,y,buttons:integer);
  private
    fRotateButtons:array[0..2] of TBDButton;
  end;

  { TBDAboutDialog }

  TBDAboutDialog=class(TBDModalDialog)
    constructor Create;
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
  end;

  { TBDMessageBox }

  TBDMessageBox=class(TBDModalDialog)
    constructor Create(iMessage:string;iButtons:string='OK');
    function Run:integer;
    function KeyDown(Sender:TObject;key:integer):boolean;
  end;

  { TBDDitherDialog }

  TBDDitherDialog=class(TBDModalDialog)
    constructor Create;
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure CancelButtonClick(Sender:TObject;x,y,buttons:integer);
  private
    fSlider:TBDHorizontalSlider;
  end;

  { TBDSelectColorClusterDialog }

  TBDSelectColorClusterDialog=class(TBDModalDialog)
    constructor Create;
    procedure Refresh;
    procedure Click(Sender:TObject;x,y,buttons:integer);
    procedure AddButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure ColorClusterClick(Sender:TObject;x,y,buttons:integer);
    procedure SetPositionAndWidth(pLeft,pBottom,pWidth:integer);
  private
    fBottom: integer;
    fWindowWidth:integer;
    procedure fSetWindowLeft(value:integer);
    procedure fSetWindowWidth(value:integer);
    procedure fSetBottom(value:integer);
  public
    property WindowLeft:integer write fSetWindowLeft;
    property WindowWidth:integer read fWindowWidth write fSetWindowWidth;
    property Bottom:integer write fSetBottom;
  end;

  { TBDConfigureRGradDialog }

  TBDConfigureRGradDialog=class(TBDModalDialog)
    constructor Create;
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure CenterButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure CancelButtonClick(Sender:TObject;x,y,buttons:integer);
  private
    fRepetitionSlider:TBDHorizontalSlider;
    fRotationSlider:TBDHorizontalSlider;
    procedure Show(Sender:TObject);
  end;

function MessageBox(iMessage:string;iButtons:string='OK'):integer;

implementation

uses
  sdl2, BDPShared, BDPMessage, BDPKeyMapping, MKMouse2, MKToolbox,
  BDPColorCluster, Logger;

const
  MAGNIFYDIALOGWIDTH=480;
  MAGNIFYDIALOGHEIGHT=128;
  XBUTTONWIDTH=63;
  ROTATEDIALOGWIDTH=512;
  ROTATEDIALOGHEIGHT=128;
  SPLASHSCREENWIDTH=640;
  SPLASHSCREENHEIGHT=160;
  MESSAGEBOXHEIGHT=96;
  DITHERDIALOGWIDTH=480;
  DITHERDIALOGHEIGHT=144;
  COLORCLUSTERDIALOGWIDTH=COLORCLUSTERWIDTH;
  CONFIGURERGRADDIALOGWIDTH=480;
  CONFIGURERGRADDIALOGHEIGHT=256;

function MessageBox(iMessage:string; iButtons:string):integer;
var MessageBox:TBDMessageBox;j:integer;
begin
  MessageBox:=TBDMessageBox.Create(uppercase(iMessage),uppercase(iButtons));
  try
    Result:=MessageBox.Run;
    j:=MouseObjects.IndexOf(MessageBox);
    if j>-1 then MouseObjects.Delete(j);
  finally
    MessageBox.Free;
  end;
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

{ TBDMagnifyCELDialog }

constructor TBDMagnifyCELDialog.Create;
const MAGNIFIES:array[0..2] of integer=(2,3,5);
var atmb:TBDButton;i:integer;
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
  for i:=0 to 2 do begin
    fMagnifyButtons[i]:=TBDButton.Create(
      fWindowLeft+(MAGNIFYDIALOGWIDTH div 4)*(i+1)-XBUTTONWIDTH div 2,
      fWindowTop+48,
      XBUTTONWIDTH, NORMALBUTTONHEIGHT,
      inttostr(MAGNIFIES[i])+'X','');
    fMagnifyButtons[i].ZIndex:=MODALDIALOG_ZINDEX+1;
    fMagnifyButtons[i].Tag:=i;
    fMagnifyButtons[i].OnClick:=MagnifyButtonClick;
    if i=0 then fMagnifyButtons[i].Selected:=true;
    AddChild(fMagnifyButtons[i]);
  end;
  atmB:=TBDButton.Create(
    fWindowLeft+MAGNIFYDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'OK','MAGNIFY CEL');
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  atmB.OnClick:=OKButtonClick;
  AddChild(atmB);
  atmB:=TBDButton.Create(
    fWindowLeft+(MAGNIFYDIALOGWIDTH div 3*2)-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'CANCEL','DON''T MAGNIFY CEL');
  atmB.OnClick:=CancelButtonClick;
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
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
    Self.Hide;
  end else
  if key=SDL_SCANCODE_RETURN then begin
    if fMagnifyButtons[0].Selected then Project.CELImage.Magnify(2)
    else if fMagnifyButtons[1].Selected then Project.CELImage.Magnify(3)
    else if fMagnifyButtons[2].Selected then Project.CELImage.Magnify(5);
    MessageQueue.AddMessage(MSG_SHOWCEL);
    Self.Hide;
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
  if fMagnifyButtons[0].Selected then Project.CELImage.Magnify(2)
  else if fMagnifyButtons[1].Selected then Project.CELImage.Magnify(3)
  else if fMagnifyButtons[2].Selected then Project.CELImage.Magnify(5);
  MessageQueue.AddMessage(MSG_SHOWCEL);
  Self.Hide;
end;

procedure TBDMagnifyCELDialog.CancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Self.Hide;
end;

{ TBDRotateCELDialog }

constructor TBDRotateCELDialog.Create;
const ROTATES:array[0..2] of integer=(90,180,270);
var atmb:TBDButton;i:integer;
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
  for i:=0 to 2 do begin
    fRotateButtons[i]:=TBDButton.Create(
      fWindowLeft+(ROTATEDIALOGWIDTH div 4)*(i+1)-NORMALBUTTONWIDTH div 2,
      fWindowTop+48,
      NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
      inttostr(ROTATES[i])+'°','');
    fRotateButtons[i].ZIndex:=MODALDIALOG_ZINDEX+1;
    fRotateButtons[i].Tag:=i;
    fRotateButtons[i].OnClick:=RotateButtonClick;
    if i=0 then fRotateButtons[i].Selected:=true;
    AddChild(fRotateButtons[i]);
  end;
  atmB:=TBDButton.Create(
    fWindowLeft+ROTATEDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'OK','ROTATE CEL');
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  atmB.OnClick:=OKButtonClick;
  AddChild(atmB);
  atmB:=TBDButton.Create(
    fWindowLeft+(ROTATEDIALOGWIDTH div 3*2)-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'CANCEL','DON''T ROTATE CEL');
  atmB.OnClick:=CancelButtonClick;
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
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
    Self.Hide;
  end else
  if key=SDL_SCANCODE_RETURN then begin
    if fRotateButtons[0].Selected then Project.CELImage.Rotate(1)
    else if fRotateButtons[1].Selected then Project.CELImage.Rotate(2)
    else if fRotateButtons[2].Selected then Project.CELImage.Rotate(3);
    MessageQueue.AddMessage(MSG_SHOWCEL);
    Self.Hide;
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
  if fRotateButtons[0].Selected then Project.CELImage.Rotate(1)
  else if fRotateButtons[1].Selected then Project.CELImage.Rotate(2)
  else if fRotateButtons[2].Selected then Project.CELImage.Rotate(3);
  MessageQueue.AddMessage(MSG_SHOWCEL);
  Self.Hide;
end;

procedure TBDRotateCELDialog.CancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Self.Hide;
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
  MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,'LICENSED UNDER GNU GPL 3',80,88,0);
  MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,'COPYRIGHT 2023 MKSZTSZ',80,120,0);
  MM.Images.ItemByName['Burdock'].CopyTo(0,0,46,52,16,(SPLASHSCREENHEIGHT-52) div 2,fTexture.ARGBImage,true);
  fTexture.Update;
  atmB:=TBDButton.Create(
    fWindowLeft+SPLASHSCREENWIDTH-NORMALBUTTONWIDTH-6-16,
    fWindowTop+SPLASHSCREENHEIGHT-44,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'OK','CLOSE DIALOG');
  atmB.OnClick:=OKButtonClick;
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);
  Visible:=false;
  MouseObjects.Add(Self);
end;

procedure TBDAboutDialog.OKButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Self.Hide;
end;

{ TBDMessageBox }

constructor TBDMessageBox.Create(iMessage:string; iButtons:string);
var atmB:TBDButton;i,x,buttoncount,key:integer;s:string;
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
  x:=(fTexture.Width-(buttoncount*(NORMALBUTTONWIDTH+18))+18) div 2;
  i:=0;
  while length(iButtons)>0 do begin
    key:=-1;
    s:=copy(iButtons,1,pos(';',iButtons)-1);
    if (pos('^',s)>0) and (pos('^',s)<length(s)) then begin
      key:=SDL_SCANCODE_A+ord(s[pos('^',s)+1])-65;
      delete(s,pos('^',s),1);
    end;
    atmB:=TBDButton.Create(
      fWindowLeft+x,
      fWindowTop+MESSAGEBOXHEIGHT-44,
      NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
      s,'');
    atmB.Message:=TMessage.Init(MSG_MESSAGEBOXRESP,i);
    atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
    atmB.Tag:=key;
    atmB.OnKeyDown:=KeyDown;
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

function TBDMessageBox.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=false;
  if (Sender is TBDButton) then begin
    if key=TBDButton(Sender).Tag then begin
      MessageQueue.AddMessage(TBDButton(Sender).Message);
      Result:=true;
    end;
  end;
end;

{ TBDDitherDialog }

constructor TBDDitherDialog.Create;
const SLIDERWIDTH=300;
      SLIDERHEIGHT=33;
var atmB:TBDButton;
begin
  inherited Create(DITHERDIALOGWIDTH,DITHERDIALOGHEIGHT);
  fName:='DitherDialog';
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height,SystemPalette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,SystemPalette[3]);
  MM.Fonts['Black'].OutText(fTexture.ARGBImage,'DITHER STRENGTH',DITHERDIALOGWIDTH div 2,16,1);
  fTexture.Update;

  fSlider:=TBDHorizontalSlider.Create(
    fWindowLeft+(DITHERDIALOGWIDTH-SLIDERWIDTH) div 2, fWindowTop+48, SLIDERWIDTH, SLIDERHEIGHT);
  fSlider.ZIndex:=MODALDIALOG_ZINDEX+1;
  fSlider.Name:='DitherSlider';
  fSlider.MinValue:=1;
  fSlider.MaxValue:=100;
  fSlider.Position:=Settings.DitherStrength;
  AddChild(fSlider);

  atmB:=TBDButton.Create(
    fWindowLeft+(DITHERDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2),
    fWindowTop+96,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'OK','APPLY DITHER STRENGTH');
  atmb.OnClick:=OKButtonClick;
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);

  atmB:=TBDButton.Create(
    fWindowLeft+(DITHERDIALOGWIDTH div 3*2-NORMALBUTTONWIDTH div 2),
    fWindowTop+96,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'CANCEL','CLOSE DIALOG');
  atmb.OnClick:=CancelButtonClick;
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);


  Visible:=false;
  MouseObjects.Add(Self);
end;

procedure TBDDitherDialog.OKButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Settings.DitherStrength:=fSlider.Position;
  Self.Hide;
end;

procedure TBDDitherDialog.CancelButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Self.Hide;
end;

{ TBDSelectColorClusterDialog }

constructor TBDSelectColorClusterDialog.Create;
begin
  inherited Create(COLORCLUSTERDIALOGWIDTH,36*(Project.CurrentImage.ColorClusters.Count+1)+3);
  fWindowWidth:=COLORCLUSTERDIALOGWIDTH;
  fName:='SelectColorClusterDialog';
  Visible:=false;
  MouseObjects.Add(Self);
  Refresh;
  OnClick:=CLick;
end;

procedure TBDSelectColorClusterDialog.Refresh;
var y,i:integer;atmC:TBDSimpleColorCluster;atmB:TBDButton;fNeedAddButton:boolean;
begin
  ClearChildren;
  fTexture.Free;
  fNeedAddButton:=Project.CurrentImage.ColorClusters.Count<16;
  if fNeedAddButton then begin
    fTexture:=TStreamingTexture.Create(fWindowWidth,36*(Project.CurrentImage.ColorClusters.Count+1)+3);
    y:=36;
  end else begin
    fTexture:=TStreamingTexture.Create(fWindowWidth,36*(Project.CurrentImage.ColorClusters.Count)+9);
    y:=6;
  end;
  fWindowTop:=fBottom-fTexture.Height;
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height,SystemPalette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,SystemPalette[3]);
  fTexture.Update;
  Log.Trace('WINDOWTOP='+inttostr(fWindowTop));
  if fNeedAddButton then begin
    atmB:=TBDButton.Create(fWindowLeft+(fTexture.Width-NORMALBUTTONWIDTH) div 2,fWindowTop+6,
      NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,'ADD','');
    atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
    atmB.Name:='ADD_CC';
    atmB.OnClick:=AddButtonClick;
    AddChild(atmB);
  end;
  for i:=Project.CurrentImage.ColorClusters.Count-1 downto 0 do begin
    atmC:=TBDSimpleColorCluster.Create(fWindowLeft+6,fWindowTop+y+(Project.CurrentImage.ColorClusters.Count-1-i)*36,Project.CurrentImage.ColorClusters[i]);
    atmC.Width:=fTexture.Width-12;
    atmC.Height:=COLORCLUSTERHEIGHT-3;
    atmC.ZIndex:=MODALDIALOG_ZINDEX+1;
    atmC.Tag:=i;
    atmC.OnClick:=ColorClusterClick;
    atmC.Selected:=(i=Project.CurrentImage.ColorClusters.ActiveIndex);
    atmC.Name:=Format('SimpleColorCluster %d',[i]);
    AddChild(atmC);
  end;
end;

procedure TBDSelectColorClusterDialog.Click(Sender:TObject; x,y,buttons:integer);
begin
  if (x<fWindowLeft) or (x>=fWindowLeft+fTexture.Width) or (y<fWindowTop) or (y>fWindowTop+fTexture.Height) then
    MessageQueue.AddMessage(MSG_COLORCLUSTERDIALOGRESP);
end;

procedure TBDSelectColorClusterDialog.AddButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Project.CurrentImage.ColorClusters.Add(TColorCluster.Create(0,15));
  fWindowTop-=COLORCLUSTERHEIGHT;
  Refresh;
end;

procedure TBDSelectColorClusterDialog.ColorClusterClick(Sender:TObject; x,y,buttons:integer);
var cc:TBDSimpleColorCluster;
begin
  cc:=Sender as TBDSimpleColorCluster;
  x-=cc.Left;
  if x<cc.ColorsWidth then
    MessageQueue.AddMessage(MSG_COLORCLUSTERDIALOGRESP,cc.Tag)
  else begin
    if not cc.Selected then begin
      Project.CurrentImage.ColorClusters.Delete(cc.Tag);
      if Project.CurrentImage.ColorClusters.ActiveIndex>cc.Tag then
        Project.CurrentImage.ColorClusters.ActiveIndex:=Project.CurrentImage.ColorClusters.ActiveIndex-1;
      fWindowTop+=COLORCLUSTERHEIGHT;
      Refresh;
    end;
  end;
end;

procedure TBDSelectColorClusterDialog.SetPositionAndWidth(pLeft,pBottom,pWidth:integer);
begin
  fWindowLeft:=pLeft;
  fWindowWidth:=pWidth;
  fBottom:=pBottom;
  Refresh;
end;

procedure TBDSelectColorClusterDialog.fSetWindowLeft(value:integer);
begin
  fWindowLeft:=value;
  Refresh;
end;

procedure TBDSelectColorClusterDialog.fSetWindowWidth(value:integer);
begin
  fWindowWidth:=value;
  Refresh;
end;

procedure TBDSelectColorClusterDialog.fSetBottom(value: integer);
begin
  fBottom:=value;
  Refresh;
end;

{ TBDConfigureRGradDialog }

constructor TBDConfigureRGradDialog.Create;
const SLIDERWIDTH=300;
      SLIDERHEIGHT=33;
var atmB:TBDButton;
begin
  inherited Create(CONFIGURERGRADDIALOGWIDTH,CONFIGURERGRADDIALOGHEIGHT);
  fName:='ConfigureRGradDialog';
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height,SystemPalette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,SystemPalette[3]);
  MM.Fonts['Black'].OutText(fTexture.ARGBImage,'REPETITIONS',DITHERDIALOGWIDTH div 2,16,1);
  MM.Fonts['Black'].OutText(fTexture.ARGBImage,'ROTATION °',DITHERDIALOGWIDTH div 2,96,1);
  fTexture.Update;

  fRepetitionSlider:=TBDHorizontalSlider.Create(
    fWindowLeft+(DITHERDIALOGWIDTH-SLIDERWIDTH) div 2, fWindowTop+48, SLIDERWIDTH, SLIDERHEIGHT);
  fRepetitionSlider.ZIndex:=MODALDIALOG_ZINDEX+1;
  fRepetitionSlider.Name:='RGradRepSlider';
  fRepetitionSlider.MinValue:=1;
  fRepetitionSlider.MaxValue:=16;
  fRepetitionSlider.Position:=Settings.RGradRepetitions;
  AddChild(fRepetitionSlider);

  fRotationSlider:=TBDHorizontalSlider.Create(
    fWindowLeft+(DITHERDIALOGWIDTH-SLIDERWIDTH) div 2, fWindowTop+128, SLIDERWIDTH, SLIDERHEIGHT);
  fRotationSlider.ZIndex:=MODALDIALOG_ZINDEX+1;
  fRotationSlider.Name:='RGradRotSlider';
  fRotationSlider.MinValue:=0;
  fRotationSlider.MaxValue:=359;
  fRotationSlider.Position:=Settings.RGradRotation;
  AddChild(fRotationSlider);

  atmB:=TBDButton.Create(
    fWindowLeft+(DITHERDIALOGWIDTH div 2-NORMALBUTTONWIDTH div 2),
    fWindowTop+176,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'CENTER','SET RADIAL GRADIENT CENTER POINT');
  atmb.OnClick:=CenterButtonClick;
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);

  atmB:=TBDButton.Create(
    fWindowLeft+(DITHERDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2),
    fWindowTop+216,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'OK','APPLY VALUES');
  atmb.OnClick:=OKButtonClick;
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);

  atmB:=TBDButton.Create(
    fWindowLeft+(DITHERDIALOGWIDTH div 3*2-NORMALBUTTONWIDTH div 2),
    fWindowTop+216,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'CANCEL','CLOSE DIALOG');
  atmb.OnClick:=CancelButtonClick;
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);

  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);
end;

procedure TBDConfigureRGradDialog.OKButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Settings.RGradRepetitions:=fRepetitionSlider.Position;
  Settings.RGradRotation:=fRotationSlider.Position;
  Self.Hide;
end;

procedure TBDConfigureRGradDialog.CenterButtonClick(Sender:TObject;x,y,buttons:integer);
begin

end;

procedure TBDConfigureRGradDialog.CancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Self.Hide;
end;

procedure TBDConfigureRGradDialog.Show(Sender:TObject);
begin
  fRotationSlider.Position:=Settings.RGradRotation;
  fRepetitionSlider.Position:=Settings.RGradRepetitions;
end;

end.

