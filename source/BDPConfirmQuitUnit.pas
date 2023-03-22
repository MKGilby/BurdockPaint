unit BDPConfirmQuitUnit;

{$mode Delphi}

interface

uses
  BDPModalDialogUnit;

type

  { TConfirmQuitDialog }

  TConfirmQuitDialog=class(TModalDialog)
    constructor Create;
    function KeyDown(Sender:TObject;key:integer):boolean;
  end;

implementation

uses
  SysUtils, BDPSharedUnit, BDPButtonUnit, BDPMessageUnit, BDPKeyMappingUnit;

const
  QUITDIALOGWIDTH=480;
  QUITDIALOGHEIGHT=96;

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

end.

