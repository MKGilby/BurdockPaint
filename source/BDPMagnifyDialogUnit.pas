unit BDPMagnifyDialogUnit;

{$mode Delphi}

interface

uses vcc2_Container, mk_sdl2, BDPButtonUnit;

type

  { TMagnifyCELDialog }

  TMagnifyCELDialog=class(TContainer)
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
    function MagnifyButtonClick(Sender:TObject;x,y,buttons:integer):boolean;
    function OKButtonClick(Sender:TObject;x,y,buttons:integer):boolean;
  private
    fTexture:TStreamingTexture;
    fMagnifyButtons:array[0..2] of TBDButton;
    fWindowLeft,fWindowTop:integer;
  end;

implementation

uses SysUtils, BDPSharedUnit, BDPMessageUnit, BDPKeyMappingUnit,
  SDL2;

const
  MAGNIFYDIALOGWIDTH=480;
  MAGNIFYDIALOGHEIGHT=128;
  XBUTTONWIDTH=63;

{ TMagnifyCELDialog }

constructor TMagnifyCELDialog.Create;
const MAGNIFIES:array[0..2] of integer=(2,3,5);
var atmb:TBDButton;msg:TMessage;i:integer;
begin
  inherited Create;
  SetBoundsWH(0,0,WINDOWWIDTH,WINDOWHEIGHT);
  fName:='Magnify CEL';
  fWindowLeft:=(WINDOWWIDTH-MAGNIFYDIALOGWIDTH) div 2;
  fWindowTop:=(WINDOWHEIGHT-MAGNIFYDIALOGHEIGHT) div 2;
  fTexture:=TStreamingTexture.Create(MAGNIFYDIALOGWIDTH,MAGNIFYDIALOGHEIGHT);
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
    fMagnifyButtons[i].ZIndex:=MaxLongint;
    fMagnifyButtons[i].Tag:=i;
    fMagnifyButtons[i].OnClick:=MagnifyButtonClick;
    if i=0 then fMagnifyButtons[i].Selected:=true;
    AddChild(fMagnifyButtons[i]);
  end;
  atmB:=TBDButton.Create(
    fWindowLeft+MAGNIFYDIALOGWIDTH div 4-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH,
    'OK','MAGNIFY CEL',msg);
  atmb.ZIndex:=MaxLongint;
  atmb.OnClick:=OKButtonClick;
  AddChild(atmB);
  msg.TypeID:=MSG_MAGNIFYCEL;
  msg.DataInt:=0;
  atmB:=TBDButton.Create(
    fWindowLeft+(MAGNIFYDIALOGWIDTH div 4*3)-NORMALBUTTONWIDTH div 2,
    fWindowTop+84,
    NORMALBUTTONWIDTH,
    'CANCEL','DON''T MAGNIFY CEL',msg);
  atmb.ZIndex:=MaxLongint;
  AddChild(atmB);
  OnKeyDown:=KeyDown;
end;

destructor TMagnifyCELDialog.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TMagnifyCELDialog.Draw;
begin
  if fVisible then
    PutTexture(fWindowLeft,fWindowTop,fTexture);
end;

function TMagnifyCELDialog.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  Result:=true;
end;

function TMagnifyCELDialog.Click(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TMagnifyCELDialog.MouseDown(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TMagnifyCELDialog.MouseUp(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TMagnifyCELDialog.MouseWheel(Sender:TObject; x,y,wheelx,wheely:integer):boolean;
begin
  Result:=true;
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

function TMagnifyCELDialog.KeyUp(Sender:TObject; key:integer):boolean;
begin
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

end.

