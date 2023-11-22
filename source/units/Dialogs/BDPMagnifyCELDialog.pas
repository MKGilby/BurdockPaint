{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPMagnifyCELDialog;

{$mode Delphi}

interface

uses
  SysUtils, BDPButton, BDPModalDialog;

type

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

implementation

uses BDPShared, MKMouse2, SDL2;

const
  MAGNIFYDIALOGWIDTH=480;
  MAGNIFYDIALOGHEIGHT=MODALDIALOGCAPTIONHEIGHT+3+3*9+2*NORMALBUTTONHEIGHT;
  XBUTTONWIDTH=63;

{ TBDMagnifyCELDialog }

constructor TBDMagnifyCELDialog.Create;
const MAGNIFIES:array[0..2] of integer=(2,3,5);
var atmb:TBDButton;i:integer;
begin
  inherited Create(MAGNIFYDIALOGWIDTH,MAGNIFYDIALOGHEIGHT);
  Caption:='MAGNIFY CEL';
  for i:=0 to 2 do begin
    fMagnifyButtons[i]:=TBDButton.Create(
      fLeft+((MAGNIFYDIALOGWIDTH div 4)*(i+1)-XBUTTONWIDTH div 2),
      fTop+((MODALDIALOGCAPTIONHEIGHT+9)),
      XBUTTONWIDTH, NORMALBUTTONHEIGHT,
      inttostr(MAGNIFIES[i])+'X','');
    fMagnifyButtons[i].ZIndex:=MODALDIALOG_ZINDEX+1;
    fMagnifyButtons[i].Tag:=i;
    fMagnifyButtons[i].OnClick:=MagnifyButtonClick;
    if i=0 then fMagnifyButtons[i].Selected:=true;
    AddChild(fMagnifyButtons[i]);
  end;
  atmB:=TBDButton.Create(
    fLeft+(MAGNIFYDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2),
    fTop+(MODALDIALOGCAPTIONHEIGHT+2*9+NORMALBUTTONHEIGHT),
    NORMALBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'OK','MAGNIFY CEL');
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  atmB.OnClick:=OKButtonClick;
  AddChild(atmB);
  atmB:=TBDButton.Create(
    fLeft+((MAGNIFYDIALOGWIDTH div 3*2)-NORMALBUTTONWIDTH div 2),
    fTop+(MODALDIALOGCAPTIONHEIGHT+2*9+NORMALBUTTONHEIGHT),
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

end.

