{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPRotateCELDialog;

{$mode Delphi}

interface

uses
  SysUtils, BDPButton, BDPModalDialog;

type

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

implementation

uses BDPShared, MKMouse2, sdl2;

const
  ROTATEDIALOGWIDTH=512;
  ROTATEDIALOGHEIGHT=MODALDIALOGCAPTIONHEIGHT+3+3*9+2*NORMALBUTTONHEIGHT;

{ TBDRotateCELDialog }

constructor TBDRotateCELDialog.Create;
const ROTATES:array[0..2] of integer=(90,180,270);
var atmb:TBDButton;i:integer;
begin
  inherited Create(ROTATEDIALOGWIDTH,ROTATEDIALOGHEIGHT);
  fName:='Rotate CEL';
  Caption:='ROTATE CEL';
  for i:=0 to 2 do begin
    fRotateButtons[i]:=TBDButton.Create(
      fLeft+(ROTATEDIALOGWIDTH div 4)*(i+1)-NORMALBUTTONWIDTH div 2,
      fTop+((MODALDIALOGCAPTIONHEIGHT+9)),
      NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
      inttostr(ROTATES[i])+'°','');
    fRotateButtons[i].ZIndex:=MODALDIALOG_ZINDEX+1;
    fRotateButtons[i].Tag:=i;
    fRotateButtons[i].OnClick:=RotateButtonClick;
    if i=0 then fRotateButtons[i].Selected:=true;
    AddChild(fRotateButtons[i]);
  end;
  atmB:=TBDButton.Create(
    fLeft+ROTATEDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2,
    fTop+(MODALDIALOGCAPTIONHEIGHT+2*9+NORMALBUTTONHEIGHT),
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'OK','ROTATE CEL');
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  atmB.OnClick:=OKButtonClick;
  AddChild(atmB);
  atmB:=TBDButton.Create(
    fLeft+(ROTATEDIALOGWIDTH div 3*2)-NORMALBUTTONWIDTH div 2,
    fTop+(MODALDIALOGCAPTIONHEIGHT+2*9+NORMALBUTTONHEIGHT),
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

end.

