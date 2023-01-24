unit BDPControlsUnit;

{$mode Delphi}{$H+}

interface

uses
  vcc_Container2, BDPButtonUnit, mk_sdl2;

type

  { TBDControls }

  TBDControls=class(TContainer)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure ActivateToolButton(index:integer);
    procedure ActivateInkButton(index:integer);
//    procedure ToggleFilledButton;
    function MouseEnter(Sender:TObject;{%H-}x,{%H-}y:integer):boolean;
    function MouseLeave(Sender:TObject;{%H-}x,{%H-}y:integer):boolean;
    function MouseMove(Sender:TObject;{%H-}x,{%H-}y:integer):boolean;
    function Click(Sender:TObject;{%H-}x, {%H-}y, {%H-}buttons: integer):boolean;
    function FilledButtonClick(Sender:TObject;{%H-}x, {%H-}y, {%H-}buttons: integer):boolean;
//    function ToolEditorActivatorClick(Sender:TObject;{%H-}x, {%H-}y, {%H-}buttons: integer):boolean;
  private
    fTexture:TStreamingTexture;
    fToolButtons:array[0..5] of TBDButton;
    fInkButtons:array[0..5] of TBDButton;
    fFilledButton:TBDButton;
//    fToolEditorActivator:TBDInvisibleButton;
  end;

implementation

uses SysUtils, BDPSharedUnit, sdl2, BDPToolsUnit, BDPInksUnit, BDPMessageUnit;

{ TBDControls }

constructor TBDControls.Create;
var i:integer;atmT:TBDTool;atmI:TBDInk;msg:TMessage;
begin
  inherited Create;
  fLeft:=0;
  fTop:=WINDOWHEIGHT-CONTROLSHEIGHT;
  fWidth:=WINDOWWIDTH;
  fHeight:=CONTROLSHEIGHT;
  fTexture:=TStreamingTexture.Create(WINDOWWIDTH,CONTROLSHEIGHT);
  fTexture.ARGBImage.Clear;

  msg.TypeID:=MSG_ACTIVATETOOL;
  msg.DataString:='';
  for i:=0 to 5 do begin
    atmT:=Tools[Tools.IndexOf(Settings.SelectedTools[i])];
    if atmT=nil then raise Exception.Create('Tool not found! ('+Settings.SelectedTools[i]+')');
    msg.DataInt:=i;
    fToolButtons[i]:=TBDButton.Create(
      fTexture.ARGBImage,
      fLeft+TOOLBUTTONSLEFT+i mod 2*130,
      fTop+TOOLBUTTONSTOP+i div 2*30,
      NORMALBUTTONWIDTH,
      atmT.Name,
      atmT.Hint,
      msg,
      atmT);
    fToolButtons[i].ParentX:=fLeft;
    fToolButtons[i].ParentY:=fTop;
    fToolButtons[i].ZIndex:=15;
    AddChild(fToolButtons[i]);
  end;
  ActivateToolButton(Settings.ActiveTool);

  msg.TypeID:=MSG_ACTIVATEINK;
  msg.DataString:='';
  for i:=0 to 5 do begin
    atmi:=Inks[Inks.IndexOf(Settings.SelectedInks[i])];
    if atmT=nil then raise Exception.Create('Ink not found! ('+Settings.SelectedInks[i]+')');
    msg.DataInt:=i;
    fInkButtons[i]:=TBDButton.Create(
      fTexture.ARGBImage,
      fLeft+InkButtonsLeft+i mod 2*130,
      fTop+InkButtonsTop+i div 2*30,
      NORMALBUTTONWIDTH,
      atmI.Name,
      atmI.Hint,
      msg,
      atmI);
    fInkButtons[i].ParentX:=fLeft;
    fInkButtons[i].ParentY:=fTop;
    fInkButtons[i].ZIndex:=15;
    AddChild(fInkButtons[i]);
  end;
  ActivateInkButton(Settings.ActiveInk);

{  fFilledButton:=TBDButton.Create(
    fTexture.ARGBImage,
    fLeft+3,
    fTop+6,
    NORMALBUTTONWIDTH,
    'TEST',
    '',
    'This is a test button');
  fFilledButton.Selected:=false;
  fFilledButton.OnClick:=FilledButtonClick;
  fFilledButton.ParentX:=fLeft;
  fFilledButton.ParentY:=fTop;
  fFilledButton.ZIndex:=15;
  AddChild(fFilledButton);}
  fVisible:=true;
  OnMouseEnter:=MouseEnter;
  OnMouseLeave:=MouseLeave;
  OnMouseMove:=MouseMove;
  OnClick:=Click;
  fName:='Controls';
end;

destructor TBDControls.Destroy;
begin
//  if Assigned(fFilledButton) then FreeAndNil(fFilledButton);
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TBDControls.Draw;
begin
  if fVisible then begin
    fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,OverlayImage.Palette[2]);
    fTexture.ARGBImage.Bar(0,3,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height-3,OverlayImage.Palette[3]);
    inherited Draw;
    fTexture.Update;
    PutTexture(fLeft,fTop,fTexture);
  end;
end;

procedure TBDControls.ActivateToolButton(index:integer);
var i:integer;
begin
  if (index>=0) and (index<6) then begin
    Settings.ActiveTool:=index;
    for i:=0 to length(fToolButtons)-1 do
      if i<>index then begin
        fToolButtons[i].Selected:=false;
      end else begin
        fToolButtons[i].Selected:=true;
        ActiveTool:=TBDTool(fToolButtons[i].AssignedObject);
      end;
  end else
    raise Exception.Create(Format('ActivateToolButton: Index out of range! (%d)',[index]));
end;

procedure TBDControls.ActivateInkButton(index:integer);
var i:integer;
begin
  if (index>=0) and (index<6) then begin
    Settings.ActiveInk:=index;
    for i:=0 to length(fInkButtons)-1 do
      if i<>index then begin
        fInkButtons[i].Selected:=false;
      end else begin
        fInkButtons[i].Selected:=true;
        ActiveInk:=TBDInk(fInkButtons[i].AssignedObject);
    end;
  end else
    raise Exception.Create(Format('ActivateInkButton: Index out of range! (%d)',[index]));
end;

function TBDControls.MouseEnter(Sender:TObject; x,y:integer):boolean;
begin
  if fVisible then SDL_ShowCursor(SDL_ENABLE);
  Result:=false;
end;

function TBDControls.MouseLeave(Sender:TObject; x,y:integer):boolean;
begin
  if fVisible then SDL_ShowCursor(SDL_DISABLE);
  Result:=false;
end;

function TBDControls.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  Result:=true;
end;

function TBDControls.Click(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TBDControls.FilledButtonClick(Sender:TObject; x,y,buttons:integer
  ):boolean;
begin
  if Sender is TBDButton then with Sender as TBDButton do
    fSelected:=not fSelected;
  Result:=true;
end;

end.

