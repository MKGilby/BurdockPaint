unit BDPControlsUnit;

{$mode Delphi}{$H+}

interface

uses
  vcc2_Container, BDPButtonUnit, mk_sdl2, BDPMessageUnit, BDPColorSelectorUnit;

type

  { TBDControls }

  TBDControls=class(TContainer)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure ActivateToolButton(index:integer);
    procedure ActivateInkButton(index:integer);
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    function Click(Sender:TObject;x,y,buttons: integer):boolean;
    function FilledButtonClick(Sender:TObject;x,y,buttons: integer):boolean;
    function ClearKeyColorButtonClick(Sender:TObject;x,y,buttons: integer):boolean;
    function ToolButtonClick(Sender:TObject;x,y,buttons: integer):boolean;
    function InkButtonClick(Sender:TObject;x,y,buttons: integer):boolean;
    function UndoButtonClick(Sender:TObject;x,y,buttons: integer):boolean;
    function RedoButtonClick(Sender:TObject;x,y,buttons: integer):boolean;
    procedure SetMouseCoords(x,y:integer);
    function ProcessMessage(msg:TMessage):boolean;
    procedure ControlsShow(Sender:TObject);
  private
    fTexture:TStreamingTexture;
    fToolButtons:array[0..5] of TBDButton;
    fInkButtons:array[0..5] of TBDButton;
    fUndoButton,fRedoButton:TBDButton;
    fColorSelector:TColorSelector;
    fMouseX,fMouseY:integer;
  end;

implementation

uses SysUtils, BDPSharedUnit, BDPToolsUnit, BDPInksUnit, MKMouse2;

{ TBDControls }

constructor TBDControls.Create;
var
  i:integer;
  atmT:TBDTool;
  atmI:TBDInk;
  atmB:TBDButton;
  msg:TMessage;
begin
  inherited Create;
  fLeft:=0;
  fTop:=WINDOWHEIGHT-CONTROLSHEIGHT;
  fWidth:=WINDOWWIDTH;
  fHeight:=CONTROLSHEIGHT;
  fTexture:=TStreamingTexture.Create(WINDOWWIDTH,CONTROLSHEIGHT);
  fTexture.ARGBImage.Clear;
  fVisible:=true;
  OnMouseEnter:=MouseEnter;
  OnMouseLeave:=MouseLeave;
  OnMouseMove:=MouseMove;
  OnMouseDown:=MouseDown;
  OnClick:=Click;
  OnShow:=ControlsShow;
  ZIndex:=LEVEL1CONTROLS_ZINDEX;
  fName:='Controls';

  msg.TypeID:=MSG_NONE;
  msg.DataInt:=0;
  for i:=0 to 5 do begin
    atmT:=Tools[Tools.IndexOf(Settings.SelectedTools[i])];
    if atmT=nil then raise Exception.Create('Tool not found! ('+Settings.SelectedTools[i]+')');
    fToolButtons[i]:=TBDButton.Create(
      fLeft+TOOLBUTTONSLEFT+i mod 2*130,
      fTop+TOOLBUTTONSTOP+i div 2*30,
      NORMALBUTTONWIDTH,
      atmT.Name,
      atmT.Hint,
      msg,
      atmT);
    fToolButtons[i].ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
    fToolButtons[i].Tag:=i;
    fToolButtons[i].OnClick:=Self.ToolButtonClick;
    AddChild(fToolButtons[i]);
  end;
  ActivateToolButton(Settings.ActiveTool);

  for i:=0 to 5 do begin
    atmi:=Inks[Inks.IndexOf(Settings.SelectedInks[i])];
    if atmT=nil then raise Exception.Create('Ink not found! ('+Settings.SelectedInks[i]+')');
    fInkButtons[i]:=TBDButton.Create(
      fLeft+InkButtonsLeft+i mod 2*130,
      fTop+InkButtonsTop+i div 2*30,
      NORMALBUTTONWIDTH,
      atmI.Name,
      atmI.Hint,
      msg,
      atmI);
    fInkButtons[i].ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
    fInkButtons[i].Tag:=i;
    fInkButtons[i].OnClick:=Self.InkButtonClick;
    AddChild(fInkButtons[i]);
  end;
  ActivateInkButton(Settings.ActiveInk);

  fUndoButton:=TBDButton.Create(fLeft+UNDOBUTTONSLEFT, fTop+UNDOBUTTONSTOP,
    NORMALBUTTONWIDTH, 'UNDO', 'UNDO LAST OPERATION', msg);
  fUndoButton.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fUndoButton.OnClick:=UndoButtonClick;
  AddChild(fUndoButton);

  fRedoButton:=TBDButton.Create(fLeft+UNDOBUTTONSLEFT, fTop+UNDOBUTTONSTOP+30,
    NORMALBUTTONWIDTH, 'REDO', 'REDO LAST UNDOED OPERATION', msg);
  fRedoButton.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fRedoButton.OnClick:=RedoButtonClick;
  AddChild(fRedoButton);

  msg.TypeID:=MSG_NONE;
  atmB:=TBDButton.Create(fLeft+TOGGLEBUTTONSLEFT, fTop+TOGGLEBUTTONSTOP,
    SMALLBUTTONWIDTH, 'F', 'FILL SHAPES', msg);
  atmB.Selected:=Settings.FillShapes;
  atmB.OnClick:=FilledButtonClick;
  atmB.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  AddChild(atmB);

  atmB:=TBDButton.Create(fLeft+TOGGLEBUTTONSLEFT, fTop+TOGGLEBUTTONSTOP+30,
    SMALLBUTTONWIDTH, 'K', 'CLEAR KEY COLOR', msg);
  atmB.Selected:=Settings.ClearKeyColor;
  atmB.OnClick:=ClearKeyColorButtonClick;
  atmB.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  AddChild(atmB);

  fColorSelector:=TColorSelector.Create(fLeft+COLORSELECTORLEFT, fTop+COLORSELECTORTOP);
  fColorSelector.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fColorSelector.Name:='ColorSelector';
  AddChild(fColorSelector);
  MouseObjects.Add(Self);
end;

destructor TBDControls.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TBDControls.Draw;
begin
  if fVisible then begin
    fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,OverlayImage.Palette[2]);
    fTexture.ARGBImage.Bar(COORDSLEFT,0,COORDSWIDTH,CONTROLSHEIGHT,OverlayImage.Palette[2]);
    fTexture.ARGBImage.Bar(0,3,COORDSLEFT,fTexture.ARGBImage.Height-3,OverlayImage.Palette[3]);
    if (fMouseX>=0) and (fMouseX<MainImage.Width) and (fMouseY>=0) and (fMouseY<MainImage.Height) then begin
      MM.Fonts['Black'].OutText(fTexture.ARGBImage,'X='+inttostr(fMouseX),COORDSCENTER,CONTROLSHEIGHT-84,1);
      MM.Fonts['Black'].OutText(fTexture.ARGBImage,'Y='+inttostr(fMouseY),COORDSCENTER,CONTROLSHEIGHT-54,1);
      MM.Fonts['Black'].OutText(fTexture.ARGBImage,'C='+inttostr(MainImage.GetPixel(fMouseX,fMouseY)),COORDSCENTER,CONTROLSHEIGHT-24,1);
    end else begin
      if (fMouseX>-1000) and (fMouseX<1000) then
        MM.Fonts['DarkRed'].OutText(fTexture.ARGBImage,'X='+inttostr(fMouseX),COORDSCENTER,CONTROLSHEIGHT-84,1)
      else
        MM.Fonts['DarkRed'].OutText(fTexture.ARGBImage,'X=OUT',COORDSCENTER,CONTROLSHEIGHT-84,1);
      if (fMouseY>=-1000) and (fMouseY<=1000) then
        MM.Fonts['DarkRed'].OutText(fTexture.ARGBImage,'Y='+inttostr(fMouseY),COORDSCENTER,CONTROLSHEIGHT-54,1)
      else
        MM.Fonts['DarkRed'].OutText(fTexture.ARGBImage,'Y=OUT',COORDSCENTER,CONTROLSHEIGHT-54,1);
      MM.Fonts['DarkRed'].OutText(fTexture.ARGBImage,'C=?',COORDSCENTER,CONTROLSHEIGHT-24,1);
    end;
//    inherited Draw;
    fTexture.Update;
    PutTexture(fLeft,fTop,fTexture);
  end;
end;

procedure TBDControls.ActivateToolButton(index:integer);
var i:integer;
begin
  if (index>=0) and (index<6) then begin
    // Select the specified button and fill ActiveTool accordingly.
    Settings.ActiveTool:=index;
    for i:=0 to length(fToolButtons)-1 do
      if i<>index then begin
        fToolButtons[i].Selected:=false;
      end else begin
        fToolButtons[i].Selected:=true;
        ActiveTool:=TBDTool(fToolButtons[i].AssignedObject);
      end;
  end else
  if index=-1 then begin
    // Don't change selected button, but refill ActiveTool
    for i:=0 to length(fToolButtons)-1 do
      if fToolButtons[i].Selected then
        ActiveTool:=TBDTool(fToolButtons[i].AssignedObject);
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

procedure TBDControls.MouseEnter(Sender:TObject);
begin
  InfoBar.ShowText('');
end;

procedure TBDControls.MouseLeave(Sender:TObject);
begin
end;

function TBDControls.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  Result:=true;
end;

function TBDControls.MouseDown(Sender:TObject; x,y,buttons:integer):boolean;
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
  if Sender is TBDButton then with Sender as TBDButton do begin
    fSelected:=not fSelected;
    Settings.FillShapes:=fSelected;
  end;
  Result:=true;
end;

function TBDControls.ClearKeyColorButtonClick(Sender:TObject;
  x,y,buttons:integer):boolean;
begin
  if Sender is TBDButton then with Sender as TBDButton do begin
    fSelected:=not fSelected;
    Settings.ClearKeyColor:=fSelected;
  end;
  Result:=true;
end;

function TBDControls.ToolButtonClick(Sender:TObject; x,y,buttons:integer):boolean;
begin
  if Sender is TBDButton then
    ActivateToolButton((Sender as TBDButton).Tag);
  ActiveTool.Move(fMouseX,fMouseY);
  Result:=true;
end;

function TBDControls.InkButtonClick(Sender:TObject; x,y,buttons:integer):boolean;
begin
  if Sender is TBDButton then
    ActivateInkButton((Sender as TBDButton).Tag);
  Result:=true;
end;

function TBDControls.UndoButtonClick(Sender:TObject; x,y,buttons:integer):boolean;
begin
  ImageUndoSystem.Undo;
  Result:=true;
end;

function TBDControls.RedoButtonClick(Sender:TObject; x,y,buttons:integer):boolean;
begin
  ImageUndoSystem.Redo;
  Result:=true;
end;

procedure TBDControls.SetMouseCoords(x,y:integer);
begin
  if x and $7000<>$7000 then fMouseX:=x else fMouseX:=x-32768;
  if y and $7000<>$7000 then fMouseY:=y else fMouseY:=y-32768;
end;

function TBDControls.ProcessMessage(msg: TMessage): boolean;
begin
  Result:=false;
  case msg.TypeID of
    MSG_SETUNDOREDOBUTTON:begin
      fUndoButton.Enabled:=ImageUndoSystem.CanUndo;
      fRedoButton.Enabled:=ImageUndoSystem.CanRedo;
    end;
    MSG_PICKEDCOLOR:begin
      fColorSelector.SetSelectedSlotTo(msg.DataInt);
      Self.ActivateToolButton(-1);  // Puts the already selected tool into ActiveTool
      InfoBar.ShowText('');
      Result:=true;
    end;
  end;
end;

procedure TBDControls.ControlsShow(Sender:TObject);
begin
  ActivateToolButton(-1);
  InfoBar.ShowText('');
end;

end.

