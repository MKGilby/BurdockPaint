{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPControls;

{$mode Delphi}{$H+}

interface

uses
  SysUtils, mk_sdl2, vcc2_ContainerStatic, BDPMessage, BDPGradientControl, BDPToolBase,
  BDPInkBase, BDPColorSelector, BDPButton, BDPSliders;

type

  { TBDControls }

  TBDControls=class(TContainerStatic)
    constructor Create;
    procedure ActivateToolButton(index:integer);
    procedure ActivateInkButton(index:integer);
    function ProcessMessage(msg:TMessage):boolean;
    procedure ChangeActiveToolButtonTo(pTool:TBDTool);
    procedure ChangeActiveInkButtonTo(pInk:TBDInk);
    procedure SetColor(pTarget:integer;pColor:uint32);
  protected
    procedure ReDraw; override;
  private
    fToolButtons:array[0..5] of TBDButton;
    fInkButtons:array[0..5] of TBDButton;
    fUndoButton,fRedoButton:TBDButton;
    fColorSelector:TBDColorSelector;
    fGradient:TBDGradient;
    fImageCountSlider:TBDHorizontalSlider;
    procedure MouseEnter(Sender:TObject);
    procedure ControlsShow(Sender:TObject);
    function ControlsKeyDown(Sender:TObject;key:integer):boolean;
    procedure FilledButtonClick(Sender:TObject;x,y,buttons: integer);
    function FilledButtonKeyDown(Sender:TObject;key:integer):boolean;
    procedure ClearKeyColorButtonClick(Sender:TObject;x,y,buttons: integer);
    function ClearKeyColorButtonKeyDown(Sender:TObject;key:integer):boolean;
    procedure DitherButtonClick(Sender:TObject;x,y,buttons: integer);
    function DitherButtonKeyDown(Sender:TObject;key:integer):boolean;
    procedure ToolButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure InkButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure UndoButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure RedoButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure ActiveImageChange(Sender:TObject;oldvalue,newvalue:integer);
  end;

implementation

uses SDL2, BDPShared, MKMouse2, BDPKeyMapping;

const
  GRADIENTLEFT=720;
  GRADIENTWIDTH=320;
  GRADIENTHEIGHT=36;

{ TBDControls }

constructor TBDControls.Create;
var
  i:integer;
  atmT:TBDTool;
  atmI:TBDInk;
  atmB:TBDButton;
begin
  inherited Create;
  fLeft:=0;
  fTop:=Settings.WindowHeight-CONTROLSHEIGHT;
  Width:=Settings.WindowWidth-112;
  Height:=CONTROLSHEIGHT;
  fVisible:=true;
  OnMouseEnter:=MouseEnter;
  OnShow:=ControlsShow;
  OnKeyDown:=ControlsKeyDown;
  ZIndex:=LEVEL1CONTROLS_ZINDEX;
  fName:='Controls';

  // Tool buttons
  for i:=0 to 5 do begin
    atmT:=Tools.ItemByName[Settings.SelectedTools[i]];
    if atmT=nil then raise Exception.Create('Tool not found! ('+Settings.SelectedTools[i]+')');
    fToolButtons[i]:=TBDButton.Create(
      fLeft+TOOLBUTTONSLEFT+i mod 2*130,
      fTop+TOOLBUTTONSTOP+i div 2*30,
      NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
      atmT.Name,
      atmT.Hint,
      atmT);
    fToolButtons[i].ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
    fToolButtons[i].Tag:=i;
    fToolButtons[i].OnClick:=Self.ToolButtonClick;
    AddChild(fToolButtons[i]);
  end;
  ActivateToolButton(Settings.ActiveTool);

  // Ink buttons
  for i:=0 to 5 do begin
    atmi:=Inks.ItemByName[Settings.SelectedInks[i]];
    if atmi=nil then raise Exception.Create('Ink not found! ('+Settings.SelectedInks[i]+')');
    fInkButtons[i]:=TBDButton.Create(
      fLeft+InkButtonsLeft+i mod 3*130,
      fTop+InkButtonsTop+i div 3*30,
      NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
      atmI.Name,
      atmI.Hint,
      atmI);
    fInkButtons[i].ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
    fInkButtons[i].Tag:=i;
    fInkButtons[i].OnClick:=Self.InkButtonClick;
    AddChild(fInkButtons[i]);
  end;
  ActivateInkButton(Settings.ActiveInk);

  // Undo/redo buttons
  fUndoButton:=TBDButton.Create(fLeft+CONTROLUNDOBUTTONSLEFT, fTop+CONTROLUNDOBUTTONSTOP,
    CONTROLUNDOBUTTONWIDTH, NORMALBUTTONHEIGHT, 'UNDO', 'UNDO LAST OPERATION.');
  fUndoButton.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fUndoButton.OnClick:=UndoButtonClick;
  AddChild(fUndoButton);

  fRedoButton:=TBDButton.Create(fLeft+CONTROLUNDOBUTTONSLEFT, fTop+CONTROLUNDOBUTTONSTOP+30,
    CONTROLUNDOBUTTONWIDTH, NORMALBUTTONHEIGHT, 'REDO', 'REDO LAST UNDOED OPERATION.');
  fRedoButton.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fRedoButton.OnClick:=RedoButtonClick;
  AddChild(fRedoButton);

  // Toggle buttons
  atmB:=TBDButton.Create(fLeft+TOGGLEBUTTONSLEFT, fTop+TOGGLEBUTTONSTOP+30,
    SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT, 'F', 'FILL SHAPES.');
  atmB.Selected:=Settings.FillShapes;
  atmB.OnClick:=FilledButtonClick;
  atmB.OnKeyDown:=FilledButtonKeyDown;
  atmB.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  AddChild(atmB);

  atmB:=TBDButton.Create(fLeft+TOGGLEBUTTONSLEFT+SMALLBUTTONWIDTH+3, fTop+TOGGLEBUTTONSTOP+30,
    SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT, 'K', 'CLEAR KEY COLOR.');
  atmB.Selected:=Settings.ClearKeyColor;
  atmB.OnClick:=ClearKeyColorButtonClick;
  atmB.OnKeyDown:=ClearKeyColorButtonKeyDown;
  atmB.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  AddChild(atmB);

  atmB:=TBDButton.Create(fLeft+TOGGLEBUTTONSLEFT, fTop+TOGGLEBUTTONSTOP+60,
    SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT, 'D', 'DITHER GRADIENTS. '#132'TOGGLE '#133'CONFIGURE');
  atmB.Selected:=Settings.DitherGradients;
  atmB.OnClick:=DitherButtonClick;
  atmB.OnKeyDown:=DitherButtonKeyDown;
  atmB.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  AddChild(atmB);

  atmB:=TBDButton.Create(fLeft+TOGGLEBUTTONSLEFT+SMALLBUTTONWIDTH+3, fTop+TOGGLEBUTTONSTOP+60,
    SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT, 'G', 'SHOW GRID. '#132'TOGGLE '#133'CONFIGURE');
  atmB.Selected:=Settings.ShowGrid;
//  atmB.OnClick:=DitherButtonClick;
//  atmB.OnKeyDown:=DitherButtonKeyDown;
  atmB.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  AddChild(atmB);

  // Color selector
  fColorSelector:=TBDColorSelector.Create(fLeft+CONTROLCOLORSELECTORLEFT, fTop+CONTROLCOLORSELECTORTOP,
    279,36);
  fColorSelector.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fColorSelector.Name:='ColorSelector';
  AddChild(fColorSelector);

  // Imagecount slider
  fImageCountSlider:=TBDHorizontalSlider.Create(
    fLeft+IMAGECOUNTSLIDERLEFT, fTop+IMAGECOUNTSLIDERTOP, IMAGECOUNTSLIDERWIDTH, IMAGECOUNTSLIDERHEIGHT);
  fImageCountSlider.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fImageCountSlider.Name:='ImagesSlider';
  fImageCountSlider.MinValue:=1;
  fImageCountSlider.MaxValue:=Project.Images.Count;
  fImageCountSlider.Position:=Project.CurrentImageIndex+1;
  fImageCountSlider.MaxValue:=1;
  fImageCountSlider.Position:=1;
  fImageCountSlider.OnChange:=ActiveImageChange;
  AddChild(fImageCountSlider);

  // Gradient
  fGradient:=TBDGradient.Create(fLeft+INKBUTTONSLEFT,fTop+6,GRADIENTWIDTH,GRADIENTHEIGHT,Project.CurrentGradientList.ActiveGradient);
  fGradient.Height:=NORMALBUTTONHEIGHT;
  fGradient.Width:=CONTROLGRADIENTWIDTH;
  fGradient.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fGradient.Name:='Gradient (Controls)';
  AddChild(fGradient);

  MouseObjects.Add(Self);
  fNeedRedraw:=true;
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

procedure TBDControls.FilledButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  if Sender is TBDButton then with Sender as TBDButton do begin
    Selected:=not Selected;
    Settings.FillShapes:=fSelected;
  end;
end;

function TBDControls.FilledButtonKeyDown(Sender:TObject; key:integer):boolean;
begin
  if key=KeyMap[KEY_TOGGLEFILLSHAPES] then begin
    if Sender is TBDButton then with Sender as TBDButton do begin
      Selected:=not Selected;
      Settings.FillShapes:=fSelected;
    end;
    Result:=true;
  end else Result:=false;
end;

procedure TBDControls.ClearKeyColorButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  if Sender is TBDButton then with Sender as TBDButton do begin
    Selected:=not Selected;
    Settings.ClearKeyColor:=fSelected;
  end;
end;

function TBDControls.ClearKeyColorButtonKeyDown(Sender:TObject; key:integer):boolean;
begin
  if key=KeyMap[KEY_TOGGLEKEYCOLOR] then begin
    if Sender is TBDButton then with Sender as TBDButton do begin
      Selected:=not Selected;
      Settings.ClearKeyColor:=fSelected;
    end;
    Result:=true;
  end else Result:=false;
end;

procedure TBDControls.DitherButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  if buttons=SDL_BUTTON_LEFT then begin
    if Sender is TBDButton then with Sender as TBDButton do begin
      Selected:=not Selected;
      Settings.DitherGradients:=fSelected;
    end;
  end else begin
    MessageQueue.AddMessage(MSG_OPENDITHERDIALOG);
  end;
end;

function TBDControls.DitherButtonKeyDown(Sender:TObject; key:integer):boolean;
begin
  if key=KeyMap[KEY_TOGGLEDITHER] then begin
    if Sender is TBDButton then with Sender as TBDButton do begin
      Selected:=not Selected;
      Settings.DitherGradients:=fSelected;
    end;
    Result:=true;
  end else Result:=false;
end;

procedure TBDControls.ToolButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  if Sender is TBDButton then
    if buttons=SDL_BUTTON_LEFT then
      ActivateToolButton((Sender as TBDButton).Tag)
    else if buttons=SDL_BUTTON_RIGHT then
      TBDTool((Sender as TBDButton).AssignedObject).Configure;
  ActiveTool.Move(DrawAreaX,DrawAreaY);
end;

procedure TBDControls.InkButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  if Sender is TBDButton then begin
    if buttons=SDL_BUTTON_LEFT then
      ActivateInkButton((Sender as TBDButton).Tag)
    else if buttons=SDL_BUTTON_RIGHT then
      TBDInk((Sender as TBDButton).AssignedObject).Configure;
  end;
end;

procedure TBDControls.UndoButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Project.CurrentImage.RegionUndo.Undo;
  MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED);
end;

procedure TBDControls.RedoButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Project.CurrentImage.RegionUndo.Redo;
  MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED);
end;

procedure TBDControls.ActiveImageChange(Sender:TObject; oldvalue,newvalue:integer);
begin
  Project.CurrentImageIndex:=newvalue-1;
  MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED);
end;

function TBDControls.ProcessMessage(msg: TMessage): boolean;
begin
  Result:=false;
  case msg.TypeID of
    MSG_ACTIVEIMAGECHANGED:begin
      fImageCountSlider.MaxValue:=Project.Images.Count;
      fImageCountSlider.Position:=Project.CurrentImageIndex+1;
      fGradient.Gradient:=Project.CurrentGradientList.ActiveGradient;
      fUndoButton.Enabled:=Project.CurrentImage.RegionUndo.CanUndo;
      fRedoButton.Enabled:=Project.CurrentImage.RegionUndo.CanRedo;
      Result:=false;  // Not true, let the others also know about the count change!
    end;
    MSG_ACTIVECOLORCHANGED:begin
      fColorSelector.Refresh;
      fGradient.Refresh;
    end;
    MSG_ACTIVEGRADIENTCHANGED:begin
      fGradient.Gradient:=Project.CurrentGradientList.ActiveGradient;
    end;
    MSG_TOOLDRAW:ChangeActiveToolButtonTo(Tools.ItemByName['DRAW']);
    MSG_TOOLBOX:ChangeActiveToolButtonTo(Tools.ItemByName['BOX']);
    MSG_TOOLLINE:ChangeActiveToolButtonTo(Tools.ItemByName['LINE']);
    MSG_TOOLCIRCLE:ChangeActiveToolButtonTo(Tools.ItemByName['CIRCLE']);
    MSG_TOOLFILL:ChangeActiveToolButtonTo(Tools.ItemByName['FILL']);
    MSG_TOOLFILLTO:ChangeActiveToolButtonTo(Tools.ItemByName['FILLTO']);
    MSG_TOOLSEP:ChangeActiveToolButtonTo(Tools.ItemByName['SEP.']);
    MSG_TOOLEDGE:ChangeActiveToolButtonTo(Tools.ItemByName['EDGE']);
    MSG_INKOPAQUE:ChangeActiveInkButtonTo(Inks.ItemByName['OPAQUE']);
    MSG_INKHGRAD:ChangeActiveInkButtonTo(Inks.ItemByName['H GRAD']);
    MSG_INKVGRAD:ChangeActiveInkButtonTo(Inks.ItemByName['V GRAD']);
    MSG_INKLGRAD:ChangeActiveInkButtonTo(Inks.ItemByName['L GRAD']);
    MSG_INKRGRAD:ChangeActiveInkButtonTo(Inks.ItemByName['R GRAD']);
    MSG_INKCGRAD:ChangeActiveInkButtonTo(Inks.ItemByName['C GRAD']);
    MSG_INKRANDOM:ChangeActiveInkButtonTo(Inks.ItemByName['RANDOM']);
    MSG_INKSOFTEN:ChangeActiveInkButtonTo(Inks.ItemByName['SOFTEN']);
    MSG_INKTINT:ChangeActiveInkButtonTo(Inks.ItemByName['TINT']);
  end;
end;

procedure TBDControls.ControlsShow(Sender:TObject);
begin
  ActivateToolButton(-1);
  InfoBar.ShowText('');
  fGradient.Gradient:=Project.CurrentGradientList.ActiveGradient;
end;

function TBDControls.ControlsKeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=false;
  if key=KeyMap[KEY_GETCEL] then begin
    if ActiveTool.Pinnable then begin  // Not GetCEL or PutCEL
      MessageQueue.AddMessage(MSG_GETCEL);
    end else begin
      MessageQueue.AddMessage(MSG_RESTORECONTROLS);
    end;
    Result:=true;
  end else
  if key=KeyMap[KEY_PUTCEL] then begin
    if ActiveTool.Pinnable and (Assigned(Project.CELImage)) then begin  // Not GetCEL or PutCEL
      MessageQueue.AddMessage(MSG_PUTCEL);
    end else begin
      MessageQueue.AddMessage(MSG_RESTORECONTROLS);
    end;
    Result:=true;
  end;
end;

procedure TBDControls.ChangeActiveToolButtonTo(pTool:TBDTool);
begin
  fToolButtons[Settings.ActiveTool].Caption:=pTool.Name;
  fToolButtons[Settings.ActiveTool].Hint:=pTool.Hint;
  fToolButtons[Settings.ActiveTool].Refresh;
  fToolButtons[Settings.ActiveTool].AssignedObject:=pTool;
  Settings.SelectedTools[Settings.ActiveTool]:=pTool.Name;
  ActiveTool:=pTool;
  MessageQueue.AddMessage(MSG_SETTOOLSMENU);
end;

procedure TBDControls.ChangeActiveInkButtonTo(pInk:TBDInk);
begin
  fInkButtons[Settings.ActiveInk].Caption:=pInk.Name;
  fInkButtons[Settings.ActiveInk].Hint:=pInk.Hint;
  fInkButtons[Settings.ActiveInk].Refresh;
  fInkButtons[Settings.ActiveInk].AssignedObject:=pInk;
  Settings.SelectedInks[Settings.ActiveInk]:=pInk.Name;
  ActiveInk:=pInk;
  MessageQueue.AddMessage(MSG_SETINKSMENU);
end;

procedure TBDControls.SetColor(pTarget: integer; pColor: uint32);
begin
  if pColor<>POSTPROCESSCOLOR then begin
    case pTarget of
      PARM_COL_SELECTOR_LEFT:Settings.ColorSelectorLeftColor:=pColor;
      PARM_COL_SELECTOR_MAIN:Settings.ColorSelectorMainColor:=pColor;
      PARM_COL_SELECTOR_RIGHT:Settings.ColorSelectorRightColor:=pColor;
    end;
    fColorSelector.Refresh;
  end;
end;

procedure TBDControls.ReDraw;
begin
  fImage.Bar(0,0,fImage.Width,3,SystemPalette[SYSTEMCOLORDARK]);
  fImage.Bar(0,3,fImage.Width,fImage.Height-3,SystemPalette[SYSTEMCOLORMID]);
end;

end.

