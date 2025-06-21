{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPControls;

{$mode Delphi}{$H+}

interface

uses
  SysUtils, mk_sdl2, vcc2_ContainerStatic, BDPMessage, BDPGradientControl, BDPToolBase,
  BDPInkBase, BDPColorSelector, BDPButton, BDPSliders, MKMouse2;

type

  { TBDControls }

  TBDControls=class(TContainerStatic)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
    procedure ActivateToolButton(index:integer);
    procedure ActivateInkButton(index:integer);
    function ProcessMessage(msg:TMessage):boolean;
    procedure ChangeActiveToolButtonTo(pTool:TBDTool);
    procedure ChangeActiveInkButtonTo(pInk:TBDInk);
    procedure SetColor(pTarget:integer;pColor:uint32);
  protected
    procedure ReDraw; override;
  private
    fToolButtons:array of TBDButton;
    fInkButtons:array of TBDButton;
    fUndoButton,fRedoButton:TBDButton;
    fColorSelector:TBDColorSelector;
    fGradient:TBDGradient;
    fImageCountSlider:TBDHorizontalSlider;
    procedure CreateBaseLayout;
    procedure CreateExtendedLayout;
    procedure CreateUndoButtonGroup(var x:integer;y:integer);
    procedure CreateToolButtons(var x:integer;y,rowcount,colcount:integer);
    procedure CreateColorSelector(var x:integer;y:integer);
    procedure CreateImageCountSlider(var x:integer;y:integer);
    procedure CreateInkButtons(var x:integer;y,rowcount,colcount:integer);
    procedure CreateGradient(var x:integer;y:integer);
    procedure CreateToggleButtons2x2(var x:integer;y:integer);
    procedure CreateToggleButtons4x1(var x:integer;y:integer);
    procedure CreateButton(pX,pY,pWidth,pHeight:integer;
                           pCaption,pHint:string;pSelected:boolean;
                           pClick:TMouseButtonEvent;
                           pKeyDown:TKeyEvent);
    procedure MouseEnter(Sender:TObject);
    procedure ControlsShow(Sender:TObject);
    function ControlsKeyDown(Sender:TObject;key:integer):boolean;
    procedure FilledButtonClick(Sender:TObject;x,y,buttons: integer);
    function FilledButtonKeyDown(Sender:TObject;key:integer):boolean;
    procedure ClearKeyColorButtonClick(Sender:TObject;x,y,buttons: integer);
    function ClearKeyColorButtonKeyDown(Sender:TObject;key:integer):boolean;
//    procedure DitherButtonClick(Sender:TObject;x,y,buttons: integer);
//    function DitherButtonKeyDown(Sender:TObject;key:integer):boolean;
    procedure ToolButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure InkButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure UndoButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure RedoButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure ActiveImageChange(Sender:TObject;oldvalue,newvalue:integer);
    procedure UpdateUndoRedoButtons;
  end;

implementation

uses SDL2, BDPShared, BDPKeyMapping;


{ TBDControls }

constructor TBDControls.Create(iLeft, iTop, iWidth, iHeight: integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=iWidth;
  Height:=iHeight;
  fVisible:=true;
  OnMouseEnter:=MouseEnter;
  OnShow:=ControlsShow;
  OnKeyDown:=ControlsKeyDown;
  ZIndex:=LEVEL1CONTROLS_ZINDEX;
  fName:='Controls';

  if Width>1168+NORMALBUTTONWIDTH+3 then
    CreateExtendedLayout
  else
    CreateBaseLayout;

  ActivateToolButton(Settings.ActiveTool);
  ActivateInkButton(Settings.ActiveInk);

  MouseObjects.Add(Self);
  fNeedRedraw:=true;
end;

procedure TBDControls.ActivateToolButton(index:integer);
var i:integer;
begin
  if (index>=0) and (index<length(fToolButtons)) then begin
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
  if (index>=0) and (index<length(fInkButtons)) then begin
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

{procedure TBDControls.DitherButtonClick(Sender:TObject; x,y,buttons:integer);
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
end;}

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

procedure TBDControls.UpdateUndoRedoButtons;
begin
  fUndoButton.Enabled:=Project.CurrentImage.RegionUndo.CanUndo;
  fRedoButton.Enabled:=Project.CurrentImage.RegionUndo.CanRedo;
end;

function TBDControls.ProcessMessage(msg: TMessage): boolean;
begin
  Result:=false;
  case msg.TypeID of
    MSG_ACTIVEIMAGECHANGED:begin
      fImageCountSlider.MaxValue:=Project.Images.Count;
      fImageCountSlider.Position:=Project.CurrentImageIndex+1;
      fGradient.Gradient:=Project.CurrentGradientList.ActiveGradient;
      UpdateUndoRedoButtons;
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
    MSG_INKGRAYSCALE:ChangeActiveInkButtonTo(Inks.ItemByName['GRAYSCALE']);
  end;
end;

procedure TBDControls.ControlsShow(Sender:TObject);
begin
  ActivateToolButton(-1);
  InfoBar.ShowText('');
  fGradient.Gradient:=Project.CurrentGradientList.ActiveGradient;
  UpdateUndoRedoButtons;
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

procedure TBDControls.CreateBaseLayout;
var x,y,savedx:integer;
begin
  x:=fLeft+3;y:=fTop+6;
  CreateUndoButtonGroup(x,y);
  CreateToolButtons(x,y,3,2);
  savedx:=x;
  CreateColorSelector(savedx,y);
  CreateImageCountSlider(x,y+CONTROLSHEIGHT-IMAGECOUNTSLIDERHEIGHT-9);
  savedx:=x;
  CreateGradient(savedx,y);
  CreateInkButtons(x,y+NORMALBUTTONHEIGHT+3,2,3);
  CreateToggleButtons2x2(x,y+NORMALBUTTONHEIGHT+3);
end;

procedure TBDControls.CreateExtendedLayout;
var x,y,savedx:integer;
begin
  x:=fLeft+3;y:=fTop+6;
  CreateUndoButtonGroup(x,y);
  CreateToolButtons(x,y,3,3);
  savedx:=x;
  CreateColorSelector(savedx,y);
  CreateImageCountSlider(x,y+CONTROLSHEIGHT-IMAGECOUNTSLIDERHEIGHT-9);
  savedx:=x;
  CreateGradient(savedx,y);
  CreateInkButtons(x,y+NORMALBUTTONHEIGHT+3,2,4);
  CreateToggleButtons4x1(savedx,y);
end;

procedure TBDControls.CreateUndoButtonGroup(var x:integer; y:integer);
begin
  fUndoButton:=TBDButton.Create(x, y+NORMALBUTTONHEIGHT+3,
    CONTROLUNDOBUTTONWIDTH, NORMALBUTTONHEIGHT, 'UNDO', 'UNDO LAST OPERATION.');
  fUndoButton.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fUndoButton.OnClick:=UndoButtonClick;
  AddChild(fUndoButton);

  fRedoButton:=TBDButton.Create(x, y+(NORMALBUTTONHEIGHT+3)*2,
    CONTROLUNDOBUTTONWIDTH, NORMALBUTTONHEIGHT, 'REDO', 'REDO LAST UNDOED OPERATION.');
  fRedoButton.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fRedoButton.OnClick:=RedoButtonClick;
  AddChild(fRedoButton);

  x+=CONTROLUNDOBUTTONWIDTH+3;
end;

procedure TBDControls.CreateToolButtons(var x:integer;y,rowcount,colcount:integer);
var i,j:integer;atmT:TBDTool;
begin
  j:=rowcount*colcount;
  if j>8 then j:=8;
  SetLength(fToolButtons,j);
  for i:=0 to j-1 do begin
    atmT:=Tools.ItemByName[Settings.SelectedTools[i]];
    if atmT=nil then raise Exception.Create('Tool not found! ('+Settings.SelectedTools[i]+')');
    fToolButtons[i]:=TBDButton.Create(
      x+i div rowcount*(NORMALBUTTONWIDTH+3),
      y+i mod rowcount*(NORMALBUTTONHEIGHT+3),
      NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
      atmT.Name,
      atmT.Hint,
      atmT);
    fToolButtons[i].ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
    fToolButtons[i].Tag:=i;
    fToolButtons[i].OnClick:=Self.ToolButtonClick;
    AddChild(fToolButtons[i]);
  end;
  x+=colcount*(NORMALBUTTONWIDTH+3);
end;

procedure TBDControls.CreateColorSelector(var x:integer; y:integer);
begin
  fColorSelector:=TBDColorSelector.Create(x, y, 279,36);
  fColorSelector.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fColorSelector.Name:='ColorSelector';
  AddChild(fColorSelector);
  x+=IMAGECOUNTSLIDERWIDTH+3;
end;

procedure TBDControls.CreateImageCountSlider(var x:integer; y:integer);
begin
  fImageCountSlider:=TBDHorizontalSlider.Create(
    x, y, IMAGECOUNTSLIDERWIDTH, IMAGECOUNTSLIDERHEIGHT);
  fImageCountSlider.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fImageCountSlider.Name:='ImagesSlider';
  fImageCountSlider.MinValue:=1;
  fImageCountSlider.MaxValue:=Project.Images.Count;
  fImageCountSlider.Position:=Project.CurrentImageIndex+1;
  fImageCountSlider.MaxValue:=1;
  fImageCountSlider.Position:=1;
  fImageCountSlider.OnChange:=ActiveImageChange;
  AddChild(fImageCountSlider);
  x+=IMAGECOUNTSLIDERWIDTH+3;
end;

procedure TBDControls.CreateInkButtons(var x:integer;y,rowcount,colcount:integer);
var i:integer;atmi:TBDInk;
begin
  SetLength(fInkButtons,rowcount*colcount);
  for i:=0 to rowcount*colcount-1 do begin
    atmi:=Inks.ItemByName[Settings.SelectedInks[i]];
    if atmi=nil then raise Exception.Create('Ink not found! ('+Settings.SelectedInks[i]+')');
    fInkButtons[i]:=TBDButton.Create(
    x+i div rowcount*(NORMALBUTTONWIDTH+3),
    y+i mod rowcount*(NORMALBUTTONHEIGHT+3),
      NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
      atmI.Name,
      atmI.Hint,
      atmI);
    fInkButtons[i].ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
    fInkButtons[i].Tag:=i;
    fInkButtons[i].OnClick:=Self.InkButtonClick;
    AddChild(fInkButtons[i]);
  end;
  x+=colcount*(NORMALBUTTONWIDTH+3);
end;

procedure TBDControls.CreateGradient(var x:integer; y:integer);
begin
  fGradient:=TBDGradient.Create(x,y,CONTROLGRADIENTWIDTH,NORMALBUTTONHEIGHT,Project.CurrentGradientList.ActiveGradient);
  fGradient.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fGradient.Name:='Gradient (Controls)';
  AddChild(fGradient);
  x+=CONTROLGRADIENTWIDTH+3;
end;

procedure TBDControls.CreateToggleButtons2x2(var x:integer; y:integer);
begin
  CreateButton(x, y, SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'F', 'FILL SHAPES.',
    Settings.FillShapes, FilledButtonClick, FilledButtonKeyDown);

  CreateButton(x+SMALLBUTTONWIDTH+3, y, SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'K', 'CLEAR KEY COLOR.',
    Settings.ClearKeyColor, ClearKeyColorButtonClick, ClearKeyColorButtonKeyDown);

{  CreateButton(x, y+NORMALBUTTONHEIGHT+3, SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'D', 'DITHER GRADIENTS. '#132'TOGGLE '#133'CONFIGURE',
    Settings.DitherGradients, DitherButtonClick, DitherButtonKeyDown);}

  CreateButton(x+SMALLBUTTONWIDTH+3, y+NORMALBUTTONHEIGHT+3, SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'G', 'SHOW GRID. '#132'TOGGLE '#133'CONFIGURE',
    Settings.ShowGrid, nil, nil);
  x+=2*(SMALLBUTTONWIDTH+3);
end;

procedure TBDControls.CreateToggleButtons4x1(var x:integer; y:integer);
begin
  CreateButton(x, y, SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'F', 'FILL SHAPES.',
    Settings.FillShapes, FilledButtonClick, FilledButtonKeyDown);

  CreateButton(x+SMALLBUTTONWIDTH+3, y, SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'K', 'CLEAR KEY COLOR.',
    Settings.ClearKeyColor, ClearKeyColorButtonClick, ClearKeyColorButtonKeyDown);

{  CreateButton(x+(SMALLBUTTONWIDTH+3)*2, y, SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'D', 'DITHER GRADIENTS. '#132'TOGGLE '#133'CONFIGURE',
    Settings.DitherGradients, DitherButtonClick, DitherButtonKeyDown);}

  CreateButton(x+(SMALLBUTTONWIDTH+3)*2, y, SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'G', 'SHOW GRID. '#132'TOGGLE '#133'CONFIGURE',
    Settings.ShowGrid, nil, nil);
  x+=4*(SMALLBUTTONWIDTH+3);
end;

procedure TBDControls.CreateButton(pX,pY,pWidth,pHeight:integer;
  pCaption,pHint:string; pSelected:boolean; pClick:TMouseButtonEvent;
  pKeyDown:TKeyEvent);
var atmB:TBDButton;
begin
  atmB:=TBDButton.Create(pX, pY, pWidth, pHeight, pCaption, pHint);
  atmB.Selected:=pSelected;
  atmB.OnClick:=pClick;
  atmB.OnKeyDown:=pKeyDown;
  atmB.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  AddChild(atmB);
end;

end.

