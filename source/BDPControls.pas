unit BDPControls;

{$mode Delphi}{$H+}

interface

uses
  SysUtils, mk_sdl2, vcc2_Container, BDPBasicControls, BDPMessage, BDPColorSelector;

type

  { TBDControls }

  TBDControls=class(TContainer)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure ActivateToolButton(index:integer);
    procedure ActivateInkButton(index:integer);
    procedure MouseEnter(Sender:TObject);
    procedure FilledButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure ClearKeyColorButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure DitherButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure ToolButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure InkButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure UndoButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure RedoButtonClick(Sender:TObject;x,y,buttons: integer);
    procedure ActiveImageChange(Sender:TObject;newvalue:integer);
    procedure SetMouseCoords(x,y:integer);
    function ProcessMessage(msg:TMessage):boolean;
    procedure ControlsShow(Sender:TObject);
  private
    fTexture:TStreamingTexture;
    fToolButtons:array[0..5] of TBDButton;
    fInkButtons:array[0..5] of TBDButton;
    fUndoButton,fRedoButton:TBDButton;
    fColorSelector:TBDColorSelector;
    fImageCountSlider:TBDHorizontalSlider;
    fMouseX,fMouseY:integer;
  end;

implementation

uses SDL2, BDPShared, BDPTools, BDPInks, MKMouse2;

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
  fTop:=WINDOWHEIGHT-CONTROLSHEIGHT;
  fWidth:=WINDOWWIDTH;
  fHeight:=CONTROLSHEIGHT;
  fTexture:=TStreamingTexture.Create(WINDOWWIDTH,CONTROLSHEIGHT);
  fTexture.ARGBImage.Clear;
  fVisible:=true;
  OnMouseEnter:=MouseEnter;
  OnShow:=ControlsShow;
  ZIndex:=LEVEL1CONTROLS_ZINDEX;
  fName:='Controls';

  for i:=0 to 5 do begin
    atmT:=Tools[Tools.IndexOf(Settings.SelectedTools[i])];
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

  for i:=0 to 5 do begin
    atmi:=Inks[Inks.IndexOf(Settings.SelectedInks[i])];
    if atmT=nil then raise Exception.Create('Ink not found! ('+Settings.SelectedInks[i]+')');
    fInkButtons[i]:=TBDButton.Create(
      fLeft+InkButtonsLeft+i mod 2*130,
      fTop+InkButtonsTop+i div 2*30,
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

  fUndoButton:=TBDButton.Create(fLeft+CONTROLUNDOBUTTONSLEFT, fTop+CONTROLUNDOBUTTONSTOP,
    NORMALBUTTONWIDTH, NORMALBUTTONHEIGHT, 'UNDO', 'UNDO LAST OPERATION');
  fUndoButton.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fUndoButton.OnClick:=UndoButtonClick;
  AddChild(fUndoButton);

  fRedoButton:=TBDButton.Create(fLeft+CONTROLUNDOBUTTONSLEFT, fTop+CONTROLUNDOBUTTONSTOP+30,
    NORMALBUTTONWIDTH, NORMALBUTTONHEIGHT, 'REDO', 'REDO LAST UNDOED OPERATION');
  fRedoButton.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fRedoButton.OnClick:=RedoButtonClick;
  AddChild(fRedoButton);

  atmB:=TBDButton.Create(fLeft+TOGGLEBUTTONSLEFT, fTop+TOGGLEBUTTONSTOP,
    SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT, 'F', 'FILL SHAPES');
  atmB.Selected:=Settings.FillShapes;
  atmB.OnClick:=FilledButtonClick;
  atmB.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  AddChild(atmB);

  atmB:=TBDButton.Create(fLeft+TOGGLEBUTTONSLEFT, fTop+TOGGLEBUTTONSTOP+30,
    SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT, 'K', 'CLEAR KEY COLOR');
  atmB.Selected:=Settings.ClearKeyColor;
  atmB.OnClick:=ClearKeyColorButtonClick;
  atmB.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  AddChild(atmB);

  atmB:=TBDButton.Create(fLeft+TOGGLEBUTTONSLEFT, fTop+TOGGLEBUTTONSTOP+60,
    SMALLBUTTONWIDTH, NORMALBUTTONHEIGHT, 'D', 'DITHER GRADIENTS. '#132'TOGGLE '#133'CONFIGURE');
  atmB.Selected:=Settings.DitherGradients;
  atmB.OnClick:=DitherButtonClick;
  atmB.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  AddChild(atmB);

  fColorSelector:=TBDColorSelector.Create(fLeft+COLORSELECTORLEFT, fTop+COLORSELECTORTOP);
  fColorSelector.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fColorSelector.Name:='ColorSelector';
  AddChild(fColorSelector);

  fImageCountSlider:=TBDHorizontalSlider.Create(
    fLeft+IMAGECOUNTSLIDERLEFT, fTop+IMAGECOUNTSLIDERTOP, IMAGESCOUNTLIDERWIDTH, IMAGECOUNTSLIDERHEIGHT);
  fImageCountSlider.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fImageCountSlider.Name:='ImagesSlider';
  fImageCountSlider.MinValue:=1;
  fImageCountSlider.MaxValue:=Project.Images.Count;
  fImageCountSlider.Position:=Project.CurrentImageIndex+1;
  fImageCountSlider.OnChange:=ActiveImageChange;
  AddChild(fImageCountSlider);

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
    fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,SystemPalette[2]);
    fTexture.ARGBImage.Bar(COORDSLEFT,0,COORDSWIDTH,CONTROLSHEIGHT,SystemPalette[2]);
    fTexture.ARGBImage.Bar(0,3,COORDSLEFT,fTexture.ARGBImage.Height-3,SystemPalette[3]);
    if (fMouseX>=0) and (fMouseX<Project.CurrentImage.Width) and (fMouseY>=0) and (fMouseY<Project.CurrentImage.Height) then begin
      MM.Fonts['Black'].OutText(fTexture.ARGBImage,'X='+inttostr(fMouseX),COORDSCENTER,CONTROLSHEIGHT-84,1);
      MM.Fonts['Black'].OutText(fTexture.ARGBImage,'Y='+inttostr(fMouseY),COORDSCENTER,CONTROLSHEIGHT-54,1);
      MM.Fonts['Black'].OutText(fTexture.ARGBImage,'C='+inttostr(Project.CurrentImage.GetPixel(fMouseX,fMouseY)),COORDSCENTER,CONTROLSHEIGHT-24,1);
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

procedure TBDControls.FilledButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  if Sender is TBDButton then with Sender as TBDButton do begin
    Selected:=not Selected;
    Settings.FillShapes:=fSelected;
  end;
end;

procedure TBDControls.ClearKeyColorButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  if Sender is TBDButton then with Sender as TBDButton do begin
    Selected:=not Selected;
    Settings.ClearKeyColor:=fSelected;
  end;
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

procedure TBDControls.ToolButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  if Sender is TBDButton then
    ActivateToolButton((Sender as TBDButton).Tag);
  ActiveTool.Move(fMouseX,fMouseY);
end;

procedure TBDControls.InkButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  if Sender is TBDButton then
    ActivateInkButton((Sender as TBDButton).Tag);
end;

procedure TBDControls.UndoButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Project.CurrentImage.ImageUndo.Undo;
end;

procedure TBDControls.RedoButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Project.CurrentImage.ImageUndo.Redo;
end;

procedure TBDControls.ActiveImageChange(Sender:TObject; newvalue:integer);
begin
  Project.CurrentImageIndex:=newvalue-1;
  MessageQueue.AddMessage(MSG_SETIMAGEUNDOREDOBUTTON);
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
    MSG_SETIMAGEUNDOREDOBUTTON:begin
      fUndoButton.Enabled:=Project.CurrentImage.ImageUndo.CanUndo;
      fRedoButton.Enabled:=Project.CurrentImage.ImageUndo.CanRedo;
      Result:=true;
    end;
    MSG_COLORSELECTORPICKEDCOLOR:begin
      fColorSelector.SetSelectedSlotTo(msg.DataInt);
      Self.ActivateToolButton(-1);  // Puts the already selected tool into ActiveTool
      InfoBar.ShowText('');
      Result:=true;
    end;
    MSG_PROJECTIMAGECOUNTCHANGED:begin
      fImageCountSlider.MaxValue:=Project.Images.Count;
      fImageCountSlider.Position:=Project.CurrentImageIndex+1;
      MessageQueue.AddMessage(MSG_SETIMAGEUNDOREDOBUTTON);
      Result:=false;  // Not true, let the others also know about the count change!
    end;
  end;
end;

procedure TBDControls.ControlsShow(Sender:TObject);
begin
  ActivateToolButton(-1);
  InfoBar.ShowText('');
end;

end.

