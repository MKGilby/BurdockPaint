unit BDPPaletteEditorUnit;

{$mode Delphi}

interface

uses
  vcc2_Container, BDPButtonUnit, mk_sdl2, BDPMessageUnit, BDPSliderUnit,
  BDPColorSelectorUnit, BDPColorBoxUnit, BDPColorClusterUnit;

type

  { TBDPaletteEditor }

  TBDPaletteEditor=class(TContainer)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
    procedure MouseMove(Sender:TObject;x,y:integer);
    procedure MouseDown(Sender:TObject;x,y,buttons:integer);
    procedure MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer);
    procedure OnSliderRChange(Sender:TObject;newValue:integer);
    procedure OnColorSliderMouseDown(Sender:TObject;x,y,buttons:integer);
    procedure OnColorSliderMouseUp(Sender:TObject;x,y,buttons:integer);
    procedure OnSliderGChange(Sender:TObject;newValue:integer);
    procedure OnSliderBChange(Sender:TObject;newValue:integer);
    procedure OnSliderAChange(Sender:TObject;newValue:integer);
    procedure OnSliderBankChange(Sender:TObject;newValue:integer);
    procedure UndoButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure RedoButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure PaletteEditorShow(Sender:TObject);
    procedure PaletteEditorHide(Sender:TObject);
    procedure RefreshSliders;
    function ProcessMessage(msg:TMessage):boolean;
  private
    fTexture:TStreamingTexture;
    fSliderR,fSliderG,fSliderB,fSliderA:TBDHorizontalSlider;
    fSliderBank:TBDVerticalSlider;
    fUndoButton,fRedoButton:TBDButton;
    fColorSelector:TBDColorSelector;
    fColorBox:TBDColorBox;
    fColorCluster:TBDColorCluster;
    fSavedColor:uint32;
  end;

implementation

uses SysUtils, BDPSharedUnit, MKMouse2, BDPToolsUnit, SDL2;

{ TBDPaletteEditor }

constructor TBDPaletteEditor.Create;
begin
  inherited Create;
  fLeft:=0;
  fTop:=WINDOWHEIGHT-PALETTEEDITORHEIGHT;
  fWidth:=WINDOWWIDTH;
  fHeight:=PALETTEEDITORHEIGHT;
  fTexture:=TStreamingTexture.Create(fWidth,fHeight);
  fTexture.ARGBImage.Clear;
  OnMouseEnter:=MouseEnter;
  OnMouseLeave:=MouseLeave;
  OnMouseMove:=MouseMove;
  OnMouseDown:=MouseDown;
  OnMouseWheel:=MouseWheel;
  OnShow:=PaletteEditorShow;
  OnHide:=PaletteEditorHide;
  fName:='PaletteEditor';
  fVisible:=false;
  ZIndex:=LEVEL1CONTROLS_ZINDEX;

  fSliderR:=TBDHorizontalSlider.Create(fLeft+COLORSLIDERSLEFT,fTop+COLORSLIDERSTOP);
  with fSliderR do begin
    MinValue:=0;MaxValue:=255;Position:=32;
    ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
    Name:='R-slider';
    OnChange:=OnSliderRChange;
    OnMouseDown:=OnColorSliderMouseDown;
    OnMouseUp:=OnColorSliderMouseUp;
  end;
  AddChild(fSliderR);

  fSliderG:=TBDHorizontalSlider.Create(fLeft+COLORSLIDERSLEFT+COLORSLIDERWIDTH+3,fTop+COLORSLIDERSTOP);
  with fSliderG do begin
    MinValue:=0;MaxValue:=255;Position:=32;
    ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
    Name:='G-slider';
    OnChange:=OnSliderGChange;
    OnMouseDown:=OnColorSliderMouseDown;
    OnMouseUp:=OnColorSliderMouseUp;
  end;
  AddChild(fSliderG);

  fSliderB:=TBDHorizontalSlider.Create(fLeft+COLORSLIDERSLEFT+2*(COLORSLIDERWIDTH+3),fTop+COLORSLIDERSTOP);
  with fSliderB do begin
    MinValue:=0;MaxValue:=255;Position:=32;
    ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
    Name:='B-slider';
    OnChange:=OnSliderBChange;
    OnMouseDown:=OnColorSliderMouseDown;
    OnMouseUp:=OnColorSliderMouseUp;
  end;
  AddChild(fSliderB);

  fSliderA:=TBDHorizontalSlider.Create(fLeft+COLORSLIDERSLEFT+3*(COLORSLIDERWIDTH+3),fTop+COLORSLIDERSTOP);
  with fSliderA do begin
    MinValue:=0;MaxValue:=255;Position:=255;
    ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
    Name:='A-slider';
    OnChange:=OnSliderAChange;
    OnMouseDown:=OnColorSliderMouseDown;
    OnMouseUp:=OnColorSliderMouseUp;
  end;
  AddChild(fSliderA);

  fSliderBank:=TBDVerticalSlider.Create(PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3+3,Self.Top+PALETTESOCKETSTOP);
  with fSliderBank do begin
    Height:=PALETTESOCKETHEIGHT*8+3;
    MinValue:=1;
    MaxValue:=8;
    Position:=1;
    ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
    Name:='Bank-slider';
    OnChange:=OnSliderBankChange;
  end;
  AddChild(fSliderBank);

  fUndoButton:=TBDButton.Create(6,fTop+6,127-2*18,'UNDO','UNDO LAST PALETTE OPERATION',TMessage.Init(MSG_NONE,0));
  fUndoButton.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fUndoButton.Name:='Palette Undo';
  fUndoButton.OnClick:=UndoButtonClick;
  AddChild(fUndoButton);

  fRedoButton:=TBDButton.Create(6,fTop+6+30,127-2*18,'REDO','REDO LAST UNDOED PALETTE OPERATION',TMessage.Init(MSG_NONE,0));
  fRedoButton.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fRedoButton.Name:='Palette Redo';
  fRedoButton.OnClick:=RedoButtonClick;
  AddChild(fRedoButton);

  fColorSelector:=TBDColorSelector.Create(320,fTop+6);
  fColorSelector.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fColorSelector.Name:='ColorSelector (PalEd)';
  AddChild(fColorSelector);

  fColorBox:=TBDColorBox.Create(COLORBOXLEFT,fTop+COLORBOXTOP);
  fColorBox.ColorIndex:=Settings.ActiveColorIndex;
  fColorBox.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fColorBox.Name:='ColorBox';
  AddChild(fColorBox);

  MouseObjects.Add(Self);
end;

destructor TBDPaletteEditor.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TBDPaletteEditor.Draw;
var i:integer;
begin
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(0,3,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height-3,OverlayImage.Palette[3]);
  fTexture.ARGBImage.Bar(
    PALETTESOCKETSLEFT,
    PALETTESOCKETSTOP,
    PALETTESOCKETWIDTH*32+3,
    PALETTESOCKETHEIGHT*8+3,
    OverlayImage.Palette.Colors[2]);
  if (Settings.ActiveColorIndex div 256)+1=fSliderBank.Position then
    fTexture.ARGBImage.bar(
      PALETTESOCKETSLEFT+(Settings.ActiveColorIndex mod 32)*PALETTESOCKETWIDTH,
      PALETTESOCKETSTOP+(Settings.ActiveColorIndex div 32)*PALETTESOCKETHEIGHT,
      PALETTESOCKETWIDTH+3,
      PALETTESOCKETHEIGHT+3,
      OverlayImage.Palette.Colors[5]);

  for i:=0 to 255 do begin
    fTexture.ARGBImage.Bar(
      PALETTESOCKETSLEFT+(i mod 32)*PALETTESOCKETWIDTH+3,
      PALETTESOCKETSTOP+(i div 32)*PALETTESOCKETHEIGHT+3,
      PALETTESOCKETWIDTH-3,
      PALETTESOCKETHEIGHT-3,
      MainImage.Palette.Colors[i+(fSliderBank.Position-1)*256]);
  end;
  fTexture.Update;
  PutTexture(fLeft,fTop,fTexture);
end;

procedure TBDPaletteEditor.MouseEnter(Sender:TObject);
begin
  InfoBar.ShowText('');
end;

procedure TBDPaletteEditor.MouseLeave(Sender:TObject);
begin
end;

procedure TBDPaletteEditor.MouseMove(Sender:TObject; x,y:integer);
begin
  x-=Left;
  y-=Top;
  if ActiveTool.Name='SELCOL' then begin
    if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
       (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
      x:=(x-PALETTESOCKETSLEFT) div PALETTESOCKETWIDTH;
      y:=(y-PALETTESOCKETSTOP) div PALETTESOCKETHEIGHT;

      TBDToolSelectColor(ActiveTool).SetColor(x+y*32+(fSliderBank.Position-1)*256);
    end else
      TBDToolSelectColor(ActiveTool).SetColor(-1);
  end else
  if ActiveTool.Name='PICKCOL' then begin
    if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
       (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
      x:=(x-PALETTESOCKETSLEFT) div PALETTESOCKETWIDTH;
      y:=(y-PALETTESOCKETSTOP) div PALETTESOCKETHEIGHT;

      TBDToolPickColor(ActiveTool).SetColor(x+y*32+(fSliderBank.Position-1)*256);
    end else
      TBDToolPickColor(ActiveTool).SetColor(-1);
  end;
end;

procedure TBDPaletteEditor.MouseDown(Sender:TObject; x,y,buttons:integer);
begin
  x-=Left;
  y-=Top;
  if ActiveTool.Name='SELCOL' then begin
    if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
       (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
      if buttons=SDL_BUTTON_LEFT then begin
        x:=(x-PALETTESOCKETSLEFT) div PALETTESOCKETWIDTH;
        y:=(y-PALETTESOCKETSTOP) div PALETTESOCKETHEIGHT;
        Settings.ActiveColorIndex:=y*32+x;
        fColorBox.ColorIndex:=y*32+x;
        RefreshSliders;
      end else
      if buttons=SDL_BUTTON_RIGHT then begin
        MessageQueue.AddMessage(MSG_DEACTIVATEPALETTEEDITOR);
      end;
    end;
  end else
  if ActiveTool.Name='PICKCOL' then begin
    if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
       (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
      TBDToolPickColor(ActiveTool).Click(x,y,buttons);
    end;
  end;
end;

procedure TBDPaletteEditor.MouseWheel(Sender:TObject; x,y,wheelx,wheely:integer);
begin
  x-=Left;
  y-=Top;
  if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
     (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
    fSliderBank.MouseWheel(fSliderBank,x,y,wheelx,wheely);
  end;
end;

procedure TBDPaletteEditor.OnSliderRChange(Sender:TObject; newValue:integer);
begin
  MainImage.Palette.ColorR[Settings.ActiveColorIndex]:=newValue;
  fColorBox.ColorChanged;
end;

procedure TBDPaletteEditor.OnSliderGChange(Sender:TObject; newValue:integer);
begin
  MainImage.Palette.ColorG[Settings.ActiveColorIndex]:=newValue;
  fColorBox.ColorChanged;
end;

procedure TBDPaletteEditor.OnSliderBChange(Sender:TObject; newValue:integer);
begin
  MainImage.Palette.ColorB[Settings.ActiveColorIndex]:=newValue;
  fColorBox.ColorChanged;
end;

procedure TBDPaletteEditor.OnSliderAChange(Sender:TObject; newValue:integer);
begin
  MainImage.Palette.ColorA[Settings.ActiveColorIndex]:=newValue;
  fColorBox.ColorChanged;
end;

procedure TBDPaletteEditor.OnSliderBankChange(Sender:TObject; newValue:integer);
begin

end;

procedure TBDPaletteEditor.OnColorSliderMouseDown(Sender:TObject;x,y,buttons:integer);
begin
  fSavedColor:=MainImage.Palette.Colors[Settings.ActiveColorIndex];
  TBDHorizontalSlider(Sender).MouseDown(Sender,x,y,buttons);
end;

procedure TBDPaletteEditor.OnColorSliderMouseUp(Sender:TObject;x,y,buttons:integer);
var tmp:uint32;
begin
  TBDHorizontalSlider(Sender).MouseUp(Sender,x,y,buttons);
  if fSavedColor<>MainImage.Palette.Colors[Settings.ActiveColorIndex] then begin
    tmp:=MainImage.Palette.Colors[Settings.ActiveColorIndex];
    MainImage.Palette.Colors[Settings.ActiveColorIndex]:=fSavedColor;
    PaletteUndoSystem.AddPaletteUndo(Settings.ActiveColorIndex,1);
    MainImage.Palette.Colors[Settings.ActiveColorIndex]:=tmp;
    PaletteUndoSystem.AddPaletteRedoToLastUndo(Settings.ActiveColorIndex,1);
    fColorBox.ColorChanged;
//    RefreshSliders;
  end;
end;

procedure TBDPaletteEditor.UndoButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  PaletteUndoSystem.Undo;
  fColorBox.ColorChanged;
  RefreshSliders;
end;

procedure TBDPaletteEditor.RedoButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  PaletteUndoSystem.Redo;
  fColorBox.ColorChanged;
  RefreshSliders;
end;

procedure TBDPaletteEditor.PaletteEditorShow(Sender:TObject);
begin
  inherited Show;
//  fSliderR.Visible:=true;
  RefreshSliders;
  ActiveTool:=Tools.ItemByName['SELCOL'];
end;

procedure TBDPaletteEditor.PaletteEditorHide(Sender:TObject);
begin
  inherited Hide;
end;

procedure TBDPaletteEditor.RefreshSliders;
begin
  fSliderR.Position:=MainImage.Palette.ColorR[Settings.ActiveColorIndex];
  fSliderG.Position:=MainImage.Palette.ColorG[Settings.ActiveColorIndex];
  fSliderB.Position:=MainImage.Palette.ColorB[Settings.ActiveColorIndex];
  fSliderA.Position:=MainImage.Palette.ColorA[Settings.ActiveColorIndex];
end;

function TBDPaletteEditor.ProcessMessage(msg:TMessage):boolean;
begin
  Result:=false;
  if Enabled then begin
    case msg.TypeID of
      MSG_ACTIVECOLORINDEXCHANGED:begin
        fColorBox.ColorIndex:=Settings.ActiveColorIndex;
        RefreshSliders;
        Result:=true;
      end;
      MSG_SETPALETTEUNDOREDOBUTTON:begin
        fUndoButton.Enabled:=PaletteUndoSystem.CanUndo;
        fRedoButton.Enabled:=PaletteUndoSystem.CanRedo;
      end;
      MSG_PICKEDCOLOR:begin
        fColorSelector.SetSelectedSlotTo(msg.DataInt);
        ActiveTool:=Tools.ItemByName['SELCOL'];
        InfoBar.ShowText('');
        Result:=true;
      end;
    end;
  end;
end;

end.

