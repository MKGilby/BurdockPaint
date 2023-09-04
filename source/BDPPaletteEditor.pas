{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of BurdockPaint.

  BurdockPaint is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  BurdockPaint is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  BurdockPaint. If not, see <https://www.gnu.org/licenses/>.
}

unit BDPPaletteEditor;

{$mode Delphi}

interface

uses
  SysUtils, mk_sdl2, vcc2_Container, BDPMessage, BDPSliders,
  BDPColorBox, BDPHSBox, BDPLightSlider;

type

  { TBDPaletteEditor }

  TBDPaletteEditor=class(TContainer)
    constructor Create;
    procedure Redraw; override;
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
    procedure OnSliderRGBChange(Sender:TObject;newValue:integer);
    procedure OnSliderAChange(Sender:TObject;newValue:integer);
    procedure OnSliderHSChange(Sender:TObject;newValue:integer);
    procedure OnSliderLChange(Sender:TObject;newValue:integer);
//    procedure OnSliderAChange(Sender:TObject;newValue:integer);
//    procedure UndoButtonClick(Sender:TObject;x,y,buttons:integer);
//    procedure RedoButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure HSBoxChange(Sender:TObject);
    procedure AlternateLSliderChange(Sender:TObject);
    procedure PaletteEditorShow(Sender:TObject);
    procedure PaletteEditorHide(Sender:TObject);
    procedure RefreshHSLbyRGB;
    procedure RefreshRGBbyHSL;
    procedure RefreshColorBox;
    function ProcessMessage(msg:TMessage):boolean;
  private
    fSliders:array[0..6] of TBDHorizontalSlider; // H S L R G B A
//    fSliderH,fSliderS,fSliderG
//    fSliderR,fSliderG,fSliderB,fSliderA:TBDHorizontalSlider;
    fAlternateLSlider:TBDLightSlider;
    fHSBox:TBDHSBox;
//    fUndoButton,fRedoButton:TBDButton;
    fColorBox:TBDColorBox;
    fSavedColor:uint32;
    fPickingColor:integer;
  end;

implementation

uses BDPShared, MKMouse2, MKToolbox, vcc2_SliderLogic;

const
  PALETTEEDITORHEIGHT=(NORMALSLIDERHEIGHT+3)*7+3+3;
  HSBOXLEFT=3;
  HSBOXTOP=6;
  HSBOXWIDTH=512+6;
  HSBOXHEIGHT=PALETTEEDITORHEIGHT-(NORMALSLIDERHEIGHT+3)-6-3-3;
  SLIDERSLEFT=HSBOXLEFT+HSBOXWIDTH+3+36;
  SLIDERSTOP=6;
  SLIDERSWIDTH=320;
  LIGHTSLIDERLEFT=HSBOXLEFT;
  LIGHTSLIDERTOP=HSBOXTOP+HSBOXHEIGHT+3;
  LIGHTSLIDERWIDTH=HSBOXWIDTH-(NORMALSLIDERHEIGHT+3);
  LIGHTSLIDERHEIGHT=NORMALSLIDERHEIGHT;
  COLORBOXLEFT=LIGHTSLIDERLEFT+LIGHTSLIDERWIDTH+3;
  COLORBOXTOP=LIGHTSLIDERTOP;
  COLORBOXWIDTH=NORMALSLIDERHEIGHT;
  COLORBOXHEIGHT=NORMALSLIDERHEIGHT;
//  PALETTESOCKETWIDTH=38;
//  PALETTESOCKETHEIGHT=26;
//  PALETTESOCKETSTOP=PALETTEEDITORHEIGHT-213;
//  PALETTESOCKETSLEFT=3;

{ TBDPaletteEditor }

constructor TBDPaletteEditor.Create;

  function CreateSlider(pLeft,pTop,pMaxValue:integer;pName:string;
    pOnChange:TOnSliderPositionChangeEvent):TBDHorizontalSlider;
  begin
    Result:=TBDHorizontalSlider.Create(pLeft,pTop,SLIDERSWIDTH,NORMALSLIDERHEIGHT);
    with Result do begin
      MinValue:=0;
      MaxValue:=pMaxValue;
      Position:=32;
      ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
      Name:=pName;
      OnChange:=pOnChange;
    end;
    AddChild(Result);
  end;

begin
  inherited Create;
  fLeft:=0;
  fTop:=WINDOWHEIGHT-PALETTEEDITORHEIGHT;
  Width:=WINDOWWIDTH;
  Height:=PALETTEEDITORHEIGHT;
  OnMouseEnter:=MouseEnter;
  OnMouseLeave:=MouseLeave;
//  OnMouseMove:=MouseMove;
//  OnMouseDown:=MouseDown;
//  OnMouseWheel:=MouseWheel;
  OnShow:=PaletteEditorShow;
  OnHide:=PaletteEditorHide;
  fName:='PaletteEditor';
  ZIndex:=LEVEL1CONTROLS_ZINDEX;

  fHSBox:=TBDHSBox.Create(fLeft+HSBOXLEFT,fTop+HSBOXTOP,HSBOXWIDTH,HSBOXHEIGHT);
  fHSBox.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fHSBox.Name:='HSBox';
  fHSBox.OnChange:=HSBoxChange;
  AddChild(fHSBox);

  fSliders[0]:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP,
    360,'H-Slider',OnSliderHSChange);
  fSliders[1]:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+NORMALSLIDERHEIGHT+3,
    100,'S-Slider',OnSliderHSChange);
  fSliders[2]:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+2*(NORMALSLIDERHEIGHT+3),
    100,'L-Slider',OnSliderLChange);
  fSliders[3]:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+3*(NORMALSLIDERHEIGHT+3),
    255,'R-Slider',OnSliderRGBChange);
  fSliders[4]:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+4*(NORMALSLIDERHEIGHT+3),
    255,'G-Slider',OnSliderRGBChange);
  fSliders[5]:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+5*(NORMALSLIDERHEIGHT+3),
    255,'B-Slider',OnSliderRGBChange);
  fSliders[6]:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+6*(NORMALSLIDERHEIGHT+3),
    255,'A-Slider',OnSliderAChange);

  fAlternateLSlider:=TBDLightSlider.Create(fLeft+LIGHTSLIDERLEFT,fTop+LIGHTSLIDERTOP,LIGHTSLIDERWIDTH,LIGHTSLIDERHEIGHT);
  fAlternateLSlider.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fAlternateLSlider.Name:='Alternate L-slider';
  fAlternateLSlider.OnChange:=AlternateLSliderChange;
  AddChild(fAlternateLSlider);

{  fUndoButton:=TBDButton.Create(
    PALETTEUNDOBUTTONSLEFT, fTop+PALETTEUNDOBUTTONSTOP,
    NORMALBUTTONWIDTH, PALETTEUNDOBUTTONHEIGHT,
    'UNDO','UNDO LAST PALETTE OPERATION');
  fUndoButton.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fUndoButton.Name:='Palette Undo';
  fUndoButton.OnClick:=UndoButtonClick;
  AddChild(fUndoButton);

  fRedoButton:=TBDButton.Create(
    PALETTEUNDOBUTTONSLEFT,fTop+PALETTEUNDOBUTTONSTOP+PALETTEUNDOBUTTONHEIGHT+3,
    NORMALBUTTONWIDTH,PALETTEREDOBUTTONHEIGHT,
    'REDO','REDO LAST UNDOED PALETTE OPERATION');
  fRedoButton.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fRedoButton.Name:='Palette Redo';
  fRedoButton.OnClick:=RedoButtonClick;
  AddChild(fRedoButton);}

  fColorBox:=TBDColorBox.Create(fLeft+COLORBOXLEFT,fTop+COLORBOXTOP,COLORBOXWIDTH,COLORBOXHEIGHT);
  fColorBox.Color:=Settings.ActiveColor;
  fColorBox.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fColorBox.Name:='ColorBox';
  AddChild(fColorBox);

  fPickingColor:=-1;

  Visible:=false;
  MouseObjects.Add(Self);
  Refresh;
end;

procedure TBDPaletteEditor.Redraw;
var i:integer;
begin
  if Assigned(fTexture) then begin
    // Panel background
    fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,SystemPalette[2]);
    // Panel top line
    fTexture.ARGBImage.Bar(0,3,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height-3,SystemPalette[3]);
    // Letters for sliders
    for i:=0 to 6 do
      MM.Fonts['Black'].OutText(fTexture.ARGBImage,'HSLRGBA'[i+1],SLIDERSLEFT-20,SLIDERSTOP+9+(NORMALSLIDERHEIGHT+3)*i,1);
    // Update texture
    fTexture.Update;
  end;
end;

procedure TBDPaletteEditor.MouseEnter(Sender:TObject);
begin
  InfoBar.ShowText('');
end;

procedure TBDPaletteEditor.MouseLeave(Sender:TObject);
begin
end;

procedure TBDPaletteEditor.OnSliderRGBChange(Sender:TObject; newValue:integer);
begin
  RefreshHSLbyRGB;
end;

procedure TBDPaletteEditor.OnSliderAChange(Sender:TObject; newValue:integer);
begin
  RefreshColorBox;
end;

procedure TBDPaletteEditor.OnSliderHSChange(Sender:TObject; newValue:integer);
begin
  fHSBox.SetColor(fSliders[0].Position,fSliders[1].Position);
  fAlternateLSlider.BaseColor:=fHSBox.Color;
  RefreshColorBox;
  RefreshRGBbyHSL;
end;

procedure TBDPaletteEditor.OnSliderLChange(Sender:TObject; newValue:integer);
begin
  fAlternateLSlider.L:=fSliders[2].Position;
  RefreshColorBox;
  RefreshRGBbyHSL;
end;

procedure TBDPaletteEditor.HSBoxChange(Sender:TObject);
begin
  fSliders[0].Position:=fHSBox.H;
  fSliders[1].Position:=fHSBox.S;
  fAlternateLSlider.BaseColor:=fHSBox.Color;
  fSliders[3].Position:=(fAlternateLSlider.Color and $ff0000)>>16;
  fSliders[4].Position:=(fAlternateLSlider.Color and $ff00)>>8;
  fSliders[5].Position:=fAlternateLSlider.Color and $ff;
  RefreshColorBox;
end;

procedure TBDPaletteEditor.AlternateLSliderChange(Sender:TObject);
begin
  fSliders[2].Position:=fAlternateLSlider.L;
  fSliders[3].Position:=(fAlternateLSlider.Color and $ff0000)>>16;
  fSliders[4].Position:=(fAlternateLSlider.Color and $ff00)>>8;
  fSliders[5].Position:=fAlternateLSlider.Color and $ff;
  RefreshColorBox;
end;

{procedure TBDPaletteEditor.OnColorSliderMouseDown(Sender:TObject;x,y,buttons:integer);
begin
  fSavedColor:=Project.CurrentPalette.Colors[Settings.ActiveColorIndex];
  TBDHorizontalSlider(Sender).MouseDown(Sender,x,y,buttons);
end;

procedure TBDPaletteEditor.OnColorSliderMouseUp(Sender:TObject;x,y,buttons:integer);
var tmp:uint32;
begin
  TBDHorizontalSlider(Sender).MouseUp(Sender,x,y,buttons);
  if fSavedColor<>Project.CurrentPalette.Colors[Settings.ActiveColorIndex] then begin
    tmp:=Project.CurrentImage.Palette.Colors[Settings.ActiveColorIndex];
    Project.CurrentImage.Palette.Colors[Settings.ActiveColorIndex]:=fSavedColor;
    Project.CurrentExtImage.PaletteUndo.AddSingleColorUndo(Settings.ActiveColorIndex);
    Project.CurrentImage.Palette.Colors[Settings.ActiveColorIndex]:=tmp;
    Project.CurrentExtImage.PaletteUndo.AddSingleColorRedoToLastUndo;
    fColorBox.ColorChanged;
  end;
end;}

{procedure TBDPaletteEditor.UndoButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Project.CurrentExtImage.PaletteUndo.Undo;
  fColorBox.ColorChanged;
  RefreshSliders;
end;

procedure TBDPaletteEditor.RedoButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Project.CurrentExtImage.PaletteUndo.Redo;
  fColorBox.ColorChanged;
  RefreshSliders;
end;}

procedure TBDPaletteEditor.PaletteEditorShow(Sender:TObject);
begin
  inherited Show;
  InfoBar.Top:=WINDOWHEIGHT-PALETTEEDITORHEIGHT-INFOBARHEIGHT;
  ActiveTool:=Tools.ItemByName['SELCOL'];
//  fUndoButton.Enabled:=Project.CurrentExtImage.PaletteUndo.CanUndo;
//  fRedoButton.Enabled:=Project.CurrentExtImage.PaletteUndo.CanRedo;
end;

procedure TBDPaletteEditor.PaletteEditorHide(Sender:TObject);
begin
  inherited Hide;
end;

procedure TBDPaletteEditor.RefreshHSLbyRGB;
var h:word;s,l:integer;
begin
  RGBtoHSL(fSliders[3].Position,fSliders[4].Position,fSliders[5].Position,h,s,l);
  fHSBox.SetColor(h,s);
  fAlternateLSlider.BaseColor:=fHSBox.Color;
  fAlternateLSlider.L:=l;
  fSliders[0].Position:=h;
  fSliders[1].Position:=s;
  fSliders[2].Position:=l;
  RefreshColorBox;
end;

procedure TBDPaletteEditor.RefreshRGBbyHSL;
var r,g,b:byte;
begin
  HSLtoRGB(fSliders[0].Position,fSliders[1].Position,fSliders[2].Position,r,g,b);
  fSliders[3].Position:=r;
  fSliders[4].Position:=g;
  fSliders[5].Position:=b;
end;

procedure TBDPaletteEditor.RefreshColorBox;
begin
  fColorBox.Color:=uint32(fSliders[6].Position)<<24+fAlternateLSlider.Color and $FFFFFF;
end;

function TBDPaletteEditor.ProcessMessage(msg:TMessage):boolean;
begin
  Result:=false;
  if Enabled then begin
{    case msg.TypeID of
      MSG_ACTIVECOLORINDEXCHANGED:begin
        fColorBox.Color:=Settings.ActiveColorIndex;
        RefreshSliders;
        fColorCluster.Refresh;
        Result:=true;
      end;
      MSG_SETPALETTEUNDOREDOBUTTON:begin
        fUndoButton.Enabled:=Project.CurrentExtImage.PaletteUndo.CanUndo;
        fRedoButton.Enabled:=Project.CurrentExtImage.PaletteUndo.CanRedo;
      end;
      MSG_COLORSELECTORPICKEDCOLOR:begin
        fColorSelector.SetSelectedSlotTo(msg.DataInt);
        ActiveTool:=Tools.ItemByName['SELCOL'];
        InfoBar.ShowText('');
        Result:=true;
      end;
      MSG_PALETTEPICKEDCOLOR:begin
        if msg.DataInt>-1 then
          Project.CurrentPalette[fPickingColor]:=msg.DataInt;
        fPickingColor:=-1;
        fColorCluster.Refresh;
        ActiveTool:=Tools.ItemByName['SELCOL'];
        InfoBar.ShowText('');
        Result:=true;
      end;
      MSG_ACTIVATEPICKCOLORCLUSTER:begin
        ActiveTool:=Tools.ItemByName['PICKCOLCLS'];
        Result:=true;
      end;
      MSG_COLORCLUSTERPICKED:begin
        if msg.DataInt>-1 then begin
          fColorCluster.ColorCluster.StartIndex:=(msg.DataInt and $7FFF0000)>>16;
          fColorCluster.ColorCluster.EndIndex:=msg.DataInt and $7FFF;
        end;
        fColorCluster.Refresh;
        ActiveTool:=Tools.ItemByName['SELCOL'];
        InfoBar.ShowText('');
        Result:=true;
      end;
      MSG_ACTIVECOLORCLUSTERCHANGED:begin
        fColorCluster.ColorCluster:=Project.CurrentColorClusters.ActiveColorCluster;
      end;
    end;}
  end;
end;

end.

