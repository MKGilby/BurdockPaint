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
//    procedure MouseMove(Sender:TObject;x,y:integer);
//    procedure MouseDown(Sender:TObject;x,y,buttons:integer);
//    procedure MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer);
//    procedure OnSliderRChange(Sender:TObject;newValue:integer);
//    procedure OnSliderGChange(Sender:TObject;newValue:integer);
//    procedure OnSliderBChange(Sender:TObject;newValue:integer);
//    procedure OnSliderAChange(Sender:TObject;newValue:integer);
//    procedure OnSliderBankChange(Sender:TObject;newValue:integer);
//    procedure UndoButtonClick(Sender:TObject;x,y,buttons:integer);
//    procedure RedoButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure PaletteEditorShow(Sender:TObject);
    procedure PaletteEditorHide(Sender:TObject);
    procedure RefreshSliders;
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

uses BDPShared, MKMouse2, BDPTools;

const
  PALETTEEDITORHEIGHT=(NORMALSLIDERHEIGHT+3)*7+3+3;
  HSBOXLEFT=3;
  HSBOXTOP=6;
  HSBOXWIDTH=512+6;
  HSBOXHEIGHT=PALETTEEDITORHEIGHT-(NORMALSLIDERHEIGHT+3)-6-3;
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
const SliderLetters='HSLRGBA';
var i:integer;
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
  AddChild(fHSBox);

  for i:=0 to 6 do begin
    fSliders[i]:=TBDHorizontalSlider.Create(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+i*(NORMALSLIDERHEIGHT+3),SLIDERSWIDTH,NORMALSLIDERHEIGHT);
    with fSliders[i] do begin
      MinValue:=0;MaxValue:=255;Position:=32;
      ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
      Name:=SliderLetters[i+1]+'-slider';
//      OnChange:=OnSliderRChange;
    end;
    AddChild(fSliders[i]);
  end;

  fAlternateLSlider:=TBDLightSlider.Create(fLeft+LIGHTSLIDERLEFT,fTop+LIGHTSLIDERTOP,LIGHTSLIDERWIDTH,LIGHTSLIDERHEIGHT);
  fAlternateLSlider.ZIndex:=LEVEL1CONTROLS_ZINDEX+1;
  fAlternateLSlider.Name:='Alternate L-slider';
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

{procedure TBDPaletteEditor.MouseMove(Sender:TObject; x,y:integer);
begin
  x-=Left;
  y-=Top;
  if ActiveTool.Name='SELCOL' then begin
    if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
       (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
      x:=(x-PALETTESOCKETSLEFT) div PALETTESOCKETWIDTH;
      y:=(y-PALETTESOCKETSTOP) div PALETTESOCKETHEIGHT;
      TBDToolSelectColor(ActiveTool).SetColor(x+y*32+(fSliderBank.Position-1)*256,'PICK COLOR');
    end else
      TBDToolSelectColor(ActiveTool).SetColor(-1);
  end else
  if ActiveTool.Name='PICKCOLCS' then begin
    if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
       (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
      x:=(x-PALETTESOCKETSLEFT) div PALETTESOCKETWIDTH;
      y:=(y-PALETTESOCKETSTOP) div PALETTESOCKETHEIGHT;
      TBDToolPickColorCOLSEL(ActiveTool).SetColor(x+y*32+(fSliderBank.Position-1)*256);
    end else
      TBDToolPickColorCOLSEL(ActiveTool).SetColor(-1);
  end else
  if ActiveTool.Name='PICKCOLP' then begin
    if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
       (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
      x:=(x-PALETTESOCKETSLEFT) div PALETTESOCKETWIDTH;
      y:=(y-PALETTESOCKETSTOP) div PALETTESOCKETHEIGHT;
      TBDToolPickColorPAL(ActiveTool).SetColor(x+y*32+(fSliderBank.Position-1)*256);
    end else
      TBDToolPickColorPAL(ActiveTool).SetColor(-1);
  end else
  if ActiveTool.Name='PICKCOLCLS' then begin
    if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
       (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
      x:=(x-PALETTESOCKETSLEFT) div PALETTESOCKETWIDTH;
      y:=(y-PALETTESOCKETSTOP) div PALETTESOCKETHEIGHT;
      TBDToolPickColorCluster(ActiveTool).SetColor(x+y*32+(fSliderBank.Position-1)*256);
    end else
      TBDToolPickColorCluster(ActiveTool).SetColor(-1);
  end;
end;

procedure TBDPaletteEditor.MouseDown(Sender:TObject; x,y,buttons:integer);
begin
  x-=Left;
  y-=Top;
  if ActiveTool.Name='SELCOL' then begin
    if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
       (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
      x:=(x-PALETTESOCKETSLEFT) div PALETTESOCKETWIDTH;
      y:=(y-PALETTESOCKETSTOP) div PALETTESOCKETHEIGHT;
      if buttons=SDL_BUTTON_LEFT then begin
        Settings.ActiveColor:=Project.CurrentPalette.Colors[x+y*32+(fSliderBank.Position-1)*256];
        fColorBox.Color:=Settings.ActiveColor;
        fColorCluster.Refresh;
        RefreshSliders;
      end else
      if buttons=SDL_BUTTON_RIGHT then begin
        ActiveTool:=Tools.ItemByName['PICKCOLP'];
        fPickingColor:=y*32+x;
      end;
    end;
  end else
  if ActiveTool.Name='PICKCOLCS' then begin
    if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
       (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
      TBDToolPickColorCOLSEL(ActiveTool).Click(x,y,buttons);
    end;
  end else
  if ActiveTool.Name='PICKCOLP' then begin
    if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
       (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
      TBDToolPickColorPAL(ActiveTool).Click(x,y,buttons);
    end;
  end else
  if ActiveTool.Name='PICKCOLCLS' then begin
    if (x>=PALETTESOCKETSLEFT) and (x<PALETTESOCKETSLEFT+PALETTESOCKETWIDTH*32+3) and
       (y>=PALETTESOCKETSTOP) and (y<PALETTESOCKETSTOP+PALETTESOCKETHEIGHT*8+3) then begin
      TBDToolPickColorCluster(ActiveTool).Click(x,y,buttons);
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
end;}

{procedure TBDPaletteEditor.OnSliderRChange(Sender:TObject; newValue:integer);
begin
  Project.CurrentPalette.ColorR[Settings.ActiveColorIndex]:=newValue;
  fColorBox.ColorChanged;
  fColorCluster.Refresh;
end;

procedure TBDPaletteEditor.OnSliderGChange(Sender:TObject; newValue:integer);
begin
  Project.CurrentPalette.ColorG[Settings.ActiveColorIndex]:=newValue;
  fColorBox.ColorChanged;
  fColorCluster.Refresh;
end;

procedure TBDPaletteEditor.OnSliderBChange(Sender:TObject; newValue:integer);
begin
  Project.CurrentPalette.ColorB[Settings.ActiveColorIndex]:=newValue;
  fColorBox.ColorChanged;
  fColorCluster.Refresh;
end;

procedure TBDPaletteEditor.OnSliderAChange(Sender:TObject; newValue:integer);
begin
  Project.CurrentPalette.ColorA[Settings.ActiveColorIndex]:=newValue;
  fColorBox.ColorChanged;
  fColorCluster.Refresh;
end;}

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
  RefreshSliders;
  ActiveTool:=Tools.ItemByName['SELCOL'];
//  fUndoButton.Enabled:=Project.CurrentExtImage.PaletteUndo.CanUndo;
//  fRedoButton.Enabled:=Project.CurrentExtImage.PaletteUndo.CanRedo;
end;

procedure TBDPaletteEditor.PaletteEditorHide(Sender:TObject);
begin
  inherited Hide;
end;

procedure TBDPaletteEditor.RefreshSliders;
begin
{  fSliderR.Position:=Project.CurrentImage.Palette.ColorR[Settings.ActiveColorIndex];
  fSliderG.Position:=Project.CurrentImage.Palette.ColorG[Settings.ActiveColorIndex];
  fSliderB.Position:=Project.CurrentImage.Palette.ColorB[Settings.ActiveColorIndex];
  fSliderA.Position:=Project.CurrentImage.Palette.ColorA[Settings.ActiveColorIndex];}
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

