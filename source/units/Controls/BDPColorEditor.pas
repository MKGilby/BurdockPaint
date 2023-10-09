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

unit BDPColorEditor;

{$mode Delphi}

interface

uses
  SysUtils, mk_sdl2, vcc2_Container, BDPMessage, BDPSliders,
  BDPColorBox, BDPHSBox, BDPLightSlider, BDPColorPalette, BDPColorPaletteUndo,
  BDPModalDialog;

type

  { TBDColorEditor }

  TBDColorEditor=class(TBDModalDialog)
    constructor Create;
    function ProcessMessage(msg:TMessage):boolean;
  protected
    procedure Redraw; override;
  private
    fSliderH,fSliderS,fSliderL,
    fSliderR,fSliderG,fSliderB,fSliderA:TBDHorizontalSlider;
    fAlternateLSlider:TBDLightSlider;
    fColorPalette:TBDColorPalette;
    fHSBox:TBDHSBox;
    fColorBox:TBDColorBox;
    fCalledFrom:integer;
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
    procedure SliderRGBChange(Sender:TObject;newValue:integer);
    procedure SliderAChange(Sender:TObject;newValue:integer);
    procedure SliderHSChange(Sender:TObject;newValue:integer);
    procedure SliderLChange(Sender:TObject;newValue:integer);
//    procedure UndoButtonClick(Sender:TObject;x,y,buttons:integer);
//    procedure RedoButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure HSBoxChange(Sender:TObject);
    procedure AlternateLSliderChange(Sender:TObject);
    procedure SelectClick(Sender:TObject;x,y,buttons:integer);
    procedure CancelClick(Sender:TObject;x,y,buttons:integer);
    procedure ColorEditorShow(Sender:TObject);
    function KeyDown(Sender:TObject;key:integer):boolean;
    procedure RefreshHSLbyRGB;
    procedure RefreshRGBbyHSL;
    procedure RefreshColorBox;
  private
  end;

implementation

uses BDPShared, MKMouse2, MKToolbox, vcc2_SliderLogic, BDPButton, sdl2;

const
  HSBOXLEFT=3+9;
  HSBOXTOP=MODALDIALOGCAPTIONHEIGHT+9;
  HSBOXWIDTH=512+6;
  SLIDERSLEFT=HSBOXLEFT+HSBOXWIDTH+36;
  SLIDERSTOP=MODALDIALOGCAPTIONHEIGHT+9;
  SLIDERSWIDTH=320;
  SLIDERSHEIGHT=7*(NORMALSLIDERHEIGHT+3)-3;
  HSBOXHEIGHT=SLIDERSHEIGHT-(NORMALSLIDERHEIGHT+3);
  LIGHTSLIDERLEFT=HSBOXLEFT;
  LIGHTSLIDERTOP=HSBOXTOP+HSBOXHEIGHT+3;
  LIGHTSLIDERWIDTH=HSBOXWIDTH;
  LIGHTSLIDERHEIGHT=NORMALSLIDERHEIGHT;

  COLOREDITORWIDTH=3+9+HSBOXWIDTH+36+SLIDERSWIDTH+9+3;

  COLORPALETTELEFT=3+9;
  COLORPALETTETOP=MODALDIALOGCAPTIONHEIGHT+SLIDERSHEIGHT+9+9;
  COLORPALETTEWIDTH=COLOREDITORWIDTH-NORMALBUTTONWIDTH-9-9-3-3 -8;
  COLORPALETTEHEIGHT=340;

  COLOREDITORHEIGHT=MODALDIALOGCAPTIONHEIGHT+9+(NORMALSLIDERHEIGHT+3)*7-3+9+COLORPALETTEHEIGHT+9+3;

  BUTTONSLEFT=COLOREDITORWIDTH-3-9-NORMALBUTTONWIDTH;
  BUTTONSTOP=COLOREDITORHEIGHT-6*(NORMALBUTTONHEIGHT+3)-6;
  COLORBOXLEFT=BUTTONSLEFT;
  COLORBOXTOP=COLORPALETTETOP;
  COLORBOXWIDTH=NORMALBUTTONWIDTH;
  COLORBOXHEIGHT=BUTTONSTOP-COLORPALETTETOP-3;




{ TBDColorEditor }

constructor TBDColorEditor.Create;
var atmB:TBDButton;

  function CreateSlider(pLeft,pTop,pMaxValue:integer;pName:string;
    pOnChange:TOnSliderPositionChangeEvent):TBDHorizontalSlider;
  begin
    Result:=TBDHorizontalSlider.Create(pLeft,pTop,SLIDERSWIDTH,NORMALSLIDERHEIGHT);
    with Result do begin
      MinValue:=0;
      MaxValue:=pMaxValue;
      Position:=32;
      ZIndex:=MODALDIALOG_ZINDEX+1;
      Name:=pName;
      OnChange:=pOnChange;
    end;
    AddChild(Result);
  end;

begin
  inherited Create(COLOREDITORWIDTH,COLOREDITORHEIGHT);
  OnMouseEnter:=MouseEnter;
  OnMouseLeave:=MouseLeave;
  OnShow:=ColorEditorShow;
  OnKeyDown:=KeyDown;
  fName:='ColorEditor';
  Caption:='COLOR EDITOR';
  ZIndex:=MODALDIALOG_ZINDEX;

  fHSBox:=TBDHSBox.Create(fLeft+HSBOXLEFT,fTop+HSBOXTOP,HSBOXWIDTH,HSBOXHEIGHT);
  fHSBox.ZIndex:=MODALDIALOG_ZINDEX+1;
  fHSBox.Name:='HSBox';
  fHSBox.OnChange:=HSBoxChange;
  AddChild(fHSBox);

  fSliderH:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP,
    360,'H-Slider',SliderHSChange);
  fSliderS:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+NORMALSLIDERHEIGHT+3,
    100,'S-Slider',SliderHSChange);
  fSliderL:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+2*(NORMALSLIDERHEIGHT+3),
    100,'L-Slider',SliderLChange);
  fSliderR:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+3*(NORMALSLIDERHEIGHT+3),
    255,'R-Slider',SliderRGBChange);
  fSliderG:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+4*(NORMALSLIDERHEIGHT+3),
    255,'G-Slider',SliderRGBChange);
  fSliderB:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+5*(NORMALSLIDERHEIGHT+3),
    255,'B-Slider',SliderRGBChange);
  fSliderA:=CreateSlider(fLeft+SLIDERSLEFT,fTop+SLIDERSTOP+6*(NORMALSLIDERHEIGHT+3),
    255,'A-Slider',SliderAChange);

  fAlternateLSlider:=TBDLightSlider.Create(fLeft+LIGHTSLIDERLEFT,fTop+LIGHTSLIDERTOP,LIGHTSLIDERWIDTH,LIGHTSLIDERHEIGHT);
  fAlternateLSlider.ZIndex:=MODALDIALOG_ZINDEX+1;
  fAlternateLSlider.Name:='Alternate L-slider';
  fAlternateLSlider.OnChange:=AlternateLSliderChange;
  AddChild(fAlternateLSlider);

  atmB:=TBDButton.Create(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'SELECT','SELECT THE COLOR SHOWN IN THE BOX.');
  with atmB do begin
    Name:='Select Color';
    ZIndex:=MODALDIALOG_ZINDEX+1;
    OnClick:=SelectClick;
  end;
  AddChild(atmB);

  atmB:=TBDButton.Create(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+NORMALBUTTONHEIGHT+3,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'CANCEL','CLOSE PALETTE EDITOR WITHOUT SELECTING COLOR.');
  with atmB do begin
    Name:='Cancel Color';
    ZIndex:=MODALDIALOG_ZINDEX+1;
    OnClick:=CancelClick;
  end;
  AddChild(atmB);

  atmB:=TBDButton.Create(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+2*(NORMALBUTTONHEIGHT+3),NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'UNDO','UNDO LAST COLOR OPERATION.');
  with atmB do begin
    Name:='Undo Color';
    ZIndex:=MODALDIALOG_ZINDEX+1;
  end;
  AddChild(atmB);

  atmB:=TBDButton.Create(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+3*(NORMALBUTTONHEIGHT+3),NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'REDO','REDO LAST COLOR OPERATION.');
  with atmB do begin
    Name:='Redo Color';
    ZIndex:=MODALDIALOG_ZINDEX+1;
  end;
  AddChild(atmB);

  atmB:=TBDButton.Create(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+4*(NORMALBUTTONHEIGHT+3),NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'SAVE','SAVE COLOR PALETTE TO FILE.');
  with atmB do begin
    Name:='Save Palette';
    ZIndex:=MODALDIALOG_ZINDEX+1;
  end;
  AddChild(atmB);

  atmB:=TBDButton.Create(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+5*(NORMALBUTTONHEIGHT+3),NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'LOAD','LOAD COLOR PALETTE FROM FILE.');
  with atmB do begin
    Name:='Load Palette';
    ZIndex:=MODALDIALOG_ZINDEX+1;
  end;
  AddChild(atmB);

  fColorBox:=TBDColorBox.Create(fLeft+COLORBOXLEFT,fTop+COLORBOXTOP,COLORBOXWIDTH,COLORBOXHEIGHT);
  fColorBox.Color:=Settings.ActiveColor;
  fColorBox.ZIndex:=MODALDIALOG_ZINDEX+1;
  fColorBox.Name:='ColorBox';
  AddChild(fColorBox);

  fColorPalette:=TBDColorPalette.Create(fLeft+COLORPALETTELEFT,fTop+COLORPALETTETOP,
    COLORPALETTEWIDTH,COLORPALETTEHEIGHT);
  fColorPalette.Palette:=Project.CurrentPalette;
  fColorPalette.ZIndex:=MODALDIALOG_ZINDEX+1;
  fColorPalette.Name:='ColorPalette';
  AddChild(fColorPalette);

//  fPickingColor:=-1;

  Visible:=false;
  MouseObjects.Add(Self);
  Refresh;
  fCalledFrom:=0;
end;

procedure TBDColorEditor.Redraw;
var i:integer;
begin
  inherited ReDraw;
  if Assigned(fTexture) then begin
    // Letters for sliders
    for i:=0 to 6 do
      MM.Fonts['Black'].OutText(fTexture.ARGBImage,'HSLRGBA'[i+1],SLIDERSLEFT-20,SLIDERSTOP+9+(NORMALSLIDERHEIGHT+3)*i,1);
    // Update texture
    fTexture.Update;
  end;
end;

procedure TBDColorEditor.MouseEnter(Sender:TObject);
begin
  InfoBar.ShowText('');
end;

procedure TBDColorEditor.MouseLeave(Sender:TObject);
begin
end;

procedure TBDColorEditor.SliderRGBChange(Sender:TObject; newValue:integer);
begin
  RefreshHSLbyRGB;
end;

procedure TBDColorEditor.SliderAChange(Sender:TObject; newValue:integer);
begin
  RefreshColorBox;
end;

procedure TBDColorEditor.SliderHSChange(Sender:TObject; newValue:integer);
begin
  fHSBox.SetColor(fSliderH.Position,fSliderS.Position);
  fAlternateLSlider.BaseColor:=fHSBox.Color;
  RefreshColorBox;
  RefreshRGBbyHSL;
end;

procedure TBDColorEditor.SliderLChange(Sender:TObject; newValue:integer);
begin
  fAlternateLSlider.L:=fSliderL.Position;
  RefreshColorBox;
  RefreshRGBbyHSL;
end;

procedure TBDColorEditor.HSBoxChange(Sender:TObject);
begin
  fSliderH.Position:=fHSBox.H;
  fSliderS.Position:=fHSBox.S;
  fAlternateLSlider.BaseColor:=fHSBox.Color;
  fSliderR.Position:=(fAlternateLSlider.Color and $ff0000)>>16;
  fSliderG.Position:=(fAlternateLSlider.Color and $ff00)>>8;
  fSliderB.Position:=fAlternateLSlider.Color and $ff;
  RefreshColorBox;
end;

procedure TBDColorEditor.AlternateLSliderChange(Sender:TObject);
begin
  fSliderL.Position:=fAlternateLSlider.L;
  fSliderR.Position:=(fAlternateLSlider.Color and $ff0000)>>16;
  fSliderG.Position:=(fAlternateLSlider.Color and $ff00)>>8;
  fSliderB.Position:=fAlternateLSlider.Color and $ff;
  RefreshColorBox;
end;

procedure TBDColorEditor.SelectClick(Sender:TObject; x,y,buttons:integer);
begin
  MessageQueue.AddMessage(MSG_DEACTIVATECOLOREDITOR,fCalledFrom,fColorBox.Color);
end;

procedure TBDColorEditor.CancelClick(Sender:TObject; x,y,buttons:integer);
begin
  MessageQueue.AddMessage(MSG_DEACTIVATECOLOREDITOR,0,0);
end;

{procedure TBDColorEditor.UndoButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Project.CurrentExtImage.PaletteUndo.Undo;
  fColorBox.ColorChanged;
  RefreshSliders;
end;

procedure TBDColorEditor.RedoButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Project.CurrentExtImage.PaletteUndo.Redo;
  fColorBox.ColorChanged;
  RefreshSliders;
end;}

procedure TBDColorEditor.ColorEditorShow(Sender:TObject);
begin
  inherited Show;
//  InfoBar.Top:=WINDOWHEIGHT-COLOREDITORHEIGHT-INFOBARHEIGHT;
  InfoBar.Top:=0;
//  fUndoButton.Enabled:=Project.CurrentExtImage.PaletteUndo.CanUndo;
//  fRedoButton.Enabled:=Project.CurrentExtImage.PaletteUndo.CanRedo;
end;

function TBDColorEditor.KeyDown(Sender: TObject; key: integer): boolean;
begin
  if key=SDL_SCANCODE_ESCAPE then MessageQueue.AddMessage(MSG_DEACTIVATECOLOREDITOR,0,0);
  Result:=true;
end;

procedure TBDColorEditor.RefreshHSLbyRGB;
var h:word;s,l:integer;
begin
  RGBtoHSL(fSliderR.Position,fSliderG.Position,fSliderB.Position,h,s,l);
  fHSBox.SetColor(h,s);
  fAlternateLSlider.BaseColor:=fHSBox.Color;
  fAlternateLSlider.L:=l;
  fSliderH.Position:=h;
  fSliderS.Position:=s;
  fSliderL.Position:=l;
  RefreshColorBox;
end;

procedure TBDColorEditor.RefreshRGBbyHSL;
var r,g,b:byte;
begin
  HSLtoRGB(fSliderH.Position,fSliderS.Position,fSliderL.Position,r,g,b);
  fSliderR.Position:=r;
  fSliderG.Position:=g;
  fSliderB.Position:=b;
end;

procedure TBDColorEditor.RefreshColorBox;
begin
  fColorBox.Color:=
    uint32(fSliderA.Position)<<24+
    uint32(fSliderR.Position and $FF)<<16+
    uint32(fSliderG.Position and $FF)<<8+
    uint32(fSliderB.Position and $FF);
end;

function TBDColorEditor.ProcessMessage(msg:TMessage):boolean;
begin
  Result:=false;
  if Enabled then begin
    case msg.TypeID of
      MSG_ACTIVATECOLOREDITOR:begin
        fSliderA.Position:=(msg.DataUInt32 and $FF000000)>>24;
        fSliderR.Position:=(msg.DataUInt32 and $FF0000)>>16;
        fSliderG.Position:=(msg.DataUInt32 and $FF00)>>8;
        fSliderB.Position:=msg.DataUInt32 and $FF;
        RefreshHSLbyRGB;
        fCalledFrom:=msg.DataInt;
      end;
      MSG_ACTIVECOLORCHANGED:begin
        fSliderA.Position:=(msg.DataUInt32 and $FF000000)>>24;
        fSliderR.Position:=(msg.DataUInt32 and $FF0000)>>16;
        fSliderG.Position:=(msg.DataUInt32 and $FF00)>>8;
        fSliderB.Position:=msg.DataUInt32 and $FF;
        RefreshHSLbyRGB;
      end;
      MSG_PALETTEREQUESTCOLOR:begin
        fColorPalette.SetColor(msg.DataInt,fColorBox.Color);
      end;
{      MSG_SETPALETTEUNDOREDOBUTTON:begin
        fUndoButton.Enabled:=Project.CurrentExtImage.PaletteUndo.CanUndo;
        fRedoButton.Enabled:=Project.CurrentExtImage.PaletteUndo.CanRedo;
      end;}
    end;
  end;
end;

end.

