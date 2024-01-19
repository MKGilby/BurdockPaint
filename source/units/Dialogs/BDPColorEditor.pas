{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPColorEditor;

{$mode Delphi}

interface

uses
  SysUtils, mk_sdl2, BDPMessage, BDPSliders,
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
    procedure SliderRGBChange(Sender:TObject;oldValue,newValue:integer);
    procedure SliderAChange(Sender:TObject;oldValue,newValue:integer);
    procedure SliderHSChange(Sender:TObject;oldValue,newValue:integer);
    procedure SliderLChange(Sender:TObject;oldValue,newValue:integer);
//    procedure UndoButtonClick(Sender:TObject;x,y,buttons:integer);
//    procedure RedoButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure HSBoxChange(Sender:TObject);
    procedure AlternateLSliderChange(Sender:TObject);
    procedure SelectClick(Sender:TObject;x,y,buttons:integer);
    procedure CancelClick(Sender:TObject;x,y,buttons:integer);
    procedure PaletteClick(Sender:TObject;x,y,buttons:integer);
    procedure ColorEditorShow(Sender:TObject);
    function KeyDown(Sender:TObject;key:integer):boolean;
    procedure RefreshHSLbyRGB;
    procedure RefreshRGBbyHSL;
    procedure RefreshColorBox;
  private
  end;

implementation

uses BDPShared, MKMouse2, MKToolbox, vcc2_SliderLogicStatic, BDPButton, sdl2;

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

  function CreateSlider(pLeft,pTop,pMaxValue:integer;pName:string;
    pOnChange:TValueChangeEvent):TBDHorizontalSlider;
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

  procedure CreateButton(pLeft,pTop:integer;pCaption,pHint,pName:string;pOnClick:TMouseButtonEvent=nil);
  var tmpB:TBDButton;
  begin
    tmpB:=TBDButton.Create(pLeft,pTop,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
      pCaption,pHint);
    with tmpB do begin
      Name:=pName;
      ZIndex:=MODALDIALOG_ZINDEX+1;
      OnClick:=pOnClick;
    end;
    AddChild(tmpB);
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

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP,
    'SELECT','SELECT THE COLOR SHOWN IN THE BOX.','Select Color',SelectClick);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+NORMALBUTTONHEIGHT+3,
    'CLOSE','CLOSE PALETTE EDITOR WITHOUT SELECTING COLOR.','Cancel Color',CancelClick);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+2*(NORMALBUTTONHEIGHT+3),
    'UNDO','UNDO LAST COLOR OPERATION.','Undo Color');

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+3*(NORMALBUTTONHEIGHT+3),
    'REDO','REDO LAST COLOR OPERATION.','Redo Color');

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+4*(NORMALBUTTONHEIGHT+3),
    'SAVE','SAVE COLOR PALETTE TO FILE.','Save Palette');

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+5*(NORMALBUTTONHEIGHT+3),
    'LOAD','LOAD COLOR PALETTE FROM FILE.','Load Palette');

  fColorBox:=TBDColorBox.Create(fLeft+COLORBOXLEFT,fTop+COLORBOXTOP,COLORBOXWIDTH,COLORBOXHEIGHT);
  fColorBox.Color:=Settings.ActiveColor;
  fColorBox.ZIndex:=MODALDIALOG_ZINDEX+1;
  fColorBox.Name:='ColorBox';
  AddChild(fColorBox);

  fColorPalette:=TBDColorPalette.Create(fLeft+COLORPALETTELEFT,fTop+COLORPALETTETOP,
    COLORPALETTEWIDTH,COLORPALETTEHEIGHT,16,16);
  fColorPalette.Palette:=Project.CurrentPalette;
  fColorPalette.ZIndex:=MODALDIALOG_ZINDEX+1;
  fColorPalette.Name:='ColorPalette';
  fColorPalette.OnClick:=PaletteClick;
  AddChild(fColorPalette);

  Visible:=false;
  MouseObjects.Add(Self);
  Refresh;
  fCalledFrom:=0;
end;

procedure TBDColorEditor.Redraw;
var i:integer;
begin
  inherited ReDraw;
  for i:=0 to 6 do
    MM.Fonts['Black'].OutText(fImage,'HSLRGBA'[i+1],SLIDERSLEFT-20,SLIDERSTOP+9+(NORMALSLIDERHEIGHT+3)*i,1);
end;

procedure TBDColorEditor.MouseEnter(Sender:TObject);
begin
  InfoBar.ShowText('');
end;

procedure TBDColorEditor.MouseLeave(Sender:TObject);
begin
end;

procedure TBDColorEditor.SliderRGBChange(Sender:TObject;
  oldValue,newValue:integer);
begin
  RefreshHSLbyRGB;
end;

procedure TBDColorEditor.SliderAChange(Sender:TObject; oldValue,newValue:integer
  );
begin
  RefreshColorBox;
end;

procedure TBDColorEditor.SliderHSChange(Sender:TObject;
  oldValue,newValue:integer);
begin
  fHSBox.SetColor(fSliderH.Position,fSliderS.Position);
  fAlternateLSlider.BaseColor:=fHSBox.Color;
  RefreshColorBox;
  RefreshRGBbyHSL;
end;

procedure TBDColorEditor.SliderLChange(Sender:TObject; oldValue,newValue:integer
  );
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
  MessageQueue.AddMessage(MSG_COLOREDITORRESP,fCalledFrom,fColorBox.Color);
  Self.Hide;
end;

procedure TBDColorEditor.CancelClick(Sender:TObject; x,y,buttons:integer);
begin
  MessageQueue.AddMessage(MSG_COLOREDITORRESP,fCalledFrom,POSTPROCESSCOLOR);
  Self.Hide;
end;

procedure TBDColorEditor.PaletteClick(Sender:TObject; x,y,buttons:integer);
var i:integer;
begin
  if Sender is TBDColorPalette then begin
    i:=(Sender as TBDColorPalette).GetPaletteIndexByCoords(x,y);
    if buttons=SDL_BUTTON_LEFT then begin
      Settings.ActiveColor:=Project.CurrentPalette.Colors[i];
    end
    else if buttons=SDL_BUTTON_RIGHT then begin
      Project.CurrentPalette.Colors[i]:=fColorBox.Color;
      fColorPalette.Refresh;
    end;
  end;
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
  if key=SDL_SCANCODE_ESCAPE then MessageQueue.AddMessage(MSG_COLOREDITORRESP,0,0);
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
      MSG_OPENCOLOREDITOR:begin
        fSliderA.Position:=(msg.DataUInt32 and $FF000000)>>24;
        fSliderR.Position:=(msg.DataUInt32 and $FF0000)>>16;
        fSliderG.Position:=(msg.DataUInt32 and $FF00)>>8;
        fSliderB.Position:=msg.DataUInt32 and $FF;
        RefreshHSLbyRGB;
        fCalledFrom:=msg.DataInt;
      end;
      MSG_ACTIVEIMAGECHANGED:begin
        fColorPalette.Palette:=Project.CurrentPalette;
      end;
      MSG_ACTIVECOLORCHANGED:begin
        if Visible then begin
          fSliderA.Position:=(msg.DataUInt32 and $FF000000)>>24;
          fSliderR.Position:=(msg.DataUInt32 and $FF0000)>>16;
          fSliderG.Position:=(msg.DataUInt32 and $FF00)>>8;
          fSliderB.Position:=msg.DataUInt32 and $FF;
          RefreshHSLbyRGB;
        end;
      end;
{      MSG_SETPALETTEUNDOREDOBUTTON:begin
        fUndoButton.Enabled:=Project.CurrentExtImage.PaletteUndo.CanUndo;
        fRedoButton.Enabled:=Project.CurrentExtImage.PaletteUndo.CanRedo;
      end;}
    end;
  end;
end;

end.

