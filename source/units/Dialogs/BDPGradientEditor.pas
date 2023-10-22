unit BDPGradientEditor;

{$mode Delphi}

interface

uses SysUtils, BDPModalDialog, BDPSimpleGradient, BDPCheckBox, BDPButton,
  BDPColorBox, BDPGradient, BDPSlider2;

type

  { TBDGradientEditor }

  TBDGradientEditor=class(TBDModalDialog)
    constructor Create;
    procedure SetColor(pTarget:integer;pColor:uint32);
  private
    fSimpleGradient:TBDSimpleGradient;
    fGradient:TGradient;
    fColorBoxes:array[1..5] of TBDColorBox;
    fCheckBoxes:array[3..5] of TBDCheckBox;
    fColorSliders:array[3..5] of TBDSlider2;
    procedure ColorBoxClick(Sender:TObject;x,y,buttons:integer);
    procedure CheckBoxChange(Sender:TObject);
    procedure SliderChange(Sender:TObject;value:integer);
  end;

implementation

uses BDPShared, MKMouse2, BDPMessage;

const
  GRADIENTEDITORWIDTH=640;
  GRADIENTEDITORHEIGHT=249;

{ TBDGradientEditor }

constructor TBDGradientEditor.Create;
const Helper:array[1..5] of integer=(PARM_COL_GRADEDIT_LEFT,PARM_COL_GRADEDIT_RIGHT,
        PARM_COL_GRADEDIT_COLOR3,PARM_COL_GRADEDIT_COLOR4,PARM_COL_GRADEDIT_COLOR5);
var i:integer;tmpB:TBDButton;

  procedure CreateColorBox(pIndex,pLeft,pTop:integer;pColor:uint32;pName:string;pTag:integer);
  begin
    fColorBoxes[pIndex]:=TBDColorBox.Create(pLeft,pTop,36,36);
    fColorBoxes[pIndex].Color:=pColor;
    fColorBoxes[pIndex].ZIndex:=MODALDIALOG_ZINDEX+1;
    fColorBoxes[pIndex].Name:=pName;
    fColorBoxes[pIndex].Tag:=pTag;
    fColorBoxes[pIndex].OnClick:=ColorBoxClick;
    AddChild(fColorBoxes[pIndex]);
  end;

  procedure CreateCheckBox(pIndex,pLeft,pTop:integer;pName:string;pTag:integer;pSelected:boolean);
  begin
    fCheckBoxes[pIndex]:=TBDCheckBox.Create(pLeft,pTop,27,27,'');
    fCheckBoxes[pIndex].ZIndex:=MODALDIALOG_ZINDEX+1;
    fCheckBoxes[pIndex].Name:=pName;
    fCheckBoxes[pIndex].OnChange:=CheckBoxChange;
    fCheckBoxes[pIndex].Tag:=pTag;
    fCheckBoxes[pIndex].Selected:=pSelected;
    AddChild(fCheckBoxes[pIndex]);
  end;

  procedure CreateColorSlider(pIndex,pLeft,pTop:integer;pName:string);
  begin
    fColorSliders[pIndex]:=TBDSlider2.Create(pLeft,pTop,520,36);
    fColorSliders[pIndex].Name:=pName;
    fColorSliders[pIndex].ZIndex:=MODALDIALOG_ZINDEX+1;
    fColorSliders[pIndex].MinValue:=0;
    fColorSliders[pIndex].MaxValue:=511;
    fColorSliders[pIndex].Position:=round(fGradient.ColorPositions[pIndex]*511);
    fColorSliders[pIndex].OnChange:=SliderChange;
    fColorSliders[pIndex].Tag:=pIndex;
    AddChild(fColorSliders[pIndex]);
  end;

begin
  inherited Create(GRADIENTEDITORWIDTH,GRADIENTEDITORHEIGHT);
  Caption:='GRADIENT EDITOR';
  fName:='GradientEditor';
  MouseObjects.Add(Self);
  fGradient:=Project.CurrentGradientList.ActiveGradient;
  fSimpleGradient:=TBDSimpleGradient.Create(fLeft+64,fTop+30,512,36,fGradient);
  fSimpleGradient.ZIndex:=MODALDIALOG_ZINDEX+1;
  fSimpleGradient.Name:='GDE Gradient';
  AddChild(fSimpleGradient);
  fGradient.LogContents;

  CreateColorBox(1,fLeft+58-3-36,fTop+30,fGradient.Colors[1],'GDE ColorBox 1',PARM_COL_GRADEDIT_LEFT);
  CreateColorBox(2,fLeft+58+512+12+3,fTop+30,fGradient.Colors[2],'GDE ColorBox 2',PARM_COL_GRADEDIT_RIGHT);

  for i:=3 to 5 do begin
    CreateCheckBox(i,fLeft+58-3-32,fTop+30+(36+9)*(i-2)+4,'GDE CheckBox '+inttostr(i),i,fGradient.ColorUsed[i]);
    CreateColorBox(i,fLeft+58+512+12+3,fTop+30+(36+9)*(i-2),fGradient.Colors[i],'GDE ColorBox '+inttostr(i),Helper[i]);
    CreateColorSlider(i,fLeft+60,fTop+30+(36+9)*(i-2),'GDE Slider '+inttostr(i));
  end;
//  i:=(GRADIENTEDITORWIDTH-(4*(normalbuttonwidth+9)-9)) div 2;
  i:=(GRADIENTEDITORWIDTH div 2-(2*normalbuttonwidth+9)) div 2;
  tmpB:=TBDButton.Create(fLeft+i,fTop+Height-NORMALBUTTONHEIGHT-12,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,'OK','SAVE GRADIENT');
  tmpB.ZIndex:=MODALDIALOG_ZINDEX+1;
  tmpB.Name:='GDE CloseButton';
  tmpB.Message:=TMessage.Init(MSG_GRADIENTEDITORRESPONSE,0,0);
  AddChild(tmpB);

  tmpB:=TBDButton.Create(fLeft+i+NORMALBUTTONWIDTH+9,fTop+Height-NORMALBUTTONHEIGHT-12,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,'CANCEL','CANCEL MODIFICATIONS');
  tmpB.ZIndex:=MODALDIALOG_ZINDEX+1;
  tmpB.Name:='GDE Cancel';
  tmpB.Message:=TMessage.Init(MSG_GRADIENTEDITORRESPONSE,0,0);
  AddChild(tmpB);

  i:=GRADIENTEDITORWIDTH div 2+(GRADIENTEDITORWIDTH div 2-(2*normalbuttonwidth+9)) div 2;
  tmpB:=TBDButton.Create(fLeft+i,fTop+Height-NORMALBUTTONHEIGHT-12,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,'UNDO','UNDO LAST CHANGE');
  tmpB.ZIndex:=MODALDIALOG_ZINDEX+1;
  tmpB.Name:='GDE Undo';
  tmpB.Message:=TMessage.Init(MSG_GRADIENTEDITORRESPONSE,0,0);
  AddChild(tmpB);

  tmpB:=TBDButton.Create(fLeft+i+NORMALBUTTONWIDTH+9,fTop+Height-NORMALBUTTONHEIGHT-12,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,'REDO','REDO LAST CHANGE');
  tmpB.ZIndex:=MODALDIALOG_ZINDEX+1;
  tmpB.Name:='GDE Redo';
  tmpB.Message:=TMessage.Init(MSG_GRADIENTEDITORRESPONSE,0,0);
  AddChild(tmpB);
end;

procedure TBDGradientEditor.SetColor(pTarget: integer; pColor: uint32);
begin
  if pColor<>POSTPROCESSCOLOR then begin
    case pTarget of
      PARM_COL_GRADEDIT_LEFT:begin
        fColorBoxes[1].Color:=pColor;
        fGradient.Colors[1]:=pColor;
      end;
      PARM_COL_GRADEDIT_RIGHT:begin
        fColorBoxes[2].Color:=pColor;
        fGradient.Colors[2]:=pColor;
      end;
      PARM_COL_GRADEDIT_COLOR3:begin
        fColorBoxes[3].Color:=pColor;
        fGradient.Colors[3]:=pColor;
      end;
      PARM_COL_GRADEDIT_COLOR4:begin
        fColorBoxes[4].Color:=pColor;
        fGradient.Colors[4]:=pColor;
      end;
      PARM_COL_GRADEDIT_COLOR5:begin
        fColorBoxes[5].Color:=pColor;
        fGradient.Colors[5]:=pColor;
      end;
    end;
    fSimpleGradient.Refresh;
  end;
end;

procedure TBDGradientEditor.ColorBoxClick(Sender: TObject; x, y, buttons: integer);
begin
  if Sender is TBDColorBox then begin
    MessageQueue.AddMessage(MSG_OPENCOLOREDITOR,TBDColorBox(Sender).Tag,TBDColorBox(Sender).Color);
    Self.Hide;
  end;
end;

procedure TBDGradientEditor.CheckBoxChange(Sender:TObject);
begin
  if Sender is TBDCheckBox then with Sender as TBDCheckBox do begin
    fGradient.ColorUsed[Tag]:=Selected;
    fSimpleGradient.Refresh;
  end;
end;

procedure TBDGradientEditor.SliderChange(Sender:TObject; value:integer);
begin
  if sender is TBDSlider2 then with Sender as TBDSlider2 do begin
    fGradient.ColorPositions[Tag]:=Position/511;
    fSimpleGradient.Refresh;
  end;
end;

end.

