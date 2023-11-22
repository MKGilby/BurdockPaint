{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPGradientEditor;

{$mode Delphi}

interface

uses SysUtils, BDPModalDialog, BDPSimpleGradient, BDPCheckBox, BDPButton,
  BDPColorBox, BDPGradient, BDPSlider2, BDPGradientEditorUndo, BDPMessage;

type

  { TBDGradientEditor }

  TBDGradientEditor=class(TBDModalDialog)
    constructor Create;
    destructor Destroy; override;
    procedure SetColor(pTarget:integer;pColor:uint32);
    function ProcessMessage(msg:TMessage):boolean;
  private
    fSimpleGradient:TBDSimpleGradient;
    fColorBoxes:array[1..5] of TBDColorBox;
    fCheckBoxes:array[3..5] of TBDCheckBox;
    fColorSliders:array[3..5] of TBDSlider2;
    fUndoButton,
    fRedoButton:TBDButton;
    fUndoSystem:TBDGradientEditorUndoSystem;
    fCalledFrom:integer;
    fCalledFromIndex:uint32;
    procedure ColorBoxClick(Sender:TObject;x,y,buttons:integer);
    procedure CheckBoxChange(Sender:TObject);
    procedure SliderChange(Sender:TObject;value:integer);
    procedure OKClick(Sender:TObject;x,y,buttons:integer);
    procedure CancelClick(Sender:TObject;x,y,buttons:integer);
    procedure UndoClick(Sender:TObject;x,y,buttons:integer);
    procedure RedoClick(Sender:TObject;x,y,buttons:integer);
    procedure RefreshControls(pKnobsToo:boolean=false);
    procedure Show(Sender:TObject);
  end;

implementation

uses BDPShared, MKMouse2;

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
    fColorSliders[pIndex].Position:=round(GradientEditorGradient.ColorPositions[pIndex]*511);
    fColorSliders[pIndex].OnChange:=SliderChange;
    fColorSliders[pIndex].Tag:=pIndex;
    AddChild(fColorSliders[pIndex]);
  end;

begin
  inherited Create(GRADIENTEDITORWIDTH,GRADIENTEDITORHEIGHT);
  Caption:='GRADIENT EDITOR';
  fName:='GradientEditor';
  OnShow:=Show;
  MouseObjects.Add(Self);
//  fGradient:=Project.CurrentGradientList.ActiveGradient;
  fSimpleGradient:=TBDSimpleGradient.Create(fLeft+64,fTop+30,512,36,GradientEditorGradient);
  fSimpleGradient.ZIndex:=MODALDIALOG_ZINDEX+1;
  fSimpleGradient.Name:='GDE Gradient';
  AddChild(fSimpleGradient);
//  fGradient.LogContents;

  CreateColorBox(1,fLeft+58-3-36,fTop+30,GradientEditorGradient.Colors[1],'GDE ColorBox 1',PARM_COL_GRADEDIT_LEFT);
  CreateColorBox(2,fLeft+58+512+12+3,fTop+30,GradientEditorGradient.Colors[2],'GDE ColorBox 2',PARM_COL_GRADEDIT_RIGHT);

  for i:=3 to 5 do begin
    CreateCheckBox(i,fLeft+58-3-32,fTop+30+(36+9)*(i-2)+4,'GDE CheckBox '+inttostr(i),i,GradientEditorGradient.ColorUsed[i]);
    CreateColorBox(i,fLeft+58+512+12+3,fTop+30+(36+9)*(i-2),GradientEditorGradient.Colors[i],'GDE ColorBox '+inttostr(i),Helper[i]);
    CreateColorSlider(i,fLeft+60,fTop+30+(36+9)*(i-2),'GDE Slider '+inttostr(i));
  end;

  i:=(GRADIENTEDITORWIDTH div 2-(2*normalbuttonwidth+9)) div 2;
  tmpB:=TBDButton.Create(fLeft+i,fTop+Height-NORMALBUTTONHEIGHT-12,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,'OK','SAVE GRADIENT');
  tmpB.ZIndex:=MODALDIALOG_ZINDEX+1;
  tmpB.Name:='GDE CloseButton';
  tmpB.OnClick:=OKClick;
  AddChild(tmpB);

  tmpB:=TBDButton.Create(fLeft+i+NORMALBUTTONWIDTH+9,fTop+Height-NORMALBUTTONHEIGHT-12,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,'CANCEL','CANCEL MODIFICATIONS');
  tmpB.ZIndex:=MODALDIALOG_ZINDEX+1;
  tmpB.Name:='GDE Cancel';
  tmpB.OnClick:=CancelClick;
  AddChild(tmpB);

  i:=GRADIENTEDITORWIDTH div 2+(GRADIENTEDITORWIDTH div 2-(2*normalbuttonwidth+9)) div 2;
  fUndoButton:=TBDButton.Create(fLeft+i,fTop+Height-NORMALBUTTONHEIGHT-12,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,'UNDO','UNDO LAST CHANGE');
  fUndoButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  fUndoButton.Name:='GDE Undo';
  fUndoButton.OnClick:=UndoClick;
  AddChild(fUndoButton);

  fRedoButton:=TBDButton.Create(fLeft+i+NORMALBUTTONWIDTH+9,fTop+Height-NORMALBUTTONHEIGHT-12,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,'REDO','REDO LAST CHANGE');
  fRedoButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  fRedoButton.Name:='GDE Redo';
  fRedoButton.OnClick:=RedoClick;
  AddChild(fRedoButton);

  fUndoSystem:=TBDGradientEditorUndoSystem.Create(GradientEditorGradient);
end;

destructor TBDGradientEditor.Destroy;
begin
  fUndoSystem.Free;
  inherited Destroy;
end;

procedure TBDGradientEditor.SetColor(pTarget: integer; pColor: uint32);
begin
  if pColor<>POSTPROCESSCOLOR then begin
    case pTarget of
      PARM_COL_GRADEDIT_LEFT:begin
        fUndoSystem.AddColorChangeUndo(1,GradientEditorGradient.Colors[1],pColor);
        GradientEditorGradient.Colors[1]:=pColor;
      end;
      PARM_COL_GRADEDIT_RIGHT:begin
        fUndoSystem.AddColorChangeUndo(2,GradientEditorGradient.Colors[2],pColor);
        GradientEditorGradient.Colors[2]:=pColor;
      end;
      PARM_COL_GRADEDIT_COLOR3:begin
        fUndoSystem.AddColorChangeUndo(3,GradientEditorGradient.Colors[3],pColor);
        GradientEditorGradient.Colors[3]:=pColor;
      end;
      PARM_COL_GRADEDIT_COLOR4:begin
        fUndoSystem.AddColorChangeUndo(4,GradientEditorGradient.Colors[4],pColor);
        GradientEditorGradient.Colors[4]:=pColor;
      end;
      PARM_COL_GRADEDIT_COLOR5:begin
        fUndoSystem.AddColorChangeUndo(5,GradientEditorGradient.Colors[5],pColor);
        GradientEditorGradient.Colors[5]:=pColor;
      end;
    end;
    RefreshControls;
  end;
end;

function TBDGradientEditor.ProcessMessage(msg:TMessage):boolean;
begin
  Result:=false;
  if Enabled then begin
    case msg.TypeID of
      MSG_ACTIVATEGRADIENTEDITOR:begin
        fCalledFrom:=msg.DataInt;
        fCalledFromIndex:=msg.DataUInt32;
        RefreshControls(true);
      end;
    end;
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
    fUndoSystem.AddColorUsedChangeUndo(Tag,GradientEditorGradient.ColorUsed[Tag],Selected);
    GradientEditorGradient.ColorUsed[Tag]:=Selected;
    RefreshControls;
  end;
end;

procedure TBDGradientEditor.SliderChange(Sender:TObject; value:integer);
begin
  if sender is TBDSlider2 then with Sender as TBDSlider2 do begin
    fUndoSystem.AddColorPositionChangedUndo(Tag,GradientEditorGradient.ColorPositions[Tag],Position/511);
    GradientEditorGradient.ColorPositions[Tag]:=Position/511;
    RefreshControls;
  end;
end;

procedure TBDGradientEditor.OKClick(Sender:TObject; x,y,buttons:integer);
begin
  MessageQueue.AddMessage(MSG_GRADIENTEDITORRESPONSE,fCalledFrom,fCalledFromIndex);
  Self.Hide;
end;

procedure TBDGradientEditor.CancelClick(Sender:TObject; x,y,buttons:integer);
begin
  MessageQueue.AddMessage(MSG_GRADIENTEDITORRESPONSE,fCalledFrom,9999);
  Self.Hide;
end;

procedure TBDGradientEditor.UndoClick(Sender:TObject; x,y,buttons:integer);
begin
  if fUndoSystem.CanUndo then begin
    fUndoSystem.Undo;
    RefreshControls(true);
  end;
end;

procedure TBDGradientEditor.RedoClick(Sender:TObject; x,y,buttons:integer);
begin
  if fUndoSystem.CanRedo then begin
    fUndoSystem.Redo;
    RefreshControls(true);
  end;
end;

procedure TBDGradientEditor.RefreshControls(pKnobsToo:boolean);
begin
  fUndoButton.Enabled:=fUndoSystem.CanUndo;
  fRedoButton.Enabled:=fUndoSystem.CanRedo;
  fSimpleGradient.Refresh;
  fColorBoxes[1].Color:=GradientEditorGradient.Colors[1];
  fColorBoxes[2].Color:=GradientEditorGradient.Colors[2];
  fColorBoxes[3].Color:=GradientEditorGradient.Colors[3];
  fColorBoxes[4].Color:=GradientEditorGradient.Colors[4];
  fColorBoxes[5].Color:=GradientEditorGradient.Colors[5];
  fCheckBoxes[3].Selected:=GradientEditorGradient.ColorUsed[3];
  fCheckBoxes[4].Selected:=GradientEditorGradient.ColorUsed[4];
  fCheckBoxes[5].Selected:=GradientEditorGradient.ColorUsed[5];
  if pKnobsToo then begin
    fColorSliders[3].Position:=round(GradientEditorGradient.ColorPositions[3]*511);
    fColorSliders[4].Position:=round(GradientEditorGradient.ColorPositions[4]*511);
    fColorSliders[5].Position:=round(GradientEditorGradient.ColorPositions[5]*511);
  end;
end;

procedure TBDGradientEditor.Show(Sender:TObject);
begin
  fUndoSystem.Clear;
  RefreshControls(true);
end;

end.

