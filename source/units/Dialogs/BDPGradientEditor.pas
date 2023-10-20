unit BDPGradientEditor;

{$mode Delphi}

interface

uses SysUtils, BDPModalDialog, BDPSimpleGradient, BDPCheckBox, BDPButton,
  BDPColorBox, BDPGradient;

type

  { TBDGradientEditor }

  TBDGradientEditor=class(TBDModalDialog)
    constructor Create;
    destructor Destroy; override;
    procedure SetColor(pTarget:integer;pColor:uint32);
  private
    fSimpleGradient:TBDSimpleGradient;
    fGradient:TGradient;
    fColorBoxes:array[1..5] of TBDColorBox;
    fCheckBoxes:array[3..5] of TBDCheckBox;
    fCloseButton:TBDButton;
    procedure ColorBoxClick(Sender:TObject;x,y,buttons:integer);
    procedure CheckBoxChange(Sender:TObject);
  end;

implementation

uses BDPShared, MKMouse2, BDPMessage;

const
  GRADIENTEDITORWIDTH=640;
  GRADIENTEDITORHEIGHT=480;

{ TBDGradientEditor }

constructor TBDGradientEditor.Create;
const Helper:array[1..5] of integer=(PARM_COL_GRADEDIT_LEFT,PARM_COL_GRADEDIT_RIGHT,
        PARM_COL_GRADEDIT_COLOR3,PARM_COL_GRADEDIT_COLOR4,PARM_COL_GRADEDIT_COLOR5);
var i:integer;

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

begin
  inherited Create(GRADIENTEDITORWIDTH,GRADIENTEDITORHEIGHT);
  Caption:='GRADIENT EDITOR';
  fName:='GradientEditor';
  MouseObjects.Add(Self);
  fGradient:=Project.CurrentGradientList.ActiveGradient;
  fGradient.ColorPositions[3]:=0.5;
  fGradient.Colors[3]:=$FF00a0b0;
  fSimpleGradient:=TBDSimpleGradient.Create(fLeft+64,fTop+30,512,36,fGradient);
  fSimpleGradient.ZIndex:=MODALDIALOG_ZINDEX+1;
  fSimpleGradient.Name:='GDE Gradient';
  AddChild(fSimpleGradient);
  fGradient.LogContents;

  CreateColorBox(1,fLeft+64-3-36,fTop+30,fGradient.Colors[1],'GDE ColorBox 1',PARM_COL_GRADEDIT_LEFT);
  CreateColorBox(2,fLeft+64+512+3,fTop+30,fGradient.Colors[2],'GDE ColorBox 2',PARM_COL_GRADEDIT_RIGHT);

  for i:=3 to 5 do begin
    CreateCheckBox(i,fLeft+64-3-32,fTop+30+(36+9)*(i-2)+4,'GDE CheckBox '+inttostr(i),i,fGradient.ColorUsed[i]);
    CreateColorBox(i,fLeft+64+512+3,fTop+30+(36+9)*(i-2),fGradient.Colors[i],'GDE ColorBox '+inttostr(i),Helper[i]);
  end;

{  CreateCheckBox(3,fLeft+64-3-32,fTop+30+36+9+4,'GDE CheckBox 3',3,fGradient.ColorUsed[3]);
  CreateColorBox(3,fLeft+64+512+3,fTop+30+36+9,fGradient.Colors[3],'GDE ColorBox 3',PARM_COL_GRADEDIT_COLOR3);

  CreateCheckBox(4,fLeft+64-3-32,fTop+30+(36+9)*2,'GDE CheckBox 4',4,fGradient.ColorUsed[4]);
  CreateColorBox(4,fLeft+64+512+3,fTop+30+(36+9)*2,fGradient.Colors[4],'GDE ColorBox 4',PARM_COL_GRADEDIT_COLOR4);

  CreateCheckBox(5,fLeft+64-3-32,fTop+30+(36+9)*3,'GDE CheckBox 5',5,fGradient.ColorUsed[5]);
  CreateColorBox(5,fLeft+64+512+3,fTop+30+(36+9)*3,fGradient.Colors[5],'GDE ColorBox 5',PARM_COL_GRADEDIT_COLOR5);}

  fCloseButton:=TBDButton.Create(fLeft+6,fTop+Height-NORMALBUTTONHEIGHT-6,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,'CLOSE','CLOSE DIALOG');
  fCloseButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  fCloseButton.Name:='CCE CloseButton';
  fCloseButton.Message:=TMessage.Init(MSG_GRADIENTEDITORRESPONSE,0,0);
  AddChild(fCloseButton);
end;

destructor TBDGradientEditor.Destroy;
begin
  inherited Destroy;
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

end.

