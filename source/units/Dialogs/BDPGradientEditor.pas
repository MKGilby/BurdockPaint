unit BDPGradientEditor;

{$mode Delphi}

interface

uses BDPModalDialog, BDPSimpleGradient, BDPCheckBox, BDPButton,
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
    fColorBoxes:array[0..3] of TBDColorBox;
    fCheckBoxMoreColor1,
    fCheckBoxMoreColor2:TBDCheckBox;
    fCloseButton:TBDButton;
    procedure ColorBoxClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses BDPShared, MKMouse2, BDPMessage;

const
  GRADIENTEDITORWIDTH=640;
  GRADIENTEDITORHEIGHT=480;

{ TBDGradientEditor }

constructor TBDGradientEditor.Create;
begin
  inherited Create(GRADIENTEDITORWIDTH,GRADIENTEDITORHEIGHT);
  Caption:='GRADIENT EDITOR';
  fName:='GradientEditor';
  MouseObjects.Add(Self);
  fGradient:=Project.CurrentGradientList.ActiveGradient;
  fSimpleGradient:=TBDSimpleGradient.Create(fLeft+64,fTop+30,512,36,fGradient);
  fSimpleGradient.ZIndex:=MODALDIALOG_ZINDEX+1;
  fSimpleGradient.Name:='CCE Gradient';
  AddChild(fSimpleGradient);
  fColorBoxes[0]:=TBDColorBox.Create(fLeft+64-3-36,fTop+30,36,36);
  fColorBoxes[0].Color:=fGradient.Color1;
  fColorBoxes[0].ZIndex:=MODALDIALOG_ZINDEX+1;
  fColorBoxes[0].Name:='CCE ColorBox 0';
  fColorBoxes[0].Tag:=PARM_COL_CCEDIT_LEFT;
  fColorBoxes[0].OnClick:=ColorBoxClick;
  AddChild(fColorBoxes[0]);
  fColorBoxes[1]:=TBDColorBox.Create(fLeft+64+512+3,fTop+30,36,36);
  fColorBoxes[1].Color:=fGradient.Color2;
  fColorBoxes[1].ZIndex:=MODALDIALOG_ZINDEX+1;
  fColorBoxes[1].Name:='CCE ColorBox 1';
  fColorBoxes[1].Tag:=PARM_COL_CCEDIT_RIGHT;
  fColorBoxes[1].OnClick:=ColorBoxClick;
  AddChild(fColorBoxes[1]);
  fCheckBoxMoreColor1:=TBDCheckBox.Create(fLeft+64-3-32,fTop+30+36+9+4,27,27,'');
  fCheckBoxMoreColor1.ZIndex:=MODALDIALOG_ZINDEX+1;
  fCheckBoxMoreColor1.Name:='CCE CheckBox MC1';
  AddChild(fCheckBoxMoreColor1);
  fColorBoxes[2]:=TBDColorBox.Create(fLeft+64+512+3,fTop+30+36+9,36,36);
  fColorBoxes[2].Color:=$ffff0000;
  fColorBoxes[2].ZIndex:=MODALDIALOG_ZINDEX+1;
  fColorBoxes[2].Name:='CCE ColorBox 2';
  fColorBoxes[2].Tag:=PARM_COL_CCEDIT_ADD1;
  AddChild(fColorBoxes[2]);
  fCheckBoxMoreColor2:=TBDCheckBox.Create(fLeft+64-3-32,fTop+30+36*2+9*2,27,27,'');
  fCheckBoxMoreColor2.ZIndex:=MODALDIALOG_ZINDEX+1;
  fCheckBoxMoreColor2.Name:='CCE CheckBox MC2';
  AddChild(fCheckBoxMoreColor2);
  fColorBoxes[3]:=TBDColorBox.Create(fLeft+64+512+3,fTop+30+36*2+9*2,36,36);
  fColorBoxes[3].Color:=$ffff00ff;
  fColorBoxes[3].ZIndex:=MODALDIALOG_ZINDEX+1;
  fColorBoxes[3].Name:='CCE ColorBox 3';
  fColorBoxes[3].Tag:=PARM_COL_CCEDIT_ADD2;
  AddChild(fColorBoxes[3]);
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
  if pColor<>POSTPROCESSCOLOR then
    case pTarget of
      PARM_COL_CCEDIT_LEFT:fColorBoxes[0].Color:=pColor;
      PARM_COL_CCEDIT_RIGHT:fColorBoxes[1].Color:=pColor;
      PARM_COL_CCEDIT_ADD1:fColorBoxes[2].Color:=pColor;
      PARM_COL_CCEDIT_ADD2:fColorBoxes[3].Color:=pColor;
    end;
end;

procedure TBDGradientEditor.ColorBoxClick(Sender: TObject; x, y, buttons: integer);
begin
  if Sender is TBDColorBox then begin
    MessageQueue.AddMessage(MSG_OPENCOLOREDITOR,TBDColorBox(Sender).Tag,TBDColorBox(Sender).Color);
    Self.Hide;
  end;
end;

end.

