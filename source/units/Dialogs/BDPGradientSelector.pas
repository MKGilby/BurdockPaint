{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPGradientSelector;

{$mode Delphi}

interface

uses
  SysUtils, BDPModalDialog, BDPButton, BDPSimpleGradient, BDPSliders,
  BDPGradientSelectorUndo;

type

  { TBDGradientSelector }

  TBDGradientSelector=class(TBDModalDialog)
    constructor Create;
    // The gradient editor changed the GradientEditorGradient,
    // so the gradient pointed by index should be updated (data and screen).
    procedure SetGradient(pIndex:uint32);
  private
    fUndoButton,
    fRedoButton,
    fDeleteButton:TBDButton;
    fSelectedGradientIndex:integer;  // 0..Project.CurrentGradientList.Count-1
    fGradients:array[0..7] of TBDSimpleGradient;
    fScrollBar:TBDVerticalSlider;
    procedure GradientClick(Sender:TObject;x,y,button:integer);
    procedure SelectClick(Sender:TObject;x,y,button:integer);
    procedure CancelClick(Sender:TObject;x,y,button:integer);
    procedure EditClick(Sender:TObject;x,y,button:integer);
    procedure UndoClick(Sender:TObject;x,y,button:integer);
    procedure RedoClick(Sender:TObject;x,y,button:integer);
    procedure AddClick(Sender:TObject;x,y,button:integer);
    procedure DeleteClick(Sender:TObject;x,y,button:integer);
    procedure SaveClick(Sender:TObject;x,y,button:integer);
    procedure LoadClick(Sender:TObject;x,y,button:integer);
    procedure ScrollBarChange(Sender:TObject;oldValue,newValue:integer);
    procedure RefreshUndoRedoButtons;
    procedure RefreshGradients;
    procedure GDSShow(Sender:TObject);
  end;

implementation

uses BDPShared, MKMouse2, BDPGradient, Math;

const
  GRADIENTSELECTORWIDTH=640;
//  GRADIENTSELECTORHEIGHT=6*(NORMALBUTTONHEIGHT+3)-3+2*9+3+MODALDIALOGCAPTIONHEIGHT+15;
  BUTTONSLEFT=GRADIENTSELECTORWIDTH-NORMALBUTTONWIDTH-9-3;
  BUTTONSTOP=MODALDIALOGCAPTIONHEIGHT+9;
  SCROLLBARWIDTH=33;
  GRADIENTSCROLLBARGAP=3;
  GRADIENTWIDTH=GRADIENTSELECTORWIDTH-3-9-GRADIENTSCROLLBARGAP-SCROLLBARWIDTH-9-NORMALBUTTONWIDTH-9-3;
  GRADIENTHEIGHT=36;
  GRADIENTSTOP=BUTTONSTOP;
  GRADIENTSELECTORHEIGHT=8*(GRADIENTHEIGHT+3)-3+2*9+3+MODALDIALOGCAPTIONHEIGHT;
  SCROLLBARLEFT=3+9+GRADIENTWIDTH+GRADIENTSCROLLBARGAP;
  SCROLLBARTOP=GRADIENTSTOP;
  SCROLLBARHEIGHT=8*(GRADIENTHEIGHT+3)-3;

{ TBDGradientSelector }

constructor TBDGradientSelector.Create;
var i:integer;

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

  procedure CreateGradient(pIndex,pLeft,pTop:integer;pGradient:TGradient=nil);
  begin
    fGradients[pIndex]:=TBDSimpleGradient.Create(pLeft,pTop,GRADIENTWIDTH,GRADIENTHEIGHT,pGradient);
    with fGradients[pIndex] do begin
      ZIndex:=MODALDIALOG_ZINDEX+1;
      Name:='GS Gradient '+inttostr(pIndex);
      Tag:=pIndex;
      OnClick:=GradientClick;
    end;
    AddChild(fGradients[pIndex]);
  end;

begin
  inherited Create(GRADIENTSELECTORWIDTH,GRADIENTSELECTORHEIGHT);
  Caption:='GRADIENT SELECTOR';
  fName:='GradientSelector';
  Self.OnShow:=GDSShow;
  MouseObjects.Add(Self);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP,
    'SELECT','SELECT THE GRADIENT.','GDS Select',SelectClick);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+NORMALBUTTONHEIGHT+3,
    'CANCEL','CLOSE DIALOG.','GDS Close',CancelClick);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+(NORMALBUTTONHEIGHT+3)*2+6,
    'EDIT','EDIT SELECTED GRADIENT.','GDS Edit',EditClick);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+(NORMALBUTTONHEIGHT+3)*3+12,
    'ADD','ADD NEW GRADIENT.','GDS Add',AddClick);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+(NORMALBUTTONHEIGHT+3)*4+12,
    'DELETE','DELETE SELECTED GRADIENT.','GDS Delete',DeleteClick);
  fDeleteButton:=TBDButton(fChildren.Items[fChildren.Count-1]);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+(NORMALBUTTONHEIGHT+3)*5+18,
    'UNDO','UNDO LAST GRADIENT OPERATION.','GDS Undo',UndoClick);
  fUndoButton:=TBDButton(fChildren.Items[fChildren.Count-1]);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+(NORMALBUTTONHEIGHT+3)*6+18,
    'REDO','REDO LAST GRADIENT OPERATION.','GDS Redo',RedoClick);
  fRedoButton:=TBDButton(fChildren.Items[fChildren.Count-1]);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+(NORMALBUTTONHEIGHT+3)*7+24,
    'SAVE','SAVE ALL GRADIENTS TO FILE.','GDS Save',SaveClick);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+(NORMALBUTTONHEIGHT+3)*8+24,
    'LOAD','LOAD GRADIENTS FROM FILE.','GDS Load',LoadClick);

  for i:=0 to min(Project.CurrentGradientList.Count-1,7) do
    CreateGradient(i,fLeft+3+9,fTop+GRADIENTSTOP+(GRADIENTHEIGHT+3)*i,Project.CurrentGradientList[i]);

  for i:=Project.CurrentGradientList.Count to 7 do
    CreateGradient(i,fLeft+3+9,fTop+GRADIENTSTOP+(GRADIENTHEIGHT+3)*i);

  fScrollBar:=TBDVerticalSlider.Create(fLeft+SCROLLBARLEFT,fTop+SCROLLBARTOP,SCROLLBARWIDTH,SCROLLBARHEIGHT);
  fScrollBar.ZIndex:=MODALDIALOG_ZINDEX+1;
  fScrollBar.Name:='GS ScrollBar';
  fScrollBar.MinValue:=0;
  fScrollBar.MaxValue:=max(Project.CurrentGradientList.Count-8,0);
  fScrollBar.Position:=0;
  fScrollBar.ShowNumber:=false;
  fScrollBar.OnChange:=ScrollBarChange;
  AddChild(fScrollBar);

  fSelectedGradientIndex:=Project.CurrentGradientList.ActiveIndex;

  RefreshUndoRedoButtons;
end;

procedure TBDGradientSelector.SetGradient(pIndex:uint32);
begin
  Project.CurrentImage.GradientSelectorUndo.AddUndo_EDIT(
    pIndex,Project.CurrentGradientList[pIndex],GradientEditorGradient);
  RefreshUndoRedoButtons;
  Project.CurrentGradientList[pIndex].CopyFrom(
    GradientEditorGradient);
  fGradients[pIndex-fScrollBar.Position].Refresh;
end;

procedure TBDGradientSelector.GradientClick(Sender:TObject; x,y,button:integer);
begin
  if Sender is TBDSimpleGradient then with Sender as TBDSimpleGradient do begin
    if fScrollBar.Position+Tag<Project.CurrentGradientList.Count then begin
      fSelectedGradientIndex:=fScrollBar.Position+Tag;
      RefreshGradients;
    end;
  end;
end;

procedure TBDGradientSelector.SelectClick(Sender:TObject; x,y,button:integer);
begin
  Project.CurrentGradientList.ActiveIndex:=fSelectedGradientIndex;
  Self.Hide;
end;

procedure TBDGradientSelector.CancelClick(Sender:TObject; x,y,button:integer);
begin
  Self.Hide;
end;

procedure TBDGradientSelector.EditClick(Sender:TObject; x,y,button:integer);
begin
  if (fSelectedGradientIndex>=0) and
     (fSelectedGradientIndex<Project.CurrentGradientList.Count) then begin
    GradientEditorGradient.CopyFrom(
      Project.CurrentGradientList[fSelectedGradientIndex]);
    MessageQueue.AddMessage(
      MSG_OPENGRADIENTEDITOR,
      PARM_GRAD_SELECTOR,
      fSelectedGradientIndex);
    Self.Hide;
  end;
end;

procedure TBDGradientSelector.UndoClick(Sender:TObject; x,y,button:integer);
var i:integer;
begin
  Project.CurrentImage.GradientSelectorUndo.Undo;
  RefreshUndoRedoButtons;
  RefreshGradients;
  for i:=0 to 7 do fGradients[i].Refresh;
  MessageQueue.AddMessage(MSG_ACTIVEGRADIENTCHANGED);
end;

procedure TBDGradientSelector.RedoClick(Sender:TObject; x,y,button:integer);
var i:integer;
begin
  Project.CurrentImage.GradientSelectorUndo.Redo;
  RefreshUndoRedoButtons;
  RefreshGradients;
  for i:=0 to 7 do fGradients[i].Refresh;
  MessageQueue.AddMessage(MSG_ACTIVEGRADIENTCHANGED);
end;

procedure TBDGradientSelector.AddClick(Sender:TObject; x,y,button:integer);
begin
  if (Sender is TBDButton) and (Sender as TBDButton).Enabled then begin
    Project.CurrentImage.GradientSelectorUndo.AddUndo_ADD(Project.CurrentGradientList.Count);
    RefreshUndoRedoButtons;
    Project.CurrentGradientList.Add(TGradient.Create($ff000000,$ffffffff));
    RefreshGradients;
    fScrollBar.MaxValue:=max(Project.CurrentGradientList.Count-8,0);
  end;
end;

procedure TBDGradientSelector.DeleteClick(Sender:TObject; x,y,button:integer);
begin
  if (Sender is TBDButton) and (Sender as TBDButton).Enabled then begin
    Project.CurrentImage.GradientSelectorUndo.AddUndo_DELETE(fSelectedGradientIndex);
    Project.CurrentGradientList.Delete(fSelectedGradientIndex);
    RefreshUndoRedoButtons;
    if fSelectedGradientIndex=Project.CurrentGradientList.Count then begin
      fSelectedGradientIndex:=Project.CurrentGradientList.Count-1;
    end;
    RefreshGradients;
    if Project.CurrentGradientList.ActiveIndex>=Project.CurrentGradientList.Count then
      Project.CurrentGradientList.ActiveIndex:=Project.CurrentGradientList.Count-1;
    fScrollBar.MaxValue:=max(Project.CurrentGradientList.Count-8,0);
  end;
end;

procedure TBDGradientSelector.SaveClick(Sender: TObject; x, y, button: integer);
begin

end;

procedure TBDGradientSelector.LoadClick(Sender: TObject; x, y, button: integer);
begin

end;

procedure TBDGradientSelector.ScrollBarChange(Sender:TObject;oldValue,newValue:integer);
begin
  RefreshGradients;
end;

procedure TBDGradientSelector.RefreshUndoRedoButtons;
begin
  fUndoButton.Enabled:=Project.CurrentImage.GradientSelectorUndo.CanUndo;
  fRedoButton.Enabled:=Project.CurrentImage.GradientSelectorUndo.CanRedo;
  fDeleteButton.Enabled:=Project.CurrentGradientList.Count>1;
end;

procedure TBDGradientSelector.RefreshGradients;
var i,j:integer;
begin
  j:=min(Project.CurrentGradientList.Count-fScrollBar.Position-1,7);
  for i:=0 to j do begin
    fGradients[i].Gradient:=Project.CurrentGradientList[fScrollBar.Position+i];
    fGradients[i].Selected:=(fScrollBar.Position+i=fSelectedGradientIndex);
  end;

  for i:=j+1 to 7 do
    fGradients[i].Gradient:=nil;
end;

procedure TBDGradientSelector.GDSShow(Sender:TObject);
begin
  RefreshGradients;
  RefreshUndoRedoButtons;
end;

end.

