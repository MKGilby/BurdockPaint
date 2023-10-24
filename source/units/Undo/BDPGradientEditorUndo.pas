unit BDPGradientEditorUndo;

{$mode Delphi}

interface

uses
  Classes, SysUtils, BDPUndoBase, BDPGradient;

type

  { TBDGradientEditorCCUndoItem }

  TBDGradientEditorCCUndoItem=class(TBDUndoItem)
    constructor Create(iGradient:TGradient;iSlot:integer;iOldColor,iNewColor:uint32);
    procedure Undo; override;
    procedure Redo; override;
  private
    fGradient:TGradient;
    fSlot:integer;
    fOldColor,fNewColor:uint32;
  end;

  { TBDGradientEditorCUUndoItem }

  TBDGradientEditorCUUndoItem=class(TBDUndoItem)
    constructor Create(iGradient:TGradient;iSlot:integer;iOldState,iNewState:boolean);
    procedure Undo; override;
    procedure Redo; override;
  private
    fGradient:TGradient;
    fSlot:integer;
    fOldState,fNewState:boolean;
  end;

  { TBDGradientEditorCPUndoItem }

  TBDGradientEditorCPUndoItem=class(TBDUndoItem)
    constructor Create(iGradient:TGradient;iSlot:integer;iOldPosition,iNewPosition:double);
    procedure Undo; override;
    procedure Redo; override;
  private
    fGradient:TGradient;
    fSlot:integer;
    fOldPosition,fNewPosition:double;
  public
    property Slot:integer read fSlot;
    property NewPosition:double write fNewPosition;
  end;

  { TBDGradientEditorUndoSystem }

  TBDGradientEditorUndoSystem=class(TBDUndoSystem)
    constructor Create(iGradient:TGradient);
    procedure AddColorChangeUndo(pSlot:integer;pOldColor,pNewColor:uint32);
    procedure AddColorUsedChangeUndo(pSlot:integer;pOldState,pNewState:boolean);
    procedure AddColorPositionChangedUndo(pSlot:integer;pOldPosition,pNewPosition:double);
  private
    fGradient:TGradient;
  end;


implementation

uses BDPShared;

{ TBDGradientEditorCCUndoItem }

constructor TBDGradientEditorCCUndoItem.Create(iGradient:TGradient;
  iSlot:integer; iOldColor,iNewColor:uint32);
begin
  fGradient:=iGradient;
  fSlot:=iSlot;
  fOldColor:=iOldColor;
  fNewColor:=iNewColor;
  fRedoable:=true;
end;

procedure TBDGradientEditorCCUndoItem.Undo;
begin
  fGradient.Colors[fSlot]:=fOldColor;
end;

procedure TBDGradientEditorCCUndoItem.Redo;
begin
  fGradient.Colors[fSlot]:=fNewColor;
end;

{ TBDGradientEditorCUUndoItem }

constructor TBDGradientEditorCUUndoItem.Create(iGradient:TGradient;
  iSlot:integer; iOldState,iNewState:boolean);
begin
  fGradient:=iGradient;
  fSlot:=iSlot;
  fOldState:=iOldState;
  fNewState:=iNewState;
  fRedoable:=true;
end;

procedure TBDGradientEditorCUUndoItem.Undo;
begin
  fGradient.ColorUsed[fSlot]:=fOldState;
end;

procedure TBDGradientEditorCUUndoItem.Redo;
begin
  fGradient.ColorUsed[fSlot]:=fNewState;
end;

{ TBDGradientEditorCPUndoItem }

constructor TBDGradientEditorCPUndoItem.Create(iGradient:TGradient;
  iSlot:integer;iOldPosition,iNewPosition:double);
begin
  fGradient:=iGradient;
  fSlot:=iSlot;
  fOldPosition:=iOldPosition;
  fNewPosition:=iNewPosition;
  fRedoable:=true;
end;

procedure TBDGradientEditorCPUndoItem.Undo;
begin
  fGradient.ColorPositions[fSlot]:=fOldPosition;
end;

procedure TBDGradientEditorCPUndoItem.Redo;
begin
  fGradient.ColorPositions[fSlot]:=fNewPosition;
end;

{ TBDGradientEditorUndoSystem }

constructor TBDGradientEditorUndoSystem.Create(iGradient:TGradient);
begin
  inherited Create;
  fGradient:=iGradient;
end;

procedure TBDGradientEditorUndoSystem.AddColorChangeUndo(pSlot:integer;
  pOldColor,pNewColor:uint32);
begin
  AddItem(TBDGradientEditorCCUndoItem.Create(fGradient,pSlot,pOldColor,pNewColor));
end;

procedure TBDGradientEditorUndoSystem.AddColorUsedChangeUndo(pSlot:integer;
  pOldState,pNewState:boolean);
begin
  AddItem(TBDGradientEditorCUUndoItem.Create(fGradient,pSlot,pOldState,pNewState));
end;

procedure TBDGradientEditorUndoSystem.AddColorPositionChangedUndo(
  pSlot:integer; pOldPosition,pNewPosition:double);
var w:boolean;
begin
  w:=false;
  if (fPointer>-1) then
    if Self[fPointer] is TBDGradientEditorCPUndoItem then
      if TBDGradientEditorCPUndoItem(Self[fPointer]).Slot=pSlot then begin
        TBDGradientEditorCPUndoItem(Self[fPointer]).NewPosition:=pNewPosition;
        w:=true;
      end;
  if not w then
    AddItem(TBDGradientEditorCPUndoItem.Create(fGradient,pSlot,pOldPosition,pNewPosition));
end;

end.

