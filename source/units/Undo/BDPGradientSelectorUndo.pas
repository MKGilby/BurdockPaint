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

unit BDPGradientSelectorUndo;

{$mode Delphi}

interface

uses
  BDPUndoBase, BDPGradient;

type

  { TBDGradientSelectorUndoItem }

  TBDGradientSelectorUndoItem=class(TBDUndoItem)
    constructor Create(iAffectedGradient,iNewGradient:TGradient);
    destructor Destroy; override;
    procedure Undo; override;
    procedure Redo; override;
  private
    fAffectedGradient,
    fOldGradient,
    fNewGradient:TGradient;
  end;

  { TBDGradientSelectorUndoSystem }

  TBDGradientSelectorUndoSystem=class(TBDUndoSystem)
    procedure AddUndo(pAffectedGradient,pNewGradient:TGradient);
  end;


implementation

{ TBDGradientSelectorUndoItem }

constructor TBDGradientSelectorUndoItem.Create(iAffectedGradient,iNewGradient:TGradient);
begin
  fAffectedGradient:=iAffectedGradient;
  fOldGradient:=TGradient.Create(0,0);
  fOldGradient.CopyFrom(fAffectedGradient);
  fNewGradient:=TGradient.Create(0,0);
  fNewGradient.CopyFrom(iNewGradient);
  fRedoable:=true;
end;

destructor TBDGradientSelectorUndoItem.Destroy;
begin
  if Assigned(fNewGradient) then fNewGradient.Free;
  if Assigned(fOldGradient) then fOldGradient.Free;
  inherited Destroy;
end;

procedure TBDGradientSelectorUndoItem.Undo;
begin
  fAffectedGradient.CopyFrom(fOldGradient);
end;

procedure TBDGradientSelectorUndoItem.Redo;
begin
  fAffectedGradient.CopyFrom(fNewGradient);
end;

{ TBDGradientSelectorUndoSystem }

procedure TBDGradientSelectorUndoSystem.AddUndo(pAffectedGradient,pNewGradient:TGradient);
begin
  AddItem(TBDGradientSelectorUndoItem.Create(pAffectedGradient,pNewGradient));
end;

end.

