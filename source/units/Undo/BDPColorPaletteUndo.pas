{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPColorPaletteUndo;

{$mode Delphi}

interface

uses BDPUndoBase;

type

  { TBDColorBoxUndoItem }

{  TBDColorBoxUndoItem=class(TBDUndoItem)
    constructor Create(iBefore,iAfter:uint32);
    procedure Undo; override;
    procedure Redo; override;
  private
    fBefore,fAfter:uint32;
  end;}

  { TBDStoreToPaletteUndoItem }

  TBDStoreToPaletteUndoItem=class(TBDUndoItem)
    constructor Create(iIndex,iBefore,iAfter:uint32);
    procedure Undo; override;
    procedure Redo; override;
  private
    fIndex,fBefore,fAfter:uint32;
  end;

  { TBDColorPaletteUndoSystem }

  TBDColorPaletteUndoSystem=class(TBDUndoSystem)
    constructor Create;
//    procedure AddColorBoxUndo(pBefore,pAfter:uint32);
    procedure AddStoreToPaletteUndo(pIndex,pBefore,pAfter:uint32);
  end;

implementation

uses BDPShared;

{ TBDColorBoxUndoItem }

{constructor TBDColorBoxUndoItem.Create(iBefore,iAfter:uint32);
begin
  fBefore:=iBefore;
  fAfter:=iAfter;
end;

procedure TBDColorBoxUndoItem.Undo;
begin
  MessageQueue.AddMessage(MSG_SETCOLORBOXCOLOR,0,fBefore);
end;

procedure TBDColorBoxUndoItem.Redo;
begin
  MessageQueue.AddMessage(MSG_SETCOLORBOXCOLOR,0,fAfter);
end;}


{ TBDStoreToPaletteUndoItem }

constructor TBDStoreToPaletteUndoItem.Create(iIndex,iBefore,iAfter:uint32);
begin
  fIndex:=iIndex;
  fBefore:=iBefore;
  fAfter:=iAfter;
end;

procedure TBDStoreToPaletteUndoItem.Undo;
begin
  Project.CurrentPalette.Colors[fIndex]:=fBefore;
end;

procedure TBDStoreToPaletteUndoItem.Redo;
begin
  Project.CurrentPalette.Colors[fIndex]:=fAfter;
end;


{ TBDColorPaletteUndoSystem }

constructor TBDColorPaletteUndoSystem.Create;
begin
  inherited Create;
end;

{procedure TBDColorPaletteUndoSystem.AddColorBoxUndo(pBefore,pAfter:uint32);
begin
  if (fPointer<>Self.Count-1) then   // If not the last item, delete items after it.
    Self.DeleteRange(fPointer+1,Self.Count-1);
  if Self.Count=Settings.UndoLimit then Self.Delete(0);
  Self.Add(TBDColorBoxUndoItem.Create(pBefore,pAfter));
end;}

procedure TBDColorPaletteUndoSystem.AddStoreToPaletteUndo(pIndex,pBefore,pAfter:uint32);
begin
  if (fPointer<>Self.Count-1) then   // If not the last item, delete items after it.
    Self.DeleteRange(fPointer+1,Self.Count-1);
  if Self.Count=Settings.UndoLimit then Self.Delete(0);
  Self.Add(TBDStoreToPaletteUndoItem.Create(pIndex,pBefore,pAfter));
end;

end.

