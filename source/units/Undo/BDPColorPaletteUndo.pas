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

unit BDPColorPaletteUndo;

{$mode Delphi}

interface

uses BDPUndoBase;

type

  { TBDColorBoxUndoItem }

  TBDColorBoxUndoItem=class(TBDUndoItem)
    constructor Create(iBefore,iAfter:uint32);
    procedure Undo; override;
    procedure Redo; override;
  private
    fBefore,fAfter:uint32;
  end;

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
    procedure AddColorBoxUndo(pBefore,pAfter:uint32);
    procedure AddStoreToPaletteUndo(pIndex,pBefore,pAfter:uint32);
  end;

implementation

uses BDPShared, BDPMessage;

{ TBDColorBoxUndoItem }

constructor TBDColorBoxUndoItem.Create(iBefore,iAfter:uint32);
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
end;


{ TBDStoreToPaletteUndoItem }

constructor TBDStoreToPaletteUndoItem.Create(iIndex,iBefore,iAfter:uint32);
begin
  fIndex:=iIndex;
  fBefore:=iBefore;
  fAfter:=iAfter;
end;

procedure TBDStoreToPaletteUndoItem.Undo;
begin
  MessageQueue.AddMessage(MSG_SETPALETTECOLOR,fIndex,fBefore);
end;

procedure TBDStoreToPaletteUndoItem.Redo;
begin
  MessageQueue.AddMessage(MSG_SETPALETTECOLOR,fIndex,fAfter);
end;


{ TBDColorPaletteUndoSystem }

constructor TBDColorPaletteUndoSystem.Create;
begin
  inherited Create;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_SETPALETTEUNDOREDOBUTTON,0,0);
end;

procedure TBDColorPaletteUndoSystem.AddColorBoxUndo(pBefore,pAfter:uint32);
begin
  if (fPointer<>Self.Count-1) then   // If not the last item, delete items after it.
    Self.DeleteRange(fPointer+1,Self.Count-1);
  if Self.Count=Settings.UndoLimit then Self.Delete(0);
  Self.Add(TBDColorBoxUndoItem.Create(pBefore,pAfter));
end;

procedure TBDColorPaletteUndoSystem.AddStoreToPaletteUndo(pIndex,pBefore,pAfter:uint32);
begin
  if (fPointer<>Self.Count-1) then   // If not the last item, delete items after it.
    Self.DeleteRange(fPointer+1,Self.Count-1);
  if Self.Count=Settings.UndoLimit then Self.Delete(0);
  Self.Add(TBDStoreToPaletteUndoItem.Create(pIndex,pBefore,pAfter));
end;

end.

