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

unit BDPUndoBase;

{$mode Delphi}

interface

uses Classes, Sysutils, fgl, BDPMessage;

type

  { TBDUndoItem }

  TBDUndoItem=class
    constructor Create;
    procedure Undo; virtual; abstract;
    procedure Redo; virtual; abstract;
    procedure SaveToFile(Filename:string);
    procedure SaveToStream(Target:TStream); virtual;
    procedure LoadFromFile(Filename:string);
    procedure LoadFromStream(Target:TStream); virtual;
  protected
    fRedoable:boolean;
  public
    property Redoable:boolean read fRedoable;
  end;

  TBDUndoList=TFPGObjectList<TBDUndoItem>;

  { TBDUndoSystem }

  TBDUndoSystem=class(TBDUndoList)
    constructor Create;
    constructor CreateFromStream(pStream:TStream);
    procedure Clear;
    procedure Undo;
    procedure Redo;
    procedure SaveToFile(pFilename:string);
    procedure SaveToStream(pStream:TStream); virtual;
    procedure LoadFromFile(pFilename:string);
    procedure LoadFromStream(pStream:TStream); virtual;
    procedure AddItem(pItem:TBDUndoItem);
    function CanUndo:boolean;
    function CanRedo:boolean;
  protected
    fPointer:integer;  // points to the item that will be undoed if requested.
    fAfterUndoRedoMessage:TMessage;
  end;

implementation

uses BDPShared;

{ TBDUndoItem }

constructor TBDUndoItem.Create;
begin
  fRedoable:=false;
end;

procedure TBDUndoItem.SaveToFile(Filename: string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(Filename,fmCreate);
  SaveToStream(Xs);
  Xs.Free;
end;

procedure TBDUndoItem.SaveToStream(Target:TStream);
begin
  // Override only when you want to save undoitem state
end;

procedure TBDUndoItem.LoadFromFile(Filename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(Filename,fmOpenRead or fmShareDenyWrite);
  LoadFromStream(Xs);
  Xs.Free;
end;

procedure TBDUndoItem.LoadFromStream(Target:TStream);
begin
  // Override only when you want to load undoitem state
end;


{ TBDUndoSystem }

constructor TBDUndoSystem.Create;
begin
  inherited Create;
  fPointer:=-1;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_NONE,0,0);
end;

constructor TBDUndoSystem.CreateFromStream(pStream:TStream);
begin
  inherited Create;
  LoadFromStream(pStream);
end;

procedure TBDUndoSystem.Clear;
begin
  inherited Clear;
  fPointer:=-1;
end;

procedure TBDUndoSystem.Undo;
begin
  if (fPointer>=0) and (fPointer<Count) then begin
    Self[fPointer].Undo;
    dec(fPointer);  // It can go below 0, -1 shows that no more undoable task remains.
    MessageQueue.AddMessage(fAfterUndoRedoMessage);
  end;
end;

procedure TBDUndoSystem.Redo;
begin
  if (fPointer+1>=0) and (fPointer+1<Count) then begin
    if Self[fPointer+1].Redoable then begin
      inc(fPointer);
      Self[fPointer].Redo;
      MessageQueue.AddMessage(fAfterUndoRedoMessage);
    end;
  end;
end;

procedure TBDUndoSystem.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  SaveToStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDUndoSystem.SaveToStream(pStream:TStream);
begin
  // Override only when you want to save undosystem state
end;

procedure TBDUndoSystem.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead or fmShareDenyNone);
  LoadFromStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDUndoSystem.LoadFromStream(pStream:TStream);
begin
  // Override only when you want to load undosystem state
end;

procedure TBDUndoSystem.AddItem(pItem:TBDUndoItem);
begin
  if (fPointer<>Self.Count-1) then   // If not the last item, delete items after it.
    Self.DeleteRange(fPointer+1,Self.Count-1);
  if Self.Count=Settings.UndoLimit then Self.Delete(0);
  Self.Add(pItem);
  fPointer:=Self.Count-1;
end;

function TBDUndoSystem.CanUndo:boolean;
begin
  Result:=fPointer>-1;
end;

function TBDUndoSystem.CanRedo:boolean;
begin
  Result:=fPointer<Self.Count-1;
end;

end.

