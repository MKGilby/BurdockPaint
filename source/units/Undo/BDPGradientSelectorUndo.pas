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
  SysUtils, Classes, BDPUndoBase, BDPGradient;

type

  { TBDGradientSelectorUndoItem }

  TBDGradientSelectorUndoItem=class(TBDUndoItem)
    constructor Create(iGradientIndex:integer;iAffectedGradient,iNewGradient:TGradient);
    constructor CreateFromStream(iStream:TStream);
    destructor Destroy; override;
    procedure Undo; override;
    procedure Redo; override;
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    fIndex:integer;
    fOldGradient,
    fNewGradient:TGradient;
    procedure LoadFromStreamV1(pStream:TStream);
  end;

  { TBDGradientSelectorUndoSystem }

  TBDGradientSelectorUndoSystem=class(TBDUndoSystem)
    procedure AddUndo(pGradientIndex:integer;pAffectedGradient,pNewGradient:TGradient);
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    procedure LoadFromStreamV1(pStream:TStream);
  end;


implementation

uses BDPShared;

const
  UNDOOPERATIONGRADIENTSELECTORID='UDG';
  UNDOSYSTEMGRADIENTSELECTORID='USG';

{ TBDGradientSelectorUndoItem }

constructor TBDGradientSelectorUndoItem.Create(iGradientIndex:integer;
  iAffectedGradient,iNewGradient:TGradient);
begin
  fIndex:=iGradientIndex;
  fOldGradient:=TGradient.Create(0,0);
  fOldGradient.CopyFrom(Project.CurrentGradientList[fIndex]);
  fNewGradient:=TGradient.Create(0,0);
  fNewGradient.CopyFrom(iNewGradient);
  fRedoable:=true;
end;

constructor TBDGradientSelectorUndoItem.CreateFromStream(iStream:TStream);
begin
  LoadFromStream(iStream);
end;

destructor TBDGradientSelectorUndoItem.Destroy;
begin
  if Assigned(fNewGradient) then fNewGradient.Free;
  if Assigned(fOldGradient) then fOldGradient.Free;
  inherited Destroy;
end;

procedure TBDGradientSelectorUndoItem.Undo;
begin
  if (fIndex>=0) and (fIndex<Project.CurrentGradientList.Count) then
    Project.CurrentGradientList[fIndex].CopyFrom(fOldGradient);
end;

procedure TBDGradientSelectorUndoItem.Redo;
begin
  if (fIndex>=0) and (fIndex<Project.CurrentGradientList.Count) then
    Project.CurrentGradientList[fIndex].CopyFrom(fNewGradient);
end;

procedure TBDGradientSelectorUndoItem.SaveToStream(pStream:TStream);
var i:integer;curr:int64;s:string;
begin
  s:=UNDOOPERATIONGRADIENTSELECTORID+#1;
  pStream.Write(s[1],4);
  curr:=pStream.Position;
  i:=0;
  pStream.Write(i,4);  // Size placeholder

  pStream.Write(fIndex,4);
  fOldGradient.SaveToStream(pStream);
  fNewGradient.SaveToStream(pStream);
  i:=pStream.Position-curr-4;
  pStream.Position:=curr;
  pStream.write(i,4);
  pStream.Position:=pStream.Position+i;
end;

procedure TBDGradientSelectorUndoItem.LoadFromStream(pStream:TStream);
var size,curr:int64;b:byte;s:string;
begin
  s:=#0#0#0#0;
  pStream.Read(s[1],4);
  if uppercase(copy(s,1,3))<>UNDOOPERATIONGRADIENTSELECTORID then
    raise Exception.Create(Format('GradientSelector undo operation block id expected, got %s.',[copy(s,1,3)]));
  if ord(s[1]) and $20<>0 then Exception.Create('GradientSelector undo operation block cannot be compressed!');
  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;
  b:=ord(s[4]);
  if b=1 then LoadFromStreamV1(pStream)
  else raise Exception.Create(Format('Unknown GradientSelector undo operation block version! (%d)',[b]));
  fRedoable:=true;
  pStream.Position:=curr+size;
end;

procedure TBDGradientSelectorUndoItem.LoadFromStreamV1(pStream:TStream);
begin
  pStream.Read(fIndex,4);
  fOldGradient:=TGradient.CreateFromStream(pStream);
  fNewGradient:=TGradient.CreateFromStream(pStream);
end;

{ TBDGradientSelectorUndoSystem }

procedure TBDGradientSelectorUndoSystem.AddUndo(pGradientIndex:integer;
  pAffectedGradient,pNewGradient:TGradient);
begin
  AddItem(TBDGradientSelectorUndoItem.Create(pGradientIndex,pAffectedGradient,pNewGradient));
end;

procedure TBDGradientSelectorUndoSystem.SaveToStream(pStream:TStream);
var i:integer;curr:int64;s:string;
begin
  s:=UNDOSYSTEMGRADIENTSELECTORID+#1;
  pStream.Write(s[1],4);
  curr:=pStream.Position;
  i:=0;
  pStream.Write(i,4);  // Size placeholder

  i:=Self.Count;
  pStream.Write(i,2);
  pStream.Write(fPointer,2);
  for i:=0 to Self.Count-1 do
    Self[i].SaveToStream(pStream);
  i:=pStream.Position-curr-4;
  pStream.Position:=curr;
  pStream.write(i,4);
  pStream.Position:=pStream.Position+i;
end;

procedure TBDGradientSelectorUndoSystem.LoadFromStream(pStream:TStream);
var size,curr:int64;b:byte;s:string;
begin
  s:=#0#0#0#0;
  pStream.Read(s[1],4);
  if uppercase(copy(s,1,3))<>UNDOSYSTEMGRADIENTSELECTORID then
    raise Exception.Create(Format('GradientSelector undosystem block id expected, got %s.',[copy(s,1,3)]));
  if ord(s[1]) and $20<>0 then Exception.Create('GradientSelector undosystem block cannot be compressed!');

  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;
  b:=ord(s[4]);
  case b of
    1:LoadFromStreamV1(pStream);
    else raise Exception.Create(Format('Unknown GradientSelector undosystem block version! (%d)',[b]));
  end;
  pStream.Position:=curr+size;
end;

procedure TBDGradientSelectorUndoSystem.LoadFromStreamV1(pStream:TStream);
var count,i:integer;
begin
  count:=0;
  pStream.Read(count,2);
  fPointer:=0;
  pStream.Read(fPointer,2);
  if fPointer=65535 then fPointer:=-1;
  for i:=0 to Count-1 do
    Self.Add(TBDGradientSelectorUndoItem.CreateFromStream(pStream));
end;

end.

