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

  { TBDGradientSelectorUndoItem_EDIT }

  TBDGradientSelectorUndoItem_EDIT=class(TBDUndoItem)
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
  end;

  { TBDGradientSelectorUndoItem_DELETE }

  TBDGradientSelectorUndoItem_DELETE=class(TBDUndoItem)
    constructor Create(iGradientIndex:integer);
    constructor CreateFromStream(iStream:TStream);
    destructor Destroy; override;
    procedure Undo; override;
    procedure Redo; override;
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    fIndex:integer;
    fOldGradient:TGradient;
  end;

  { TBDGradientSelectorUndoItem_ADD }

  TBDGradientSelectorUndoItem_ADD=class(TBDUndoItem)
    constructor Create(iGradientIndex:integer);
    constructor CreateFromStream(iStream:TStream);
    procedure Undo; override;
    procedure Redo; override;
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    fIndex:integer;
  end;

  { TBDGradientSelectorUndoSystem }

  TBDGradientSelectorUndoSystem=class(TBDUndoSystem)
    procedure AddUndo_EDIT(pGradientIndex:integer;pAffectedGradient,pNewGradient:TGradient);
    procedure AddUndo_DELETE(pGradientIndex:integer);
    procedure AddUndo_ADD(pGradientIndex:integer);
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    procedure LoadFromStreamV1(pStream:TStream);
  end;


implementation

uses BDPShared, BDPInternalFileFormat;

const
  UNDOOPERATIONGRADIENTSELECTORID='UDG';
  UNDOSYSTEMGRADIENTSELECTORID='USG';

{ TBDGradientSelectorUndoItem_EDIT }

constructor TBDGradientSelectorUndoItem_EDIT.Create(iGradientIndex:integer;
  iAffectedGradient,iNewGradient:TGradient);
begin
  fIndex:=iGradientIndex;
  fOldGradient:=TGradient.Create(0,0);
  fOldGradient.CopyFrom(Project.CurrentGradientList[fIndex]);
  fNewGradient:=TGradient.Create(0,0);
  fNewGradient.CopyFrom(iNewGradient);
  fRedoable:=true;
end;

constructor TBDGradientSelectorUndoItem_EDIT.CreateFromStream(iStream:TStream);
begin
  LoadFromStream(iStream);
end;

destructor TBDGradientSelectorUndoItem_EDIT.Destroy;
begin
  if Assigned(fNewGradient) then fNewGradient.Free;
  if Assigned(fOldGradient) then fOldGradient.Free;
  inherited Destroy;
end;

procedure TBDGradientSelectorUndoItem_EDIT.Undo;
begin
  if (fIndex>=0) and (fIndex<Project.CurrentGradientList.Count) then
    Project.CurrentGradientList[fIndex].CopyFrom(fOldGradient);
end;

procedure TBDGradientSelectorUndoItem_EDIT.Redo;
begin
  if (fIndex>=0) and (fIndex<Project.CurrentGradientList.Count) then
    Project.CurrentGradientList[fIndex].CopyFrom(fNewGradient);
end;

procedure TBDGradientSelectorUndoItem_EDIT.SaveToStream(pStream:TStream);
var Xs:TStream;
begin
  Xs:=TMemoryStream.Create;
  try
    Xs.Write(fIndex,4);
    fOldGradient.SaveToStream(Xs);
    fNewGradient.SaveToStream(Xs);
    TInternalFileFormat.WriteBlock(pStream,UNDOOPERATIONGRADIENTSELECTORID,1,Xs,false);
  finally
    Xs.Free;
  end;
end;

procedure TBDGradientSelectorUndoItem_EDIT.LoadFromStream(pStream:TStream);
begin
  pStream.Read(fIndex,4);
  fOldGradient:=TGradient.CreateFromStream(pStream);
  fNewGradient:=TGradient.CreateFromStream(pStream);
  fRedoable:=true;
end;


{ TBDGradientSelectorUndoItem_DELETE }

constructor TBDGradientSelectorUndoItem_DELETE.Create(iGradientIndex:integer);
begin
  fIndex:=iGradientIndex;
  fOldGradient:=TGradient.Create(0,0);
  fOldGradient.CopyFrom(Project.CurrentGradientList[fIndex]);
  fRedoable:=true;
end;

constructor TBDGradientSelectorUndoItem_DELETE.CreateFromStream(iStream:TStream);
begin
  LoadFromStream(iStream);
end;

destructor TBDGradientSelectorUndoItem_DELETE.Destroy;
begin
  if Assigned(fOldGradient) then fOldGradient.Free;
  inherited Destroy;
end;

procedure TBDGradientSelectorUndoItem_DELETE.Undo;
var tmp:TGradient;
begin
  tmp:=TGradient.Create(0,0);
  tmp.CopyFrom(fOldGradient);
  Project.CurrentGradientList.Insert(fIndex,tmp);
end;

procedure TBDGradientSelectorUndoItem_DELETE.Redo;
begin
  Project.CurrentGradientList.Delete(fIndex);
end;

procedure TBDGradientSelectorUndoItem_DELETE.SaveToStream(pStream:TStream);
var Xs:TStream;
begin
  Xs:=TMemoryStream.Create;
  try
    Xs.Write(fIndex,4);
    fOldGradient.SaveToStream(Xs);
    TInternalFileFormat.WriteBlock(pStream,UNDOOPERATIONGRADIENTSELECTORID,2,Xs,false);
  finally
    Xs.Free;
  end;
end;

procedure TBDGradientSelectorUndoItem_DELETE.LoadFromStream(pStream:TStream);
begin
  pStream.Read(fIndex,4);
  fOldGradient:=TGradient.CreateFromStream(pStream);
  fRedoable:=true;
end;

{ TBDGradientSelectorUndoItem_ADD }

constructor TBDGradientSelectorUndoItem_ADD.Create(iGradientIndex:integer);
begin
  fIndex:=iGradientIndex;
  fRedoable:=true;
end;

constructor TBDGradientSelectorUndoItem_ADD.CreateFromStream(iStream:TStream);
begin
  LoadFromStream(iStream);
end;

procedure TBDGradientSelectorUndoItem_ADD.Undo;
begin
  Project.CurrentGradientList.Delete(fIndex);
end;

procedure TBDGradientSelectorUndoItem_ADD.Redo;
begin
  Project.CurrentGradientList.Insert(fIndex,TGradient.Create($ff000000,$ffffffff));
end;

procedure TBDGradientSelectorUndoItem_ADD.SaveToStream(pStream:TStream);
var Xs:TStream;
begin
  Xs:=TMemoryStream.Create;
  try
    Xs.Write(fIndex,4);
    TInternalFileFormat.WriteBlock(pStream,UNDOOPERATIONGRADIENTSELECTORID,3,Xs,false);
  finally
    Xs.Free;
  end;
end;

procedure TBDGradientSelectorUndoItem_ADD.LoadFromStream(pStream:TStream);
begin
  pStream.Read(pStream,4);
  fRedoable:=true;
end;


{ TBDGradientSelectorUndoSystem }

procedure TBDGradientSelectorUndoSystem.AddUndo_EDIT(pGradientIndex:integer;
  pAffectedGradient,pNewGradient:TGradient);
begin
  AddItem(TBDGradientSelectorUndoItem_EDIT.Create(pGradientIndex,pAffectedGradient,pNewGradient));
end;

procedure TBDGradientSelectorUndoSystem.AddUndo_DELETE(pGradientIndex:integer);
begin
  AddItem(TBDGradientSelectorUndoItem_DELETE.Create(pGradientIndex));
end;

procedure TBDGradientSelectorUndoSystem.AddUndo_ADD(pGradientIndex:integer);
begin
  AddItem(TBDGradientSelectorUndoItem_ADD.Create(pGradientIndex));
end;

procedure TBDGradientSelectorUndoSystem.SaveToStream(pStream:TStream);
var i:integer;Xs:TStream;
begin
  Xs:=TMemoryStream.Create;
  try
    i:=Self.Count;
    Xs.Write(i,2);
    Xs.Write(fPointer,2);
    for i:=0 to Self.Count-1 do
      Self[i].SaveToStream(Xs);
    TInternalFileFormat.WriteBlock(pStream,UNDOSYSTEMGRADIENTSELECTORID,1,Xs);
  finally
    Xs.Free;
  end;
end;

procedure TBDGradientSelectorUndoSystem.LoadFromStream(pStream:TStream);
var tmp:TInternalBlock;
begin
  tmp:=TInternalFileFormat.ReadBlock(pStream);
  try
    if tmp.BlockID<>UNDOSYSTEMGRADIENTSELECTORID then
      raise Exception.Create(Format('GradientSelector undosystem block expected, got %s.',[tmp.BlockID]));
    if tmp.Version=1 then LoadFromStreamV1(tmp.Data)
    else raise Exception.Create(Format('Unknown GradientSelector undosystem block version! (%d)',[tmp.Version]));
  finally
    tmp.Free;
  end;
end;

procedure TBDGradientSelectorUndoSystem.LoadFromStreamV1(pStream:TStream);
var count:integer;tmp:TInternalBlock;
begin
  count:=0;
  pStream.Read(count,2);
  fPointer:=0;
  pStream.Read(fPointer,2);
  if fPointer=65535 then fPointer:=-1;
  while count>0 do begin
    tmp:=TInternalFileFormat.ReadBlock(pStream);
    try
      if tmp.BlockID<>UNDOOPERATIONGRADIENTSELECTORID then
        raise Exception.Create(Format('GradientSelector undo operation block expected, got %s.',[tmp.BlockID]));
      if tmp.Version=1 then Self.Add(TBDGradientSelectorUndoItem_EDIT.CreateFromStream(tmp.Data))
      else if tmp.Version=2 then Self.Add(TBDGradientSelectorUndoItem_DELETE.CreateFromStream(tmp.Data))
      else if tmp.Version=3 then Self.Add(TBDGradientSelectorUndoItem_ADD.CreateFromStream(tmp.Data))
      else raise Exception.Create(Format('Unknown GradientSelector undo operation block version! (%d)',[tmp.Version]));
    finally
      tmp.Free;
    end;
    dec(count);
  end;
end;

end.

