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

unit BDPRegionUndo;

{$mode Delphi}

interface

uses
  Classes, SysUtils, BDPUndoBase, BDPRegion;

type

  { TBDRegionUndoItem }

  TBDRegionUndoItem=class(TBDUndoItem)
    constructor Create(iBefore:TBDRegion);
    constructor CreateFromStream(iStream:TStream);
    destructor Destroy; override;
    procedure AddAfter(iAfter:TBDRegion);
    procedure Undo; override;
    procedure Redo; override;
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    fBefore,fAfter:TBDRegion;
    procedure LoadFromStreamV1(pStream:TStream);
  end;


  { TBDRegionUndoSystem }

  TBDRegionUndoSystem=class(TBDUndoSystem)
    constructor Create;
    procedure AddImageUndo(Left,Top,Width,Height:integer;Image:TBDRegion=nil);
    procedure AddImageRedoToLastUndo(Left,Top,Width,Height:integer);
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    procedure LoadFromStreamV1(pStream:TStream);
  end;

implementation

uses BDPShared, BDPMessage, BDPInternalFileFormat;

const
  UNDOOPERATIONREGIONBLOCKID='UDR';
  UNDOSYSTEMREGIONBLOCKID='USR';


{ TBDRegionUndoItem }

constructor TBDRegionUndoItem.Create(iBefore:TBDRegion);
begin
  inherited Create;
  fBefore:=iBefore;
  fAfter:=nil;
end;

constructor TBDRegionUndoItem.CreateFromStream(iStream:TStream);
begin
  inherited Create;
  LoadFromStream(iStream);
end;

destructor TBDRegionUndoItem.Destroy;
begin
  if Assigned(fBefore) then FreeAndNil(fBefore);
  if Assigned(fAfter) then FreeAndNil(fAfter);
  inherited Destroy;
end;

procedure TBDRegionUndoItem.AddAfter(iAfter:TBDRegion);
begin
  fAfter:=iAfter;
  fRedoable:=true;
end;

procedure TBDRegionUndoItem.Undo;
begin
  Project.CurrentRegion.PutImage(fBefore.Left,fBefore.Top,fBefore);
end;

procedure TBDRegionUndoItem.Redo;
begin
  if Assigned(fAfter) then
    Project.CurrentRegion.PutImage(fAfter.Left,fAfter.Top,fAfter);
end;

procedure TBDRegionUndoItem.SaveToStream(pStream:TStream);
var Xs:TStream;
begin
  if Assigned(fAfter) then begin
    Xs:=TMemoryStream.Create;
    try
      fBefore.SaveToStream(Xs);
      fAfter.SaveToStream(Xs);
      TInternalFileFormat.WriteBlock(pStream,UNDOOPERATIONREGIONBLOCKID,1,Xs,false);
    finally
      Xs.Free;
    end;
  end else
    raise Exception.Create('RegionUndoItem save error: No AfterImage assigned!');
end;

procedure TBDRegionUndoItem.LoadFromStream(pStream:TStream);
var tmp:TInternalBlock;
begin
  tmp:=TInternalFileFormat.ReadBlock(pStream);
  try
    if tmp.BlockID<>UNDOOPERATIONREGIONBLOCKID then
      raise Exception.Create(Format('Region undo operation block expected, got %s.',[tmp.BlockID]));
    if tmp.Version=1 then LoadFromStreamV1(tmp.Data)
    else raise Exception.Create(Format('Unknown region undo operation block version! (%d)',[tmp.Version]));
  finally
    tmp.Free;
  end;
  fRedoable:=true;
end;

procedure TBDRegionUndoItem.LoadFromStreamV1(pStream:TStream);
begin
  fBefore:=TBDRegion.CreateFromStream(pStream);
  fAfter:=TBDRegion.CreateFromStream(pStream);
end;


{ TBDRegionUndoSystem }

constructor TBDRegionUndoSystem.Create;
begin
  inherited Create;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_SETIMAGEUNDOREDOBUTTON,0,0);
end;

procedure TBDRegionUndoSystem.AddImageUndo(Left,Top,Width,Height:integer;Image:TBDRegion);
var atm:TBDRegionUndoItem;atmi:TBDRegion;
begin
  if (fPointer<>Self.Count-1) then   // If not the last item, delete items after it.
    Self.DeleteRange(fPointer+1,Self.Count-1);
  if Self.Count=Settings.UndoLimit then Self.Delete(0);
  atmi:=TBDRegion.Create(Width,Height);
  atmi.Left:=Left;
  atmi.Top:=Top;
  if Image=nil then
    atmi.PutImagePart(0,0,Left,Top,Width,Height,Project.CurrentRegion)
  else
    atmi.PutImagePart(0,0,Left,Top,Width,Height,Image);
  atm:=TBDRegionUndoItem.Create(atmi);
  Self.Add(atm);
  fPointer:=Self.Count-1;
  MessageQueue.AddMessage(fAfterUndoRedoMessage);
end;

procedure TBDRegionUndoSystem.AddImageRedoToLastUndo(Left,Top,Width,Height:integer);
var atmi:TBDRegion;
begin
  if fPointer>-1 then begin
    if not Self[fPointer].Redoable then begin
      atmi:=TBDRegion.Create(Width,Height);
      atmi.Left:=Left;
      atmi.Top:=Top;
      atmi.PutImagePart(0,0,Left,Top,Width,Height,Project.CurrentRegion);
      TBDRegionUndoItem(Self[fPointer]).AddAfter(atmi);
      MessageQueue.AddMessage(fAfterUndoRedoMessage);
    end;
  end;
end;

procedure TBDRegionUndoSystem.SaveToStream(pStream:TStream);
var i:integer;Xs:TStream;
begin
  Xs:=TMemoryStream.Create;
  try
    i:=Self.Count;
    Xs.Write(i,2);
    Xs.Write(fPointer,2);
    for i:=0 to Self.Count-1 do
      Self[i].SaveToStream(Xs);
    TInternalFileFormat.WriteBlock(pStream,UNDOSYSTEMREGIONBLOCKID,1,Xs,false);
  finally
    Xs.Free;
  end;
end;

procedure TBDRegionUndoSystem.LoadFromStream(pStream:TStream);
var tmp:TInternalBlock;
begin
  tmp:=TInternalFileFormat.ReadBlock(pStream);
  try
    if tmp.BlockID<>UNDOSYSTEMREGIONBLOCKID then
      raise Exception.Create(Format('Region undosystem block expected, got %s.',[tmp.BlockID]));
    if tmp.Version=1 then LoadFromStreamV1(tmp.Data)
    else raise Exception.Create(Format('Unknown region undosystem block version! (%d)',[tmp.Version]));
  finally
    tmp.Free;
  end;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_SETIMAGEUNDOREDOBUTTON,0,0);
end;

procedure TBDRegionUndoSystem.LoadFromStreamV1(pStream:TStream);
var count:integer;
begin
  count:=0;
  pStream.Read(count,2);
  fPointer:=0;
  pStream.Read(fPointer,2);
  if fPointer=65535 then fPointer:=-1;
  while count>0 do begin
    Self.Add(TBDRegionUndoItem.CreateFromStream(pStream));
    dec(count);
  end;
end;

end.

