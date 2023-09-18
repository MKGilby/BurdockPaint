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
  Classes, SysUtils, BDPUndoBase, BDPImage;

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


  { TBDImageUndoSystem }

  TBDImageUndoSystem=class(TBDUndoSystem)
    constructor Create;
    procedure AddImageUndo(Left,Top,Width,Height:integer;Image:TBDRegion=nil);
    procedure AddImageRedoToLastUndo(Left,Top,Width,Height:integer);
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream);  override;
  end;

implementation

uses BDPShared, BDPMessage;

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
  Project.CurrentImage.PutImage(fBefore.Left,fBefore.Top,fBefore);
end;

procedure TBDRegionUndoItem.Redo;
begin
  if Assigned(fAfter) then
    Project.CurrentImage.PutImage(fAfter.Left,fAfter.Top,fAfter);
end;

procedure TBDRegionUndoItem.SaveToStream(pStream:TStream);
var i:integer;curr:int64;s:string;
begin
  if Assigned(fAfter) then begin
    s:=UNDOOPERATIONREGIONBLOCKID+#1;
    pStream.Write(s[1],4);
    curr:=pStream.Position;
    i:=0;
    pStream.Write(i,4);
    fBefore.SaveToStream(pStream);
    fAfter.SaveToStream(pStream);
    i:=pStream.Position-curr-4;
    pStream.Position:=curr;
    pStream.write(i,4);
    pStream.Position:=pStream.Position+i;
  end else
    raise Exception.Create('UndoImageItem save error: No AfterImage assigned!');
end;

procedure TBDRegionUndoItem.LoadFromStream(pStream:TStream);
var size,curr:int64;b:byte;s:string;
begin
  s:=#0#0#0#0;
  pStream.Read(s[1],4);
  if uppercase(copy(s,1,3))<>UNDOOPERATIONREGIONBLOCKID then
    raise Exception.Create(Format('Region undo operation block id expected, got %s.',[copy(s,1,3)]));
  if ord(s[1]) and $20<>0 then Exception.Create('Region undo operation block cannot be compressed!');
  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;
  b:=ord(s[4]);
  if b=1 then LoadFromStreamV1(pStream)
  else raise Exception.Create(Format('Unknown region undo operation block version! (%d)',[b]));
  fRedoable:=true;
  pStream.Position:=curr+size;
end;

procedure TBDRegionUndoItem.LoadFromStreamV1(pStream:TStream);
begin
  fBefore:=TBDRegion.CreateFromStream(pStream);
  fAfter:=TBDRegion.CreateFromStream(pStream);
end;


{ TBDImageUndoSystem }

constructor TBDImageUndoSystem.Create;
begin
  inherited Create;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_SETIMAGEUNDOREDOBUTTON,0,0);
end;

procedure TBDImageUndoSystem.AddImageUndo(Left,Top,Width,Height:integer;Image:TBDRegion);
var atm:TBDRegionUndoItem;atmi:TBDRegion;
begin
  if (fPointer<>Self.Count-1) then   // If not the last item, delete items after it.
    Self.DeleteRange(fPointer+1,Self.Count-1);
  if Self.Count=Settings.UndoLimit then Self.Delete(0);
  atmi:=TBDRegion.Create(Width,Height);
  atmi.Left:=Left;
  atmi.Top:=Top;
  if Image=nil then
    atmi.PutImagePart(0,0,Left,Top,Width,Height,Project.CurrentImage)
  else
    atmi.PutImagePart(0,0,Left,Top,Width,Height,Image);
  atm:=TBDRegionUndoItem.Create(atmi);
  Self.Add(atm);
  fPointer:=Self.Count-1;
  MessageQueue.AddMessage(fAfterUndoRedoMessage);
end;

procedure TBDImageUndoSystem.AddImageRedoToLastUndo(Left,Top,Width,Height:integer);
var atmi:TBDRegion;
begin
  if fPointer>-1 then begin
    if not Self[fPointer].Redoable then begin
      atmi:=TBDRegion.Create(Width,Height);
      atmi.Left:=Left;
      atmi.Top:=Top;
      atmi.PutImagePart(0,0,Left,Top,Width,Height,Project.CurrentImage);
      TBDRegionUndoItem(Self[fPointer]).AddAfter(atmi);
      MessageQueue.AddMessage(fAfterUndoRedoMessage);
    end;
  end;
end;

procedure TBDImageUndoSystem.SaveToStream(pStream:TStream);
var i:integer;curr:int64;s:string;
begin
  s:=UNDOSYSTEMREGIONBLOCKID+#1;
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

procedure TBDImageUndoSystem.LoadFromStream(pStream:TStream);
var size,curr:int64;b:byte;count,i:integer;s:string;
begin
  s:=#0#0#0#0;
  pStream.Read(s[1],4);
  if uppercase(copy(s,1,3))<>UNDOSYSTEMREGIONBLOCKID then
    raise Exception.Create(Format('Region undosystem block id expected, got %s.',[copy(s,1,3)]));
  if ord(s[1]) and $20<>0 then Exception.Create('Region undosystem block cannot be compressed!');

  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;
  b:=ord(s[4]);
  if b=1 then begin
    count:=0;
    pStream.Read(count,2);
    fPointer:=0;
    pStream.Read(fPointer,2);
    if fPointer=65535 then fPointer:=-1;
    for i:=0 to Count-1 do
      Self.Add(TBDRegionUndoItem.CreateFromStream(pStream));
  end else raise Exception.Create(Format('Unknown region undosystem block version! (%d)',[b]));
  pStream.Position:=curr+size;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_SETIMAGEUNDOREDOBUTTON,0,0);
end;

end.

