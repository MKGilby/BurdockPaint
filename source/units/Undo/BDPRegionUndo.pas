{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPRegionUndo;

{$mode Delphi}

interface

uses
  Classes, SysUtils, BDPUndoBase, BDPRegion;

type

  { TBDRegionUndoItem_DRAW }

  TBDRegionUndoItem_DRAW=class(TBDUndoItem)
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
  end;

  { TBDRegionUndoItem_RESIZE }

  TBDRegionUndoItem_RESIZE=class(TBDUndoItem)
    constructor Create;
    constructor CreateFromStream(iStream:TStream);
    destructor Destroy; override;
    procedure AddAfter;
    procedure Undo; override;
    procedure Redo; override;
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    fBefore,fAfter:TBDRegion;
  end;


  { TBDRegionUndoSystem }

  TBDRegionUndoSystem=class(TBDUndoSystem)
    procedure AddImageUndo(Left,Top,Width,Height:integer;Image:TBDRegion=nil);
    procedure AddImageRedoToLastUndo(Left,Top,Width,Height:integer);
    procedure AddResizeUndo;
    procedure AddResizeRedo;
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    procedure LoadFromStreamV1(pStream:TStream);
  end;

implementation

uses BDPShared, BDPInternalFileFormat;

const
  UNDOOPERATIONREGIONBLOCKID='UDR';
  UNDOSYSTEMREGIONBLOCKID='USR';


{ TBDRegionUndoItem_DRAW }

constructor TBDRegionUndoItem_DRAW.Create(iBefore:TBDRegion);
begin
  inherited Create;
  fBefore:=iBefore;
  fAfter:=nil;
end;

constructor TBDRegionUndoItem_DRAW.CreateFromStream(iStream:TStream);
begin
  inherited Create;
  LoadFromStream(iStream);
end;

destructor TBDRegionUndoItem_DRAW.Destroy;
begin
  if Assigned(fBefore) then fBefore.Free;
  if Assigned(fAfter) then fAfter.Free;
  inherited Destroy;
end;

procedure TBDRegionUndoItem_DRAW.AddAfter(iAfter:TBDRegion);
begin
  fAfter:=iAfter;
  fRedoable:=true;
end;

procedure TBDRegionUndoItem_DRAW.Undo;
begin
  Project.CurrentRegion.PutImage(fBefore.Left,fBefore.Top,fBefore);
end;

procedure TBDRegionUndoItem_DRAW.Redo;
begin
  if Assigned(fAfter) then
    Project.CurrentRegion.PutImage(fAfter.Left,fAfter.Top,fAfter);
end;

procedure TBDRegionUndoItem_DRAW.SaveToStream(pStream:TStream);
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

procedure TBDRegionUndoItem_DRAW.LoadFromStream(pStream:TStream);
begin
  fBefore:=TBDRegion.CreateFromStream(pStream);
  fAfter:=TBDRegion.CreateFromStream(pStream);
  fRedoable:=true;
end;


{ TBDRegionUndoItem_RESIZE }

constructor TBDRegionUndoItem_RESIZE.Create;
begin
  inherited Create;
  fBefore:=TBDRegion.Create(Project.CurrentRegion.Width,Project.CurrentRegion.Height);
  fBefore.PutImage(0,0,Project.CurrentRegion);
  fAfter:=nil;
end;

constructor TBDRegionUndoItem_RESIZE.CreateFromStream(iStream:TStream);
begin
  inherited Create;
  LoadFromStream(iStream);
end;

destructor TBDRegionUndoItem_RESIZE.Destroy;
begin
  if Assigned(fBefore) then fBefore.Free;
  if Assigned(fAfter) then fAfter.Free;
  inherited Destroy;
end;

procedure TBDRegionUndoItem_RESIZE.AddAfter;
begin
  fAfter:=TBDRegion.Create(Project.CurrentRegion.Width,Project.CurrentRegion.Height);
  fAfter.PutImage(0,0,Project.CurrentRegion);
  fRedoable:=true;
end;

procedure TBDRegionUndoItem_RESIZE.Undo;
begin
  Project.CurrentRegion.Recreate(fBefore.Width,fBefore.Height);
  Project.CurrentRegion.PutImage(0,0,fBefore);
end;

procedure TBDRegionUndoItem_RESIZE.Redo;
begin
  Project.CurrentRegion.Recreate(fAfter.Width,fAfter.Height);
  Project.CurrentRegion.PutImage(0,0,fAfter);
end;

procedure TBDRegionUndoItem_RESIZE.SaveToStream(pStream:TStream);
var Xs:TStream;
begin
  if Assigned(fAfter) then begin
    Xs:=TMemoryStream.Create;
    try
      fBefore.SaveToStream(Xs);
      fAfter.SaveToStream(Xs);
      TInternalFileFormat.WriteBlock(pStream,UNDOOPERATIONREGIONBLOCKID,2,Xs,false);
    finally
      Xs.Free;
    end;
  end else
    raise Exception.Create('RegionUndoItem save error: No AfterImage assigned!');
end;

procedure TBDRegionUndoItem_RESIZE.LoadFromStream(pStream:TStream);
begin
  fBefore:=TBDRegion.CreateFromStream(pStream);
  fAfter:=TBDRegion.CreateFromStream(pStream);
  fRedoable:=true;
end;


{ TBDRegionUndoSystem }

procedure TBDRegionUndoSystem.AddImageUndo(Left,Top,Width,Height:integer;Image:TBDRegion);
var atm:TBDRegionUndoItem_DRAW;atmi:TBDRegion;
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
  atm:=TBDRegionUndoItem_DRAW.Create(atmi);
  Self.Add(atm);
  fPointer:=Self.Count-1;
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
      TBDRegionUndoItem_DRAW(Self[fPointer]).AddAfter(atmi);
    end;
  end;
end;

procedure TBDRegionUndoSystem.AddResizeUndo;
begin
  AddItem(TBDRegionUndoItem_RESIZE.Create);
end;

procedure TBDRegionUndoSystem.AddResizeRedo;
begin
  if fPointer>-1 then
    if not Self[fPointer].Redoable then
      TBDRegionUndoItem_RESIZE(Self[fPointer]).AddAfter;
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
end;

procedure TBDRegionUndoSystem.LoadFromStreamV1(pStream:TStream);
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
      if tmp.BlockID<>UNDOOPERATIONREGIONBLOCKID then
        raise Exception.Create(Format('Region undo operation block expected, got %s.',[tmp.BlockID]));
      if tmp.Version=1 then Self.Add(TBDRegionUndoItem_DRAW.CreateFromStream(tmp.Data))
      else if tmp.Version=2 then Self.Add(TBDRegionUndoItem_RESIZE.CreateFromStream(tmp.Data))
      else raise Exception.Create(Format('Unknown Region undo operation block version! (%d)',[tmp.Version]));
    finally
      tmp.Free;
    end;
    dec(count);
  end;
end;

end.

