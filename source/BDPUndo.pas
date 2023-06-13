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

unit BDPUndo;

{$mode Delphi}

interface

uses Classes, Sysutils, fgl, BDPImage, BDPPalette, BDPMessage;

type

  { TBDUndoItem }

  TBDUndoItem=class
    constructor Create;
    procedure Undo; virtual; abstract;
    procedure Redo; virtual; abstract;
    procedure SaveToFile(Filename:string);
    procedure SaveToStream(Target:TStream); virtual; abstract;
    procedure LoadFromFile(Filename:string);
    procedure LoadFromStream(Target:TStream); virtual; abstract;
  private
    fRedoable:boolean;
  public
    property Redoable:boolean read fRedoable;
  end;

  { TBDRegionUndoItem }

  TBDRegionUndoItem=class(TBDUndoItem)
    constructor Create(iBefore:TBDImage);
    constructor CreateFromStream(iStream:TStream);
    destructor Destroy; override;
    procedure AddAfter(iAfter:TBDImage);
    procedure Undo; override;
    procedure Redo; override;
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    fBefore,fAfter:TBDImage;
    procedure LoadFromStreamV1(pStream:TStream);
  end;

  { TBDPaletteUndoItem }

  TBDPaletteUndoItem=class(TBDUndoItem)
    constructor Create(iStart:integer;iBefore:TBDPalette);
    constructor CreateFromStream(iStream:TStream);
    destructor Destroy; override;
    procedure AddAfter(pAfter:TBDPalette);
    procedure Undo; override;
    procedure Redo; override;
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    fStart:integer;
    fBefore,fAfter:TBDPalette;
    procedure LoadFromStreamV1(pStream:TStream);
  public
    property Start:integer read fStart;
    property After:TBDPalette read fAfter;
  end;

  { TBDSingleColorUndoItem }

  TBDSingleColorUndoItem=class(TBDUndoItem)
    constructor Create(iStart:integer;iBefore:uint32);
    constructor CreateFromStream(iStream:TStream);
    procedure AddAfter(pAfter:uint32);
    procedure Undo; override;
    procedure Redo; override;
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    fStart:integer;
    fBefore,fAfter:uint32;
    procedure LoadFromStreamV1(pStream:TStream);
  public
    property Start:integer read fStart;
  end;

  TBDUndoList=TFPGObjectList<TBDUndoItem>;

  { TBDUndoSystem }

  TBDUndoSystem=class(TBDUndoList)
    constructor Create;
    constructor CreateFromStream(pStream:TStream);
    destructor Destroy; override;
    procedure Undo;
    procedure Redo;
    procedure SaveToFile(pFilename:string);
    procedure SaveToStream(pStream:TStream); virtual; abstract;
    procedure LoadFromFile(pFilename:string);
    procedure LoadFromStream(pStream:TStream); virtual; abstract;
    function CanUndo:boolean;
    function CanRedo:boolean;
  private
    fPointer:integer;  // points to the item that will be undoed if requested.
  protected
    fAfterUndoRedoMessage:TMessage;
  end;

  { TBDImageUndoSystem }

  TBDImageUndoSystem=class(TBDUndoSystem)
    constructor Create;
    procedure AddImageUndo(Left,Top,Width,Height:integer;Image:TBDImage=nil);
    procedure AddImageRedoToLastUndo(Left,Top,Width,Height:integer);
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream);  override;
  end;

  { TBDPaletteUndoSystem }

  TBDPaletteUndoSystem=class(TBDUndoSystem)
    constructor Create;
    procedure AddPaletteUndo(Start,Count:integer);
    procedure AddPaletteRedoToLastUndo(Start,Count:integer);
    procedure AddSingleColorUndo(pStart:integer);
    procedure AddSingleColorRedoToLastUndo;
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  end;

implementation

uses BDPShared, BDPSettings;

const
  UNDOSYSTEMREGIONID=$55;
  UNDOOPERATIONREGIONID=$4F;
  UNDOSYSTEMPALETTEID=$56;
  UNDOOPERATIONPALETTEID=$51;
  UNDOOPERATIONSINGLECOLORID=$53;

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
  FreeAndNil(Xs);
end;

procedure TBDUndoItem.LoadFromFile(Filename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(Filename,fmOpenRead or fmShareDenyWrite);
  LoadFromStream(Xs);
  FreeAndNil(Xs);
end;

{ TBDRegionUndoItem }

constructor TBDRegionUndoItem.Create(iBefore:TBDImage);
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

procedure TBDRegionUndoItem.AddAfter(iAfter:TBDImage);
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
var i:integer;curr:int64;
begin
  if Assigned(fAfter) then begin
    i:=UNDOOPERATIONREGIONID;
    pStream.Write(i,1);
    curr:=pStream.Position;
    i:=0;
    pStream.Write(i,4);
    i:=1;                // Version
    pStream.Write(i,1);
    fBefore.SaveWholeImageDataToStream(pStream);
    fAfter.SaveWholeImageDataToStream(pStream);
    i:=pStream.Position-curr-4;
    pStream.Position:=curr;
    pStream.write(i,4);
    pStream.Position:=pStream.Position+i;
  end else
    raise Exception.Create('UndoImageItem save error: No AfterImage assigned!');
end;

procedure TBDRegionUndoItem.LoadFromStream(pStream:TStream);
var size,curr:int64;b:byte;
begin
  b:=0;
  pStream.Read(b,1);
  if b<>UNDOOPERATIONREGIONID then raise Exception.Create(Format('ID is not for region undo operation data! (%.2x)',[b]));
  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;
  pstream.Read(b,1);
  if b=1 then LoadFromStreamV1(pStream)
  else raise Exception.Create(Format('Unknown RegionUndoItem version! (%d)',[b]));
  fRedoable:=true;
  pStream.Position:=curr+size;
end;

procedure TBDRegionUndoItem.LoadFromStreamV1(pStream:TStream);
begin
  fBefore:=TBDImage.Create(16,16);
  fBefore.LoadWholeImageDataFromStream(pStream);
  fAfter:=TBDImage.Create(16,16);
  fAfter.LoadWholeImageDataFromStream(pStream);
end;

{ TBDPaletteUndoItem }

constructor TBDPaletteUndoItem.Create(iStart:integer; iBefore:TBDPalette);
begin
  fStart:=iStart;
  fBefore:=iBefore;
end;

constructor TBDPaletteUndoItem.CreateFromStream(iStream:TStream);
begin
  inherited Create;
  LoadFromStream(iStream);
end;

destructor TBDPaletteUndoItem.Destroy;
begin
  if Assigned(fBefore) then FreeAndNil(fBefore);
  if Assigned(fAfter) then FreeAndNil(fAfter);
  inherited Destroy;
end;

procedure TBDPaletteUndoItem.AddAfter(pAfter:TBDPalette);
begin
  fAfter:=pAfter;
  fRedoable:=true;
end;

procedure TBDPaletteUndoItem.Undo;
begin
  Project.CurrentImage.Palette.CopyColorsFrom(fBefore,0,fStart,fBefore.Size);
end;

procedure TBDPaletteUndoItem.Redo;
begin
  if Assigned(fAfter) then
    Project.CurrentImage.Palette.CopyColorsFrom(fAfter,0,fStart,fAfter.Size);
end;

procedure TBDPaletteUndoItem.SaveToStream(pStream:TStream);
var i:uint32;curr:int64;
begin
  if Assigned(fAfter) then begin
    i:=UNDOOPERATIONPALETTEID;
    pStream.Write(i,1);
    curr:=pStream.Position;
    i:=0;
    pStream.Write(i,4);
    i:=1;
    pStream.Write(i,1);
    pStream.Write(fStart,2);
    fBefore.SaveToStream(pStream);
    fAfter.SaveToStream(pStream);
    i:=pStream.Position-curr-4;
    pStream.Position:=curr;
    pStream.write(i,4);
    pStream.Position:=pStream.Position+i;
  end else
    raise Exception.Create('UndoColorItem save error: No AfterPalette assigned!');
end;

procedure TBDPaletteUndoItem.LoadFromStream(pStream:TStream);
var size,curr:int64;b:byte;
begin
  b:=0;
  pStream.Read(b,1);
  if b<>UNDOOPERATIONPALETTEID then raise Exception.Create(Format('ID is not for palette undo operation data! (%.2x)',[b]));
  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;
  pStream.Read(b,1);
  if b=1 then LoadFromStreamV1(pStream)
  else raise Exception.Create(Format('Unknown ColorUndoItem version! (%d)',[b]));
  pStream.Position:=curr+size;
  fRedoable:=true;
end;

procedure TBDPaletteUndoItem.LoadFromStreamV1(pStream:TStream);
begin
  fStart:=0;
  pStream.Read(fStart,2);
  fBefore:=TBDPalette.Create;
  fBefore.LoadFromStream(pStream);
  fAfter:=TBDPalette.Create;
  fAfter.LoadFromStream(pStream);
end;

{ TBDSingleColorUndoItem }

constructor TBDSingleColorUndoItem.Create(iStart:integer; iBefore:uint32);
begin
  fStart:=iStart;
  fBefore:=iBefore;
end;

constructor TBDSingleColorUndoItem.CreateFromStream(iStream:TStream);
begin
  inherited Create;
  LoadFromStream(iStream);
end;

procedure TBDSingleColorUndoItem.AddAfter(pAfter:uint32);
begin
  fAfter:=pAfter;
end;

procedure TBDSingleColorUndoItem.Undo;
begin
  Project.CurrentImage.Palette[fStart]:=fBefore;
end;

procedure TBDSingleColorUndoItem.Redo;
begin
  Project.CurrentImage.Palette[fStart]:=fAfter;
end;

procedure TBDSingleColorUndoItem.SaveToStream(pStream:TStream);
var i:uint32;curr:int64;
begin
  i:=UNDOOPERATIONSINGLECOLORID;
  pStream.Write(i,1);
  curr:=pStream.Position;
  i:=0;
  pStream.Write(i,4);
  i:=1;
  pStream.Write(i,1);
  pStream.Write(fStart,2);
  pStream.Write(fBefore,4);
  pStream.Write(fAfter,4);
  i:=pStream.Position-curr-4;
  pStream.Position:=curr;
  pStream.write(i,4);
  pStream.Position:=pStream.Position+i;
end;

procedure TBDSingleColorUndoItem.LoadFromStream(pStream:TStream);
var size,curr:int64;b:byte;
begin
  b:=0;
  pStream.Read(b,1);
  if b<>UNDOOPERATIONSINGLECOLORID then raise Exception.Create(Format('ID is not for single color undo operation data! (%.2x)',[b]));
  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;
  pStream.Read(b,1);
  if b=1 then LoadFromStreamV1(pStream)
  else raise Exception.Create(Format('Unknown SingleColorUndoItem version! (%d)',[b]));
  pStream.Position:=curr+size;
  fRedoable:=true;
end;

procedure TBDSingleColorUndoItem.LoadFromStreamV1(pStream:TStream);
begin
  fStart:=0;
  pStream.Read(fStart,2);
  pStream.Read(fBefore,4);
  pStream.Read(fAfter,4);
end;

{ TBDUndoSystem }

constructor TBDUndoSystem.Create;
begin
  inherited Create;
  fPointer:=-1;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_NONE,0);
end;

constructor TBDUndoSystem.CreateFromStream(pStream:TStream);
begin
  inherited Create;
  LoadFromStream(pStream);
end;

destructor TBDUndoSystem.Destroy;
begin
  inherited Destroy;
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

procedure TBDUndoSystem.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead or fmShareDenyNone);
  LoadFromStream(Xs);
  FreeAndNil(Xs);
end;

function TBDUndoSystem.CanUndo: boolean;
begin
  Result:=fPointer>-1;
end;

function TBDUndoSystem.CanRedo: boolean;
begin
  Result:=fPointer<Self.Count-1;
end;

{ TBDImageUndoSystem }

constructor TBDImageUndoSystem.Create;
begin
  inherited Create;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_SETIMAGEUNDOREDOBUTTON,0);
end;

procedure TBDImageUndoSystem.AddImageUndo(Left,Top,Width,Height:integer;Image:TBDImage);
var atm:TBDRegionUndoItem;atmi:TBDImage;
begin
  if (fPointer<>Self.Count-1) then   // If not the last item, delete items after it.
    Self.DeleteRange(fPointer+1,Self.Count-1);
  if Self.Count=Settings.UndoLimit then Self.Delete(0);
  atmi:=TBDImage.Create(Width,Height);
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
var atmi:TBDImage;
begin
  if fPointer>-1 then begin
    if not Self[fPointer].Redoable then begin
      atmi:=TBDImage.Create(Width,Height);
      atmi.Left:=Left;
      atmi.Top:=Top;
      atmi.PutImagePart(0,0,Left,Top,Width,Height,Project.CurrentImage);
      TBDRegionUndoItem(Self[fPointer]).AddAfter(atmi);
      MessageQueue.AddMessage(fAfterUndoRedoMessage);
    end;
  end;
end;

procedure TBDImageUndoSystem.SaveToStream(pStream:TStream);
var i:integer;curr:int64;
begin
  i:=UNDOSYSTEMREGIONID;
  pStream.Write(i,1);
  curr:=pStream.Position;
  i:=0;
  pStream.Write(i,4);
  i:=1;
  pStream.Write(i,1);  // Version
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
var size,curr:int64;b:byte;count,i:integer;
begin
  b:=0;
  pStream.Read(b,1);
  if b<>UNDOSYSTEMREGIONID then raise Exception.Create(Format('ID is not for region undosystem data! (%.2x)',[b]));
  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;
  pStream.read(b,1);
  if b=1 then begin // Version 1
    count:=0;
    pStream.Read(count,2);
    fPointer:=0;
    pStream.Read(fPointer,2);
    if fPointer=65535 then fPointer:=-1;
    for i:=0 to Count-1 do
      Self.Add(TBDRegionUndoItem.CreateFromStream(pStream));
  end;
  pStream.Position:=curr+size;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_SETIMAGEUNDOREDOBUTTON,0);
end;

{ TBDPaletteUndoSystem }

constructor TBDPaletteUndoSystem.Create;
begin
  inherited Create;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_SETPALETTEUNDOREDOBUTTON,0);
end;

procedure TBDPaletteUndoSystem.AddPaletteUndo(Start,Count:integer);
var atm:TBDPaletteUndoItem;atmP:TBDPalette;
begin
  if (fPointer<>Self.Count-1) then   // If not the last item, delete items after it.
    Self.DeleteRange(fPointer+1,Self.Count-1);
  if Self.Count=Settings.UndoLimit then Self.Delete(0);
  atmP:=TBDPalette.Create(Count);
  atmP.ResizeAndCopyColorsFrom(Project.CurrentImage.Palette,Start,Count);
  atm:=TBDPaletteUndoItem.Create(Start,atmP);
  Self.Add(atm);
  fPointer:=Self.Count-1;
  MessageQueue.AddMessage(fAfterUndoRedoMessage);
end;

procedure TBDPaletteUndoSystem.AddPaletteRedoToLastUndo(Start,Count:integer);
var atmP:TBDPalette;
begin
  if fPointer>-1 then begin
    if not Self[fPointer].Redoable then begin
      atmP:=TBDPalette.Create(Count);
      atmP.ResizeAndCopyColorsFrom(Project.CurrentImage.Palette,Start,Count);
      TBDPaletteUndoItem(Self[fPointer]).AddAfter(atmP);
      MessageQueue.AddMessage(fAfterUndoRedoMessage);
    end;
  end;
end;

procedure TBDPaletteUndoSystem.AddSingleColorUndo(pStart:integer);
var atm:TBDSingleColorUndoItem;
begin
  if (fPointer<>Self.Count-1) then   // If not the last item, delete items after it.
    Self.DeleteRange(fPointer+1,Self.Count-1);
  if not (
      (fPointer>-1) and
      (Self[fPointer] is TBDSingleColorUndoItem) and
      (TBDSingleColorUndoItem(Self[fPointer]).Start=pStart)) then begin
    if Self.Count=Settings.UndoLimit then Self.Delete(0);
    atm:=TBDSingleColorUndoItem.Create(pStart,Project.CurrentImage.Palette[pStart]);
    Self.Add(atm);
    fPointer:=Self.Count-1;
    MessageQueue.AddMessage(fAfterUndoRedoMessage);
  end;
end;

procedure TBDPaletteUndoSystem.AddSingleColorRedoToLastUndo;
begin
  if fPointer>-1 then begin
    with TBDSingleColorUndoItem(Self[fPointer]) do
      AddAfter(Project.CurrentImage.Palette[Start]);
    MessageQueue.AddMessage(fAfterUndoRedoMessage);
  end;
end;

procedure TBDPaletteUndoSystem.SaveToStream(pStream:TStream);
var i:integer;curr:int64;
begin
  i:=UNDOSYSTEMPALETTEID;
  pStream.Write(i,1);
  curr:=pStream.Position;
  i:=0;
  pStream.Write(i,4);

  i:=1;
  pStream.Write(i,1);  // Version
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

procedure TBDPaletteUndoSystem.LoadFromStream(pStream:TStream);
var size,curr:int64;b:byte;count,i:integer;
begin
  b:=0;
  pStream.Read(b,1);
  if b<>UNDOSYSTEMPALETTEID then raise Exception.Create(Format('ID is not for palette undosystem data! (%.2x)',[b]));
  size:=0;
  pStream.Read(Size,4);
  curr:=pStream.Position;
  pStream.Read(b,1);
  if b=1 then begin  // Version 1
    count:=0;
    pStream.Read(count,2);
    fPointer:=0;
    pStream.Read(fPointer,2);
    if fPointer=65535 then fPointer:=-1;
    for i:=0 to Count-1 do begin
      pStream.Read(b,1);
      pStream.Seek(-1,soFromCurrent);
      if b=UNDOOPERATIONPALETTEID then
        Self.Add(TBDPaletteUndoItem.CreateFromStream(pStream))
      else if b=UNDOOPERATIONSINGLECOLORID then
        Self.Add(TBDSingleColorUndoItem.CreateFromStream(pStream))
    end;
  end;
  pStream.Position:=curr+size;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_SETPALETTEUNDOREDOBUTTON,0);
end;

end.

