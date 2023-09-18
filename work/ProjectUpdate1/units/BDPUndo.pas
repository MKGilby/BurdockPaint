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

// This is a stripped version of the original BDPUndo!

unit BDPUndo;

{$mode Delphi}

interface

uses Classes, Sysutils, fgl, BDPRegion, BDPPalette;

type

  { TBDUndoItem }

  TBDUndoItem=class
    constructor Create;
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
    constructor Create(iBefore:TBDRegion);
    constructor CreateFromStream(iStream:TStream);
    destructor Destroy; override;
    procedure AddAfter(iAfter:TBDRegion);
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    fBefore,fAfter:TBDRegion;
    procedure LoadFromStreamV1(pStream:TStream);
  end;

  { TBDPaletteUndoItem }

  TBDPaletteUndoItem=class(TBDUndoItem)
    constructor Create(iStart:integer;iBefore:TBDPalette);
    constructor CreateFromStream(iStream:TStream);
    destructor Destroy; override;
    procedure AddAfter(pAfter:TBDPalette);
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  private
    fStart:integer;
    fBefore,fAfter:TBDPalette;
    procedure LoadFromStreamV1(pStream:TStream);
  public
    property Start:integer read fStart;
    property Before:TBDPalette read fBefore;
    property After:TBDPalette read fAfter;
  end;

  { TBDSingleColorUndoItem }

  TBDSingleColorUndoItem=class(TBDUndoItem)
    constructor Create(iStart:integer;iBefore:uint32);
    constructor CreateFromStream(iStream:TStream);
    procedure AddAfter(pAfter:uint32);
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
    procedure SaveToFile(pFilename:string);
    procedure SaveToStream(pStream:TStream); virtual; abstract;
    procedure LoadFromFile(pFilename:string);
    procedure LoadFromStream(pStream:TStream); virtual; abstract;
    function CanUndo:boolean;
    function CanRedo:boolean;
  private
    fPointer:integer;  // points to the item that will be undoed if requested.
  end;

  { TBDImageUndoSystem }

  TBDImageUndoSystem=class(TBDUndoSystem)
    constructor Create;
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream);  override;
  end;

  { TBDPaletteUndoSystem }

  TBDPaletteUndoSystem=class(TBDUndoSystem)
    constructor Create;
    procedure SaveToStream(pStream:TStream); override;
    procedure LoadFromStream(pStream:TStream); override;
  end;

implementation

uses BDPShared, MKToolbox;

const
  UNDOSYSTEMREGIONID=$55;
  UNDOOPERATIONREGIONID=$4F;
  UNDOSYSTEMPALETTEID=$56;
  UNDOOPERATIONPALETTEID=$51;
  UNDOOPERATIONSINGLECOLORID=$53;

//  SaveCount:integer=0;

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
  fBefore:=TBDRegion.Create(16,16);
  fBefore.LoadRegionFromStream(pStream,GlobalV1Palette);
//  fBefore.WriteFile(st(SaveCount,4,'0')+'_0.png','PNG');
  fAfter:=TBDRegion.Create(16,16);
  fAfter.LoadRegionFromStream(pStream,GlobalV1Palette);
//  fAfter.WriteFile(st(SaveCount,4,'0')+'_1.png','PNG');
//  inc(SaveCount);
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
end;

{ TBDPaletteUndoSystem }

constructor TBDPaletteUndoSystem.Create;
begin
  inherited Create;
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
end;

end.

