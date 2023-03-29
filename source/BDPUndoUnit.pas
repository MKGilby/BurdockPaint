unit BDPUndoUnit;

{$mode Delphi}

interface

uses classes, fgl, BDPImageUnit, BDPPaletteUnit, BDPMessageUnit;

type

  { TBDUndoItem }

  TBDUndoItem=class
    constructor Create;
    procedure Undo; virtual; abstract;
    procedure Redo; virtual; abstract;
    procedure SaveToFile(Filename:string);
    procedure SaveToStream(Target:TStream); virtual; abstract;
  private
    fRedoable:boolean;
  public
    property Redoable:boolean read fRedoable;
  end;

  { TBDUndoRegionItem }

  TBDUndoRegionItem=class(TBDUndoItem)
    constructor Create(iBefore:TBDImage);
    constructor CreateFromStream(Source:TStream);
    destructor Destroy; override;
    procedure AddAfter(iAfter:TBDImage);
    procedure Undo; override;
    procedure Redo; override;
    procedure SaveToStream(Target:TStream); override;
  private
    fBefore,fAfter:TBDImage;
  end;

  { TBDUndoColorItem }

  TBDUndoColorItem=class(TBDUndoItem)
    constructor Create(iStart:integer;iBefore:TBDPalette);
    constructor CreateFromStream(Source:TStream);
    destructor Destroy; override;
    procedure AddAfter(iAfter:TBDPalette);
    procedure Undo; override;
    procedure Redo; override;
    procedure SaveToStream(Target:TStream); override;
  private
    fStart:integer;
    fBefore,fAfter:TBDPalette;
  end;

  TBDUndoList=TFPGObjectList<TBDUndoItem>;

  { TBDUndoSystem }

  TBDUndoSystem=class(TBDUndoList)
    constructor Create;
    destructor Destroy; override;
    procedure Undo;
    procedure Redo;
    procedure SaveToFile(Filename:string);
    procedure SaveToStream(Target:TStream);
    procedure LoadFromFile(Filename:string);
    procedure LoadFromStream(Source:TStream);
    procedure LoadItemFromStream(Source:TStream);
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
  end;

  { TBDPaletteUndoSystem }

  TBDPaletteUndoSystem=class(TBDUndoSystem)
    constructor Create;
    procedure AddPaletteUndo(Start,Count:integer);
    procedure AddPaletteRedoToLastUndo(Start,Count:integer);
  end;

implementation

uses SysUtils, BDPSharedUnit, BDPSettingsUnit;

const
  UNDODATAID=$55;
  UNDOOPERATIONDATAID=$4F;
  UNDOREGIONSUBID=$00;
  UNDOCOLORSUBID=$01;

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

{ TBDUndoRegionItem }

constructor TBDUndoRegionItem.Create(iBefore:TBDImage);
begin
  inherited Create;
  fBefore:=iBefore;
  fAfter:=nil;
end;

constructor TBDUndoRegionItem.CreateFromStream(Source:TStream);
begin
  inherited Create;
  fBefore:=TBDImage.Create(16,16);
  fBefore.Palette.CopyColorsFrom(MainImage.Palette);
  fBefore.LoadWholeImageDataFromStream(Source);
  fAfter:=TBDImage.Create(16,16);
  fAfter.Palette.CopyColorsFrom(MainImage.Palette);
  fAfter.LoadWholeImageDataFromStream(Source);
  fRedoable:=true;
end;

destructor TBDUndoRegionItem.Destroy;
begin
  if Assigned(fBefore) then FreeAndNil(fBefore);
  if Assigned(fAfter) then FreeAndNil(fAfter);
  inherited Destroy;
end;

procedure TBDUndoRegionItem.AddAfter(iAfter:TBDImage);
begin
  fAfter:=iAfter;
  fRedoable:=true;
end;

procedure TBDUndoRegionItem.Undo;
begin
  MainImage.PutImage(fBefore.Left,fBefore.Top,fBefore);
end;

procedure TBDUndoRegionItem.Redo;
begin
  if Assigned(fAfter) then
    MainImage.PutImage(fAfter.Left,fAfter.Top,fAfter);
end;

procedure TBDUndoRegionItem.SaveToStream(Target:TStream);
var i,curr:integer;
begin
  if Assigned(fAfter) then begin
    i:=UNDOOPERATIONDATAID;
    Target.Write(i,1);
    curr:=Target.Position;
    i:=0;
    Target.Write(i,4);
    i:=UNDOREGIONSUBID;
    Target.Write(i,1);
    fBefore.SaveWholeImageDataToStream(Target);
    fAfter.SaveWholeImageDataToStream(Target);
    i:=Target.Position-curr-4;
    Target.Position:=curr;
    Target.write(i,4);
    Target.Position:=Target.Position+i;
  end else
    raise Exception.Create('UndoImageItem save error: No AfterImage assigned!');
end;

{ TBDUndoColorItem }

constructor TBDUndoColorItem.Create(iStart:integer; iBefore:TBDPalette);
begin
  fStart:=iStart;
  fBefore:=iBefore;
end;

constructor TBDUndoColorItem.CreateFromStream(Source:TStream);
begin
  inherited Create;
  fStart:=0;
  Source.Read(fStart,2);
  fBefore:=TBDPalette.Create;
  fBefore.LoadFromStream(Source);
  fAfter:=TBDPalette.Create;
  fAfter.LoadFromStream(Source);
  fRedoable:=true;
end;

destructor TBDUndoColorItem.Destroy;
begin
  if Assigned(fBefore) then FreeAndNil(fBefore);
  if Assigned(fAfter) then FreeAndNil(fAfter);
  inherited Destroy;
end;

procedure TBDUndoColorItem.AddAfter(iAfter:TBDPalette);
begin
  fAfter:=iAfter;
end;

procedure TBDUndoColorItem.Undo;
begin
  MainImage.Palette.CopyColorsFrom(fBefore,fStart,fBefore.Size);
end;

procedure TBDUndoColorItem.Redo;
begin
  if Assigned(fAfter) then
    MainImage.Palette.CopyColorsFrom(fAfter,fStart,fAfter.Size);
end;

procedure TBDUndoColorItem.SaveToStream(Target:TStream);
var i:integer;curr:int64;
begin
  if Assigned(fAfter) then begin
    i:=UNDOOPERATIONDATAID;
    Target.Write(i,1);
    curr:=Target.Position;
    i:=0;
    Target.Write(i,4);
    i:=UNDOCOLORSUBID;
    Target.Write(i,1);
    Target.Write(fStart,2);
    fBefore.SaveToStream(Target);
    fAfter.SaveToStream(Target);
    i:=Target.Position-curr-4;
    Target.Position:=curr;
    Target.write(i,4);
    Target.Position:=Target.Position+i;
  end else
    raise Exception.Create('UndoColorItem save error: No AfterPalette assigned!');
end;

{ TBDUndoSystem }

constructor TBDUndoSystem.Create;
begin
  inherited Create;
  FreeObjects:=true;
  fPointer:=-1;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_NONE,0);
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

procedure TBDUndoSystem.SaveToFile(Filename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(Filename,fmCreate);
  SaveToStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDUndoSystem.SaveToStream(Target:TStream);
var i,curr:integer;
begin
  i:=UNDODATAID;
  Target.Write(i,1);
  curr:=Target.Position;
  i:=0;
  Target.Write(i,4);
  i:=Self.Count;
  Target.Write(i,2);
  Target.Write(fPointer,2);
  for i:=0 to Self.Count-1 do
    Self[i].SaveToStream(Target);
  i:=Target.Position-curr-4;
  Target.Position:=curr;
  Target.write(i,4);
  Target.Position:=Target.Position+i;
end;

procedure TBDUndoSystem.LoadFromFile(Filename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(Filename,fmOpenRead or fmShareDenyNone);
  LoadFromStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDUndoSystem.LoadFromStream(Source:TStream);
var size,curr:int64;b:byte;count,i:integer;
begin
  b:=0;
  Source.Read(b,1);
  if b<>UNDODATAID then raise Exception.Create(Format('ID is not for undosystem data! (%.2x)',[b]));
  size:=0;
  Source.Read(Size,4);
  curr:=Source.Position;
  count:=0;
  Source.Read(count,2);
  fPointer:=0;
  Source.Read(fPointer,2);
  if fPointer=65535 then fPointer:=-1;
  for i:=0 to Count-1 do
    LoadItemFromStream(Source);
  Source.Position:=curr+size;
end;

procedure TBDUndoSystem.LoadItemFromStream(Source:TStream);
var size,curr:int64;b:byte;
begin
  b:=0;
  Source.Read(b,1);
  if b<>UNDOOPERATIONDATAID then raise Exception.Create(Format('ID is not for undo operation data! (%.2x)',[b]));
  size:=0;
  Source.Read(Size,4);
  curr:=Source.Position;
  Source.Read(b,1);
  if b=UNDOREGIONSUBID then Self.Add(TBDUndoRegionItem.CreateFromStream(Source))
  else if b=UNDOCOLORSUBID then Self.Add(TBDUndoColorItem.CreateFromStream(Source));
  Source.Position:=curr+size;
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
  fAfterUndoRedoMessage:=TMessage.Init(MSG_SETUNDOREDOBUTTON,0);
end;

procedure TBDImageUndoSystem.AddImageUndo(Left,Top,Width,Height:integer;Image:TBDImage);
var atm:TBDUndoRegionItem;atmi:TBDImage;
begin
  if (fPointer<>Self.Count-1) then   // If not the last item, delete items after it.
    Self.DeleteRange(fPointer+1,Self.Count-1);
  if Self.Count=Settings.UndoLimit then Self.Delete(0);
  atmi:=TBDImage.Create(Width,Height);
  atmi.Left:=Left;
  atmi.Top:=Top;
  if Image=nil then
    atmi.PutImagePart(0,0,Left,Top,Width,Height,MainImage)
  else
    atmi.PutImagePart(0,0,Left,Top,Width,Height,Image);
  atm:=TBDUndoRegionItem.Create(atmi);
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
      atmi.PutImagePart(0,0,Left,Top,Width,Height,MainImage);
      TBDUndoRegionItem(Self[fPointer]).AddAfter(atmi);
      MessageQueue.AddMessage(fAfterUndoRedoMessage);
    end;
  end;
end;

{ TBDPaletteUndoSystem }

constructor TBDPaletteUndoSystem.Create;
begin
  inherited Create;
  fAfterUndoRedoMessage:=TMessage.Init(MSG_SETUNDOREDOBUTTON,0);
end;

procedure TBDPaletteUndoSystem.AddPaletteUndo(Start,Count:integer);
var atm:TBDUndoColorItem;atmP:TBDPalette;
begin
  if (fPointer<>Self.Count-1) then   // If not the last item, delete items after it.
    Self.DeleteRange(fPointer+1,Self.Count-1);
  if Self.Count=Settings.UndoLimit then Self.Delete(0);
  atmP:=TBDPalette.Create(Count);
  atmP.CopyColorsFrom(MainImage.Palette,Start,Count);
  atm:=TBDUndoColorItem.Create(Start,atmP);
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
      atmP.CopyColorsFrom(MainImage.Palette,Start,Count);
      TBDUndoColorItem(Self[fPointer]).AddAfter(atmP);
      MessageQueue.AddMessage(fAfterUndoRedoMessage);
    end;
  end;
end;

end.

