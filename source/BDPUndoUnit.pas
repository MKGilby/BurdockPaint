unit BDPUndoUnit;

{$mode Delphi}

interface

uses classes, fgl, BDPImageUnit;

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

  { TBDUndoImageItem }

  TBDUndoImageItem=class(TBDUndoItem)
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

  TBDUndoList=TFPGObjectList<TBDUndoItem>;

  { TBDUndoSystem }

  TBDUndoSystem=class
    constructor Create;
    destructor Destroy; override;
    procedure AddImageUndo(Left,Top,Width,Height:integer;Image:TBDImage=nil);
    procedure AddImageRedoToLastUndo(Left,Top,Width,Height:integer);
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
    fList:TBDUndoList;
    fPointer:integer;  // points to the item that will be undoed if requested.
  end;

implementation

uses SysUtils, BDPSharedUnit, BDPSettingsUnit;

const
  UNDODATAID=$55;
  UNDOOPERATIONDATAID=$4F;
  UNDOIMAGESUBID=$00;
  UNDOPALETTESUBID=$01;

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

{ TBDUndoImageItem }

constructor TBDUndoImageItem.Create(iBefore:TBDImage);
begin
  inherited Create;
  fBefore:=iBefore;
  fAfter:=nil;
end;

constructor TBDUndoImageItem.CreateFromStream(Source:TStream);
begin
  inherited Create;
  fBefore:=TBDImage.Create(16,16);
  fBefore.LoadFromStream(Source);
  fAfter:=TBDImage.Create(16,16);
  fAfter.LoadFromStream(Source);
  fRedoable:=true;
end;

destructor TBDUndoImageItem.Destroy;
begin
  if Assigned(fBefore) then FreeAndNil(fBefore);
  if Assigned(fAfter) then FreeAndNil(fAfter);
  inherited Destroy;
end;

procedure TBDUndoImageItem.AddAfter(iAfter:TBDImage);
begin
  fAfter:=iAfter;
  fRedoable:=true;
end;

procedure TBDUndoImageItem.Undo;
begin
  MainImage.PutImage(fBefore.Left,fBefore.Top,fBefore);
end;

procedure TBDUndoImageItem.Redo;
begin
  if Assigned(fAfter) then
    MainImage.PutImage(fAfter.Left,fAfter.Top,fAfter);
end;

procedure TBDUndoImageItem.SaveToStream(Target:TStream);
var i,curr:integer;
begin
  if Assigned(fAfter) then begin
    i:=UNDOOPERATIONDATAID;
    Target.Write(i,1);
    curr:=Target.Position;
    i:=0;
    Target.Write(i,4);
    i:=UNDOIMAGESUBID;
    Target.Write(i,1);
    fBefore.SaveToStream(Target);
    fAfter.SaveToStream(Target);
    i:=Target.Position-curr-4;
    Target.Position:=curr;
    Target.write(i,4);
    Target.Position:=Target.Position+i;
  end else
    raise Exception.Create('UndoImageItem save error: No AfterImage assigned!');
end;

{ TBDUndoSystem }

constructor TBDUndoSystem.Create;
begin
  fList:=TBDUndoList.Create;
  fList.FreeObjects:=true;
  fPointer:=-1;
end;

destructor TBDUndoSystem.Destroy;
begin
  if Assigned(fList) then FreeAndNil(fList);
  inherited Destroy;
end;

procedure TBDUndoSystem.AddImageUndo(Left,Top,Width,Height:integer;Image:TBDImage);
var atm:TBDUndoImageItem;atmi:TBDImage;
begin
  if (fPointer<>fList.Count-1) then   // If not the last item, delete items after it.
    fList.DeleteRange(fPointer+1,fList.Count-1);
  if fList.Count=Settings.UndoLimit then fList.Delete(0);
  atmi:=TBDImage.Create(Width,Height);
  atmi.Left:=Left;
  atmi.Top:=Top;
  if Image=nil then
    atmi.PutImagePart(0,0,Left,Top,Width,Height,MainImage)
  else
    atmi.PutImagePart(0,0,Left,Top,Width,Height,Image);
  atm:=TBDUndoImageItem.Create(atmi);
  fList.Add(atm);
  fPointer:=fList.Count-1;
end;

procedure TBDUndoSystem.AddImageRedoToLastUndo(Left,Top,Width,Height:integer);
var atmi:TBDImage;
begin
  if fPointer>-1 then begin
    if not fList[fPointer].Redoable then begin
      atmi:=TBDImage.Create(Width,Height);
      atmi.Left:=Left;
      atmi.Top:=Top;
      atmi.PutImagePart(0,0,Left,Top,Width,Height,MainImage);
      TBDUndoImageItem(fList[fPointer]).AddAfter(atmi);
      MessageQueue.AddMessage(MSG_SETUNDOREDOBUTTON);
    end;
  end;
end;

procedure TBDUndoSystem.Undo;
begin
  if (fPointer>=0) and (fPointer<fList.Count) then begin
    fList[fPointer].Undo;
    dec(fPointer);  // It can go below 0, -1 shows that no more undoable task remains.
    MessageQueue.AddMessage(MSG_SETUNDOREDOBUTTON);
  end;
end;

procedure TBDUndoSystem.Redo;
begin
  if (fPointer+1>=0) and (fPointer+1<fList.Count) then begin
    if fList[fPointer+1].Redoable then begin
      inc(fPointer);
      fList[fPointer].Redo;
      MessageQueue.AddMessage(MSG_SETUNDOREDOBUTTON);
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
  i:=fList.Count;
  Target.Write(i,2);
  Target.Write(fPointer,2);
  for i:=0 to fList.Count-1 do
    fList[i].SaveToStream(Target);
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
  if b=UNDOIMAGESUBID then fList.Add(TBDUndoImageItem.CreateFromStream(Source))
  else if b=UNDOPALETTESUBID then // not yet;
  Source.Position:=curr+size;
end;

function TBDUndoSystem.CanUndo: boolean;
begin
  Result:=fPointer>-1;
end;

function TBDUndoSystem.CanRedo: boolean;
begin
  Result:=fPointer<fList.Count-1;
end;

end.

