unit BDPUndoUnit;

{$mode Delphi}

interface

uses fgl;

type
  TBDUndoItemType=(uitImage,uitPalette);

  { TBDUndoItem }

  TBDUndoItem=class
    constructor Create(iItemType:TBDUndoItemType;iEntity:TObject);
    destructor Destroy; override;
    procedure AddRedo(iEntity:TObject);
    procedure Undo;
    procedure Redo;
  private
    fItemType:TBDUndoItemType;
    fUndoEntity,fRedoEntity:TObject;
    function fGetRedoable:boolean;
  public
    property Redoable:boolean read fGetRedoable;
  end;

  TBDUndoList=TFPGObjectList<TBDUndoItem>;

  { TBDUndoSystem }

  TBDUndoSystem=class
    constructor Create;
    destructor Destroy; override;
    procedure AddImageUndo(Left,Top,Width,Height:integer);
    procedure AddImageRedoToLastUndo(Left,Top,Width,Height:integer);
    procedure Undo;
    procedure Redo;
  private
    fList:TBDUndoList;
    fPointer:integer;  // points to the item that will be undoed if requested.
  end;

implementation

uses SysUtils, BDPImageUnit, BDPSharedUnit;

{ TBDUndoItem }

constructor TBDUndoItem.Create(iItemType:TBDUndoItemType; iEntity:TObject);
begin
  fItemType:=iItemType;
  fUndoEntity:=iEntity;
  fRedoEntity:=nil;
end;

destructor TBDUndoItem.Destroy;
begin
  if Assigned(fUndoEntity) then FreeAndNil(fUndoEntity);
  if Assigned(fRedoEntity) then FreeAndNil(fRedoEntity);
  inherited Destroy;
end;

procedure TBDUndoItem.AddRedo(iEntity:TObject);
begin
  fRedoEntity:=iEntity;
end;

procedure TBDUndoItem.Undo;
begin
  if fItemType=uitImage then
    MainImage.PutImage(TBDImage(fUndoEntity).Left,TBDImage(fUndoEntity).Top,TBDImage(fUndoEntity));
end;

procedure TBDUndoItem.Redo;
begin
  if fItemType=uitImage then
    MainImage.PutImage(TBDImage(fRedoEntity).Left,TBDImage(fRedoEntity).Top,TBDImage(fRedoEntity));
end;

function TBDUndoItem.fGetRedoable:boolean;
begin
  Result:=Assigned(fRedoEntity);
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

procedure TBDUndoSystem.AddImageUndo(Left,Top,Width,Height:integer);
var atm:TBDUndoItem;atmi:TBDImage;i:integer;
begin
  if (fPointer<>fList.Count-1) then   // If not the last item, delete items after it.
    fList.DeleteRange(fPointer+1,fList.Count-1);
  atmi:=TBDImage.Create(Width,Height);
  atmi.Left:=Left;
  atmi.Top:=Top;
  atmi.PutImagePart(0,0,Left,Top,Width,Height,MainImage);
  atm:=TBDUndoItem.Create(uitImage,atmi);
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
      fList[fPointer].AddRedo(atmi);
    end;
  end;
end;

procedure TBDUndoSystem.Undo;
begin
  if (fPointer>=0) and (fPointer<fList.Count) then begin
    fList[fPointer].Undo;
    dec(fPointer);  // It can go below 0, -1 shows that no more undoable task remains.
  end;
end;

procedure TBDUndoSystem.Redo;
begin
  if (fPointer+1>=0) and (fPointer+1<fList.Count) then begin
    if fList[fPointer+1].Redoable then begin
      inc(fPointer);
      fList[fPointer].Redo;
    end;
  end;
end;

end.

