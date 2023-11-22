{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPInternalFileFormat;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type

  { TInternalBlock }

  TInternalBlock=class
    constructor Create;
    destructor Destroy; override;
  public
    BlockID:string;
    Version:byte;
    Data:TMemoryStream;
  end;

  { TInternalFileFormat }

  TInternalFileFormat=class
    class procedure WriteBlock(pTarget:TStream;pBlockID:string;pVersion:byte;pData:TStream;pCanCompress:boolean=true); static;
    class function ReadBlock(pSource:TStream):TInternalBlock; static;
  end;

implementation

uses MyZStreamUnit;

const
  COMPRESSIONBIT=$20;

{ TInternalBlock }

constructor TInternalBlock.Create;
begin
  Data:=TMemoryStream.Create;
end;

destructor TInternalBlock.Destroy;
begin
  if Assigned(Data) then Data.Free;
  inherited Destroy;
end;

{ TInternalFileFormat }

class procedure TInternalFileFormat.WriteBlock(pTarget:TStream;
  pBlockID:string; pVersion:byte; pData:TStream; pCanCompress:boolean);
var Xs:TStream;oldpos:int64;i:integer;
begin
  oldpos:=pData.Position;
  pBlockID:=UpperCase(pBlockID);
  if pCanCompress then begin
    Xs:=TMemoryStream.Create;
    try
      pData.Position:=0;
      CompressStream(pData,Xs,pData.Size);
      if Xs.Size<pData.Size then begin
        pBlockID[1]:=chr(ord(pBlockID[1]) or COMPRESSIONBIT);
        pTarget.Write(pBlockID[1],3);
        pTarget.Write(pVersion,1);
        i:=Xs.Size;
        pTarget.Write(i,4);
        Xs.Position:=0;
        pTarget.CopyFrom(Xs,i);
        pData.Position:=oldpos;
        exit;
      end;
    finally
      Xs.Free;
    end;
  end;
  pData.Position:=0;
  pTarget.Write(pBlockID[1],3);
  pTarget.Write(pVersion,1);
  i:=pData.Size;
  pTarget.Write(i,4);
  pTarget.CopyFrom(pData,i);
  pData.Position:=oldpos;
end;

class function TInternalFileFormat.ReadBlock(pSource:TStream):TInternalBlock;
var size,curr:int64;
begin
  Result:=TInternalBlock.Create;
  Result.BlockID:=#0#0#0;
  pSource.Read(Result.BlockID[1],3);
  Result.Version:=0;
  pSource.Read(Result.Version,1);
  size:=0;
  pSource.Read(size,4);
  curr:=pSource.Position;
  if ord(Result.BlockID[1]) and COMPRESSIONBIT=COMPRESSIONBIT then begin
    UnCompressStream(pSource,Result.Data);
    Result.BlockID:=uppercase(Result.BlockID);
  end else
    Result.Data.CopyFrom(pSource,size);
  Result.Data.Position:=0;
  pSource.Position:=curr+size;
end;

end.

