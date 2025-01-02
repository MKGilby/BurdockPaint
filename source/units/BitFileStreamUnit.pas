unit BitFileStreamUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  // The new byte comes from this direction:
  //   Left (with bit size of 5):  used in GIFs
  //  11100000 32222211 44443333 66555554 77777666
  //   Right (with bit size of 5):
  //  00000111 11222223 33334444 45555566 66677777
  TBitFileStreamDirection=(bdLeft,bdRight);

  { TBitFileStream }

  TBitFileStream=class
    constructor Create(iSource:TStream;iDirection:TBitFileStreamDirection);
    function GetBits(pBitCount:integer):integer;
  private
    fDirection:TBitFileStreamDirection;
    fStream:TStream;
    fUsedBits:integer;
    fCurrentByte:byte;
  end;

implementation

uses Logger;

{ TBitFileStream }

constructor TBitFileStream.Create(iSource: TStream;
  iDirection: TBitFileStreamDirection);
begin
  fUsedBits:=8;
  fDirection:=iDirection;
  fStream:=iSource;
end;

function TBitFileStream.GetBits(pBitCount:integer):integer;
const LeftMasks:array[1..8] of byte=($01,$03,$07,$0f,$1f,$3f,$7f,$ff);
      RightMasks:array[1..8] of byte=($80,$c0,$e0,$f0,$f8,$fc,$fe,$ff);
var i,neededbits,servedbits:integer;

  procedure ReadByte;
  begin
    fStream.Read(fCurrentByte,1);
    fUsedBits:=0;
  end;

begin
  i:=pBitCount;
  Result:=0;
  servedbits:=0;
  if fDirection=bdLeft then begin
    while pBitCount>0 do begin
      if fUsedBits=8 then ReadByte;
      if pBitCount<8-fUsedBits then
        neededbits:=pBitCount
      else
        neededbits:=8-fUsedBits;
      Result:=Result or ((fCurrentByte and LeftMasks[neededbits])<<(servedbits));
      fCurrentByte:=fCurrentByte and (LeftMasks[neededbits] xor 255)>>neededbits;
      servedbits+=neededbits;
      fUsedBits+=neededbits;
      pBitCount-=neededbits;
    end;
  end;
//  Log.LogDebug(Format('GetBit(%d)=%d',[i,Result]));
end;

end.

