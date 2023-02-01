// MKSZTSZ CEL reader for TARGBImage
// ------------------------------------------------------------------
// You can freely distribute the sources under the GNU GPL Version 2.
//
// Written by Gilby/MKSZTSZ
// Hungary, 2020
// ------------------------------------------------------------------

// Version info:
//   1.00 - Gilby - 2020.03.11
//     * Initial creation from ARGBImageCELUnit
//   1.01 - Gilby - 2020.03.17-30
//     * Following changes in ARGBImageUnit
//   1.02 - Gilby - 2020.04.01
//     * Following changes in ARGBImageUnit (TAnimationData -> TAnimationDatas)
//   1.03 - Gilby - 2021.11.09
//     * Fixed a memory leak (pal was not freed)

unit ARGBImageCELReaderUnit;

{$mode delphi}

interface

implementation

uses Classes, SysUtils, ARGBImageUnit, Logger, AnimationDataUnit, FontDataUnit;

const
  Fstr={$I %FILE%}+', ';
  Version='1.03';

procedure ReadCEL(pSource:TStream;out Width,Height:integer;out RawData:pointer;{%H-}Animations:TAnimationDatas;out FontData:TFontData);
const Istr=Fstr+'ReadCEL';
var wi,he:word;
    i,j:longint;
    atm:byte;
    pal:pointer;
    pp:pointer;
begin
  wi:=0;
  pSource.Read(wi,2);   // Image Identifier Field
  if wi<>$9119 then begin
    Log.LogWarning('Not a .CEL file!',Istr);
    exit;
  end;
  he:=0;
  pSource.Read(wi,2);
  pSource.Read(he,2);
  pSource.Seek(32,soFromBeginning);
  getmem(pal,1024);
  for i:=0 to 255 do begin
    pSource.Read((pal+i*4)^,3);
    atm:=byte((pal+i*4+0)^);
    byte((pal+i*4+0)^):=byte((pal+i*4+2)^)*4;
    byte((pal+i*4+2)^):=atm*4;
    byte((pal+i*4+1)^):=byte((pal+i*4+1)^)*4;
    byte((pal+i*4+3)^):=255;
  end;

  Width:=wi;
  Height:=he;

  getmem(Rawdata,wi*he*4);
  pp:=Rawdata;

  for j:=0 to he-1 do
    for i:=0 to wi-1 do begin
      pSource.Read(atm,1);
      move((pal+atm*4)^,(pp+(i+j*wi)*4)^,4);
    end;

  FontData:=nil;
  freemem(pal);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  RegisterARGBImageReader('CEL',@ReadCEL,true);

end.

