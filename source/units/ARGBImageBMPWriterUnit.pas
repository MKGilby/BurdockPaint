// MKSZTSZ BMP writer for TARGBImage
// ------------------------------------------------------------------
// You can freely distribute the sources under the GNU GPL Version 3.
//
// Written by Gilby/MKSZTSZ
// Hungary, 2023
// ------------------------------------------------------------------

// Version info:
//   1.00 - Gilby - 2023.10.26
//     * Initial creation from RawPictureBMPUnit

unit ARGBImageBMPWriterUnit;

{$mode Delphi}

interface

implementation

uses Classes, SysUtils, ARGBImageUnit, Logger,
    AnimationDataUnit, FontDataUnit;

const
  Fstr={$I %FILE%}+', ';
  Version='1.00';

procedure WriteBMP(pTarget:TStream;pWidth,pHeight:integer;pRawData:pointer;pAnimations:TAnimationDatas;pFontData:TFontData);
const Istr=Fstr+'WriteBMP';
var s:string;l,i,j:integer;pp:pointer;
begin
  if (pWidth<1) or (pHeight<1) then raise Exception.Create('Empty picture!');
  pp:=pRawdata;
  s:='BM'#0#0#0#0#0#0#0#0#54#0#0#0#40#0#0#0;
  pTarget.Write(s[1],length(s));
  pTarget.Write(pWidth,4);
  pTarget.Write(pHeight,4);
  s:=#1#0#24#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
  pTarget.Write(s[1],length(s));
  l:=0;
  for i:=pHeight-1 downto 0 do begin
    for j:=0 to pWidth-1 do
      pTarget.Write((pp+j*4+i*pWidth*4)^,3);
    if pWidth*3 mod 4<>0 then pTarget.Write(l,4-(pWidth*3 mod 4));
  end;
  if pAnimations.Count>0 then Log.LogWarning('Can''t write animation data into .BMP file!',Istr);
  if assigned(pFontData) then Log.LogWarning('Can''t write font data into .BMP file!',Istr);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  RegisterARGBImageWriter('BMP',@WriteBMP);

end.

