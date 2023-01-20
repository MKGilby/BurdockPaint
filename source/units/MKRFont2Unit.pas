{ -[Name]-------------------------------------------

               TMKRFont class for SDL2

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2020

  --------------------------------------------------

  -[Description]------------------------------------

   * MKRFont:

   File-format specification for MKAR (.MKR) files

  Ofs    Len   Abbr  Description
   0      4     ID   'MKAR' - FourCC
   4      1     FL   Flags: 0. - y g q should be drawn at y+1...
                            1. - Char map exists after HE (Char map is 32 bytes,
                                 contains one bit for each char. If bit is set
                                 the char data exists in data stream.)
   5      1     WI   Width of chars (in pixel)
   6      1     HE   Height of chars
   7      (1+(WI-1) div 8)*HE*256 bytes of charactes definitions

   One char length in bytes =(1+(WI-1) div 8)*HE.

   Format (example: Letter A in 12*16 pixels):

     This is only filling to 8 bits. It can be anything.
      |
      |  Important bits!
      |  ||||||||||||
     +--++----------+
     0000000011110000  $00 $F0
     0000001111111100  $03 $FC
     0000011111111110  $07 $FE
     0000011110011110  $07 $9E
     0000111100001111  $0F $0F
     0000111000000111  $0E $07
     0000111000000111  $0E $07
     0000111111111111  $0F $FF
     0000111111111111  $0F $FF
     0000111111111111  $0F $FF
     0000111000000111  $0E $07
     0000111000000111  $0E $07
     0000111000000111  $0E $07
     0000111000000111  $0E $07
     0000111000000111  $0E $07
     0000111000000111  $0E $07

  -------------------------------------------------- }

// Version info:
//   V1.00 - Gilby - 2020.02.07-09
//      - Initial creation from MKRFontUnit
//      + Added one pixel padding between chars to avoid artifacts with
//        fullscreen scaling
//      + Char 'p' is also shifted down one pixel if the flag set
//   V1.00a - Gilby - 2021.03.15
//      * Changes to suppress hint in Lazarus
//   V1.01 - Gilby - 2021.08.24
//      * Following changes in Font2Unit

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit MKRFont2Unit;

interface

uses Classes, Font2Unit;

type
  TMKRFont=class(TFont)
    constructor Create(Source:TStream); overload;
    constructor Create(filename:string); overload;
  end;

implementation

uses Logger, SysUtils, MKStream, ARGBImageUnit, MKToolbox, mk_sdl2;

const Fstr='MKRFont2Unit.pas, ';
      Version='1.01';

// ------------------------------------------------------------ [ TMKRFont ]---

constructor TMKRFont.Create(Source:TStream);
const
  ad2:array[0..7] of integer=(128,64,32,16,8,4,2,1);
  flSlideDown_gqy=1;
  flHasCharTable=2;
//const Istr=Fstr+'TMKRFont.Create';

var s:String;chars:array[0..31] of byte;
    i,x,y:integer;
    b:integer;
    Flags,CharWidth,CharHeight,CharCount,CurrentChar,Slide:integer;
    pitch:integer;
//    buf:pointer;bufsize:integer;
begin
  inherited Create;
  Flags:=0;CharWidth:=0;CharHeight:=0;chars[0]:=0;

  s:=#0#0#0#0;
  Source.Read(s[1],4);
  if s='MKAR' then begin
    Source.Read(Flags,1);
    Source.Read(CharWidth,1);
    Source.Read(CharHeight,1);
    fHeight:=CharHeight;
    if Flags and flSlideDown_gqy<>0 then
      inc(FHeight);  // this flag means that g q y and p goes one pixel down
                     // so the real height is increased by one pixel

    if Flags and flHasCharTable<>0 then begin  // Has char table, so count chars
      Source.Read(chars[0],32);
      CharCount:=0;
      for i:=0 to 31 do CharCount+=CountBitsInByte(chars[i]);
    end else begin  // No char table, every char is there.
      CharCount:=256;
    end;

    pitch:=CharCount*(CharWidth+1)-1;
    fOriginalImage:=TARGBImage.Create(pitch,fHeight);
    fOriginalImage.Bar(0,0,pitch,fHeight,0,0,0,255);
//    fGSDData:=TMemoryStream.Create;
//    fGSDData.Write(pitch,2);
//    fGSDData.Write(fHeight,2);

    // Buffer to hold grayscale data
//    bufsize:=pitch*fHeight;
//    getmem(buf,bufsize);
//    fillchar(buf^,bufsize,0);

    // And now create gsd from each char that exists.
    // We have to create the coords info too.
    CurrentChar:=0;b:=0;
    for i:=0 to 255 do begin
      if (CharCount=256) or
         ((CharCount<256) and (chars[i div 8] and ad2[i mod 8]<>0)) then begin  // Character exists in data file

        fOrigDefs[i].Left:=CurrentChar*(CharWidth+1);
        fOrigDefs[i].Top:=0;
        fOrigDefs[i].Width:=CharWidth;
        fOrigDefs[i].Height:=fHeight;
        fDefs[i].Left:=fOrigDefs[i].Left;
        fDefs[i].Top:=fOrigDefs[i].Top;
        fDefs[i].Width:=fOrigDefs[i].Width;
        fDefs[i].Height:=fOrigDefs[i].Height;
        if (Flags and flSlideDown_gqy<>0) and (i in [121,112,113,103]) then Slide:=1 else Slide:=0;
        for y:=0 to CharHeight-1 do begin
          for x:=0 to CharWidth-1 do begin
            if x mod 8=0 then begin
              Source.Read(b,1);
              if (CharWidth mod 8<>0) and (x=0) then b:=b<<(8-(CharWidth mod 8));  // Skip filling bits
            end;
            if b and 128<>0 then
              fOriginalImage.PutPixel(CurrentChar*(CharWidth+1)+x,y+Slide,255,255,255,255);
//              byte((buf+CurrentChar*(CharWidth+1)+(y+Slide)*pitch+x)^):=255;
//              byte((buf+CurrentChar*(CharWidth)+(y+Slide)*CharCount*CharWidth+x)^):=255;
            b:=(b<<1) and $ff;
          end;
        end;
        inc(CurrentChar);
      end;
    end;
//    fGSDData.Write(buf^,bufsize);
//    freemem(buf,bufsize);
  end;
//  TMemoryStream(fGSDData).SaveToFile('xxx.txt');
//  fTexture:=TStaticTexture.Create(16,16);
  SetColor(255,255,255);
//  putimage(0,32,fBitmap);
  LetterSpace:=0;
  SpaceSpace:=CharWidth;
//  Log.LogDebug('Finish',Istr);
//  FreeAndNil(fOrigKar);
end;

constructor TMKRFont.Create(FileName:string);
var Xs:TStream;
begin
  Xs:=MKStreamOpener.OpenStream(Filename);
  if Xs<>nil then begin
    Create(Xs);
    FreeAndNil(Xs);
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
