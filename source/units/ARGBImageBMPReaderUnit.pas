// MKSZTSZ BMP reader for TARGBImage
// ------------------------------------------------------------------
// You can freely distribute the sources under the GNU GPL Version 2.
//
// Written by Gilby/MKSZTSZ
// Hungary, 2021
// ------------------------------------------------------------------

// Version info:
//   1.00 - Gilby - 2021.05.28
//     * Initial creation from RawPictureBMPUnit

unit ARGBImageBMPReaderUnit;

{$mode delphi}
{$smartlink on}

interface

implementation

uses Classes, SysUtils, Logger, AnimationDataUnit, FontDataUnit, ARGBImageUnit;

const
  Fstr={$I %FILE%}+', ';
  Version='1.00';

procedure ReadBMP(pSource:TStream;out Width,Height:integer;out RawData:pointer;{%H-}Animations:TAnimationDatas;out FontData:TFontData);
//procedure ReadBMP(pSource:TStream;var Width,Height:integer;var RawData:pointer;var Extra:TExtra);
var
  bfType:word;
  bfsize:longint;
  bfOffBits:longint;

  biSize:integer;
  biWidth,biHeight:integer;
  biPlanes:integer;
  biBitCount:integer;
  biCompression:integer;
  biImageSize:integer;
  biHorzPPM:integer;
  biVertPPM:integer;
  biPalCount:integer;
  biImportantColours:integer;

//  bmppalentry:array[0..3] of byte;
  scanline:integer;
  buffer:pointer;
  pal:pointer;
  pp:pointer;

  i,j,k:integer;
  b1,b2:byte;

  procedure LoadPal(HowMany:integer);
  var i:integer;
  begin
    pSource.Seek(14+biSize,soFromBeginning);
    for i:=0 to HowMany-1 do begin
{      pSource.Read(bmpPalEntry,4);
      pal[i,1]:=bmpPalEntry[0];
      pal[i,2]:=bmpPalEntry[1];
      pal[i,3]:=bmpPalEntry[2];
      pal[i,4]:=255;}
      pSource.Read((pal+i*4)^,4);
      byte((pal+i*4+3)^):=255;
    end;
  end;

  function GetBit(b,poz:byte):byte;
  begin
    GetBit:=(b and (128>>poz))>>(7-poz);
  end;

  function GetQuad(b,poz:byte):byte;
  begin
    GetQuad:=(b and ($f0>>poz))>>(1-poz);
  end;

  procedure PutPixelPal(x,y:longint;entry:integer);
  begin
    move((pal+entry*4)^,(pp+(y*biWidth+x)*4)^,4);
  end;

  procedure AddPixel(var x,y:integer;entry:integer);
  begin
    move((pal+entry*4)^,(pp+(y*biWidth+x)*4)^,4);
    inc(x);
  end;

begin
  bfType:=0;
  pSource.Read(bfType,2);
  if bfType<>19778 then raise Exception.Create('Not a .BMP file!');
  bfSize:=0;
  pSource.Read(bfSize,4);
  bfOffBits:=0;
  pSource.Read(bfOffBits,4);  // Read 2 reserved words
  pSource.Read(bfOffBits,4);

  biSize:=0;
  pSource.Read(biSize,4);
  if biSize<>40 then raise Exception.Create('Only Windows V3 BMP header supported!');
  biWidth:=0;biHeight:=0;
  pSource.Read(biWidth,4);
  pSource.Read(biHeight,4);
//  Log.Trace(biWidth);
//  Log.Trace(biHeight);
  biPlanes:=0;biBitCount:=0;biCompression:=0;biImageSize:=0;
  biHorzPPM:=0;biVertPPM:=0;biPalCount:=0;biImportantColours:=0;
  pSource.Read(biPlanes,2);
  pSource.Read(biBitCount,2);
//  Log.Trace('BitCount: '+inttostr(biBitCount));
  pSource.Read(biCompression,4);
//  Log.Trace('Compression: '+inttostr(biCompression));
  pSource.Read(biImageSize,4);
  pSource.Read(biHorzPPM,4);
  pSource.Read(biVertPPM,4);
  pSource.Read(biPalCount,4);
  pSource.Read(biImportantColours,4);

  Width:=biWidth;
  Height:=biHeight;
//  Log.Trace('BitCount: '+inttostr(biBitCount));
//  Log.Trace('PalCount: '+inttostr(biPalCount));

  getmem(Rawdata,Width*Height*4);
  pp:=Rawdata;
  b1:=0;b2:=0;
  buffer:=getmem(16384);
  case biBitCount of
    1:begin  // 2 color
        pal:=GetMem(1024);
        LoadPal(2);
        scanline:=(biWidth-1)>>3+1;
//        Log.Trace('Scanline='+inttostr(scanline));
        while scanline mod 4>0 do inc(scanline);
//        Log.Trace('Scanline='+inttostr(scanline));
        for j:=biHeight-1 downto 0 do begin
//          Log.Trace('Line='+inttostr(j));
          pSource.Read(buffer^,scanline);
          for i:=0 to biWidth-1 do begin
            PutPixelPal(i,j,GetBit(byte((buffer+i div 8)^),i mod 8));
          end;
        end;
        Freemem(pal);
      end;
    4:begin  // 16 color
        pal:=GetMem(1024);
        LoadPal(16);
        scanline:=(biWidth-1)>>1+1;
        while scanline mod 4>0 do inc(scanline);
        for j:=biHeight-1 downto 0 do begin
          pSource.Read(buffer^,scanline);
          pp:=RawData+(j*biWidth*4);
          for i:=0 to (biWidth>>1)-1 do begin
            move({%H-}(pal+(byte((buffer+i)^) and $f0)>>2)^,pp^,4);
            pp+=4;
            move((pal+(byte((buffer+i)^) and $0f)<<2)^,pp^,4);
            pp+=4;
          end;
        end;
        Freemem(pal);
      end;
    8:begin  // 256 color
        pal:=GetMem(1024);
        if biPalCount=0 then biPalCount:=256;
        LoadPal(biPalCount);
//        Log.Trace('FilePos after LoadPal: '+inttostr(iSource.Position));
        case biCompression of
          0:begin  // Uncompressed
              scanline:=biWidth;
              while scanline mod 4>0 do inc(scanline);
              for j:=biHeight-1 downto 0 do begin
                pp:=RawData+(j*biWidth*4);
                pSource.Read(buffer^,scanline);
                for i:=0 to biWidth-1 do begin
                  move((pal+(byte((buffer+i)^)*4))^,pp^,4);
                  pp+=4;
                end;
              end;
            end;
          1:begin  // RLE-compressed
              i:=0;j:=biHeight-1;
              repeat
                pSource.Read(b1,1);
                pSource.Read(b2,1);
//                Log.Trace(inttostr(b1)+', '+inttostr(b2));
                if b1>0 then begin
                  while b1>0 do begin
                    AddPixel(i,j,b2);
                    dec(b1);
                  end;
                end else begin
                  case b2 of
                    0:begin
                        i:=0;dec(j);
                      end;
                    1:begin
                        break;
                      end;
                    2:begin
                        pSource.Read(b1,1);
                        i:=i+b1;
                        pSource.Read(b2,1);
                        j:=j+b2;
                      end;
                    else begin
                      for k:=b2-1 downto 0 do begin
                        pSource.Read(b1,1);
                        AddPixel(i,j,b1);
                      end;
                      if b2 mod 2=1 then
                        pSource.Read(b1,1);
                    end;
                  end;
                end;
              until false;
            end;
        end;
        Freemem(pal);
      end;
   24:begin  // 24bit color
        scanline:=biWidth*3;
        pSource.Seek(bfOffBits,soFromBeginning);
        while scanline mod 4>0 do inc(scanline);
        for j:=biHeight-1 downto 0 do begin
          pSource.Read(buffer^,scanline);
          for i:=0 to biWidth-1 do begin
            byte((pp+(j*biWidth+i)*4+3)^):=255;
            move((buffer+i*3)^,(pp+(j*biWidth+i)*4)^,3);
{            byte((pp+(j*biWidth+i)*4+2)^):=buffer[i*3+2];
            byte((pp+(j*biWidth+i)*4+1)^):=buffer[i*3+1];
            byte((pp+(j*biWidth+i)*4  )^):=buffer[i*3  ];}
          end;
        end;
      end;
  end;
  Freemem(buffer);
  FontData:=nil;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  RegisterARGBImageReader('BMP',@ReadBMP,true);

end.

