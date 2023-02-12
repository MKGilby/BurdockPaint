// GIF reader for TARGBImage
// ------------------------------------------------------------------
// You can freely distribute the sources
//
// Written by Gilby/MKSZTSZ
// Hungary, 2023
// ------------------------------------------------------------------

// Version info:
//   1.00 - Gilby - 2023.02.03
//     * Initial creation

unit ARGBImageGIFReaderUnit;

{$mode Delphi}{$H+}

interface

implementation

uses Classes, SysUtils, ARGBImageUnit, AnimationDataUnit, FontDataUnit, Logger,
  BitFileStreamUnit;

const
  Fstr={$I %FILE%}+', ';
  Version='1.00';

  HEAD87='GIF87a';
  HEAD89='GIF89a';

var
  GlobalPalette,CurrentPalette:pointer;

function ReadByte(pSource:TStream):byte;
begin
  Result:=0;
  pSource.Read(Result,1);
end;

function ReadWord(pSource:TStream):word;
begin
  Result:=0;
  pSource.Read(Result,2);
end;

procedure LoadPalette(pSource:TStream;pColorCount:integer;palette:pointer);
var b:byte;
begin
  b:=0;
  while pColorCount>0 do begin
    pSource.Read(b,1);
    byte((palette+2)^):=b;
    pSource.Read(b,1);
    byte((palette+1)^):=b;
    pSource.Read(b,1);
    byte((palette+0)^):=b;
    byte((palette+3)^):=255;
    inc(palette,4);
    dec(pColorCount);
  end;
end;

procedure LoadImage(pSource:TStream;pRawData:pointer;pWidth:integer);
var Left,Top,Width,Height:integer;b:byte;
  localPaletteSize,currentLine,lineIncrement,interlacePass:integer;
  lzwCodeSize,lzwCurrentCodeSize:integer;
  interlaced:boolean;
  x,y,code,old:integer;
  Dictionary:array[0..4095] of string;
  CurrentDictionarySize:integer;
  ClearCode,EndOfData:integer;
  Bits:TBitFileStream;
  Data:TMemoryStream;

  procedure ReinitializeDictionary;
  var i,j:integer;
  begin
    j:=1<<(lzwCodeSize);
    for i:=0 to j-1 do Dictionary[i]:=chr(i);
    CurrentDictionarySize:=j+2;
    ClearCode:=j;
    EndOfData:=j+1;
  end;

  procedure AddDictionaryEntry(pEntry:string);
  begin
    Dictionary[CurrentDictionarySize]:=pEntry;
    inc(CurrentDictionarySize);
    if (CurrentDictionarySize=(1<<lzwCurrentCodeSize)) and
       (lzwCurrentCodeSize<12) then inc(lzwCurrentCodeSize);
  end;

  procedure PutPixels(s:string);
  var i:integer;pp:pointer;
  begin
    pp:=pRawData+(y*pWidth+x)*4;
    for i:=1 to length(s) do begin
      move((CurrentPalette+ord(s[i])*4)^,pp^,4);
      inc(x);inc(pp,4);
      if (x=Left+Width) then begin
        x:=Left;
        inc(y);
        pp:=pRawData+(y*pWidth+x)*4;
      end;
    end;
  end;

begin
  Left:=ReadWord(pSource);
  Top:=ReadWord(pSource);
  Width:=ReadWord(pSource);
  Height:=ReadWord(pSource);
//  Log.LogDebug(Format('Left=%d, Top=%d, Width=%d, Height=%d',[Left,Top,Width,Height]));
  b:=ReadByte(pSource);
  if b and $80<>0 then begin
    localPaletteSize:=1<<((b and $07)+1);
    CurrentPalette:=Getmem(localPaletteSize);
    LoadPalette(pSource,localPaletteSize,CurrentPalette);
  end else CurrentPalette:=GlobalPalette;
  interlaced:=b and $40<>0;
  currentLine:=0;
  interlacePass:=1;
  if interlaced then lineIncrement:=8 else lineIncrement:=1;
  lzwCodeSize:=ReadByte(pSource);
//  Log.LogDebug(Format('LZWCodeSize=%d',[lzwCodeSize]));
  if lzwCodeSize<>8 then raise Exception.Create(Format('LZW code size of %d is not yet supported.',[lzwCodeSize]));
  ReinitializeDictionary;
  lzwCurrentCodeSize:=lzwCodeSize+1;


  Data:=TMemoryStream.Create;
  x:=ReadByte(pSource);
  while x<>0 do begin
    Data.CopyFrom(pSource,x);
    x:=ReadByte(pSource);
  end;
  Data.Position:=0;

  x:=Left;
  y:=Top;
  Bits:=TBitFileStream.Create(Data,bdLeft);

  code:=Bits.GetBits(lzwCurrentCodeSize);
  if code=ClearCode then begin
    ReinitializeDictionary;
    lzwCurrentCodeSize:=lzwCodeSize+1;
    code:=Bits.GetBits(lzwCurrentCodeSize);
  end;
  PutPixels(Dictionary[code]);
  old:=code;
  repeat
    code:=Bits.GetBits(lzwCurrentCodeSize);
    if code=ClearCode then begin
      ReinitializeDictionary;
      lzwCurrentCodeSize:=lzwCodeSize+1;
      old:=-1;
    end else
    if code=EndOfData then begin

    end else begin
      if old=-1 then begin
        PutPixels(Dictionary[code]);
        old:=code;
      end else
      if code<CurrentDictionarySize then begin
        PutPixels(Dictionary[code]);
        AddDictionaryEntry(Dictionary[old]+Dictionary[code][1]);
        old:=code;
      end else begin
        PutPixels(Dictionary[old]+Dictionary[old][1]);
        AddDictionaryEntry(Dictionary[old]+Dictionary[old][1]);
        old:=code;
      end;
    end;
//    Log.LogDebug(Format('DictSize=%d',[CurrentDictionarySize]));
  until code=EndOfData;
  FreeAndNil(Bits);
  FreeAndNil(Data);

  if CurrentPalette<>GlobalPalette then Freemem(CurrentPalette);
end;

procedure ReadGIF(pSource:TStream;out Width,Height:integer;out RawData:pointer;Animations:TAnimationDatas;out FontData:TFontData);
var
  s:string;
  b:byte;
  gct,sort:boolean;
  colres,gctsize,bgcolindex,aspectratio:integer;
begin
  s:=#0#0#0#0#0#0;
  pSource.Read(s[1],6);
  if (s<>HEAD87) and (s<>HEAD89) then
    raise Exception.Create('Not a GIF file or unknown version!');
  Width:=ReadWord(pSource);
  Height:=ReadWord(pSource);
//  Log.LogDebug(Format('Image dimensions: %dx%d',[Width,Height]));
  b:=0;
  pSource.Read(b,1);
  gct:=b and $80<>0;
  colres:=(b and $70)>>4;
  sort:=b and $08<>0;
  gctsize:=1<<((b and $07)+1);
  s:='';
  if gct then s+='X' else s+=' ';
  if sort then s+='X' else s+=' ';
//  Log.LogDebug(Format('GlobalColorTable [%s], ColorResolution=%d, Sort [%s], GlobalColorTableSize=%d',[s[1],colres,s[2],gctsize]));
  bgcolindex:=0;
  pSource.Read(bgcolindex,1);
  aspectratio:=0;
  pSource.Read(aspectratio,1);
//  Log.LogDebug(Format('BackgroundColorIndex=%d, AspectRatio=%d',[bgcolindex,aspectratio]));
  RawData:=GetMem(Width*Height*4);
  fillchar(RawData^,Width*Height*4,0);
  GlobalPalette:=Getmem(gctsize*4);
  LoadPalette(pSource,gctsize,GlobalPalette);

  repeat
    pSource.Read(b,1);
//    Log.LogDebug(hexstr(b,2));
    if b=$2C then begin // Image descriptor
      LoadImage(pSource,RawData,Width);
    end else
    if b<>$3B then raise Exception.Create('Unknown block type!');
  until b=$3B;


  Freemem(GlobalPalette);
  FontData:=nil;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  RegisterARGBImageReader('GIF',@ReadGIF,true);

end.

