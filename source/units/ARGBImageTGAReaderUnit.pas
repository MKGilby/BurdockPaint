// MKSZTSZ TGA reader for TARGBImage class (for SDL2)
// ------------------------------------------------------------------
// You can freely distribute the sources under the GNU GPL Version 2.
//
// Written by Gilby/MKSZTSZ
// Hungary, 2020
// ------------------------------------------------------------------

// Version info:
//   1.00 - Gilby - 2020.03.11
//     * Initial creation from ARGBImageTGAUnit
//   1.01 - Gilby - 2020.03.17-30
//     * Following changes in ARGBImageUnit
//   1.02 - Gilby - 2020.04.01
//     * Following changes in ARGBImageUnit (TAnimationData -> TAnimationDatas)
//   1.03 - Gilby - 2020.06.25
//     * Following changes in AnimationDataUnit
//   1.04: Gilby - 2022.03.18
//     * Following changes in AnimationDataUnit
//   1.05: Gilby - 2022.07.19
//     * Fix with ATGAs with zero frames. (It is one frame not zero)

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

{define Mator}

unit ARGBImageTGAReaderUnit;

interface

implementation

uses Classes, SysUtils, ARGBImageUnit, MKToolBox, AnimationDataUnit,
  FontDataUnit, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.05';

procedure ReadTGA24RLE(iSource:TStream;iWidth,iHeight:integer;var p);
// const Istr=Fstr+'ReadTGARLE24';
var pp:pointer;x,y:integer;b:byte;ii:UInt32;
begin
  x:=0;y:=0;
  pp:=@p;
  ii:=$FFFFFFFF;
  b:=0;
  repeat
    iSource.Read(b,1);
    if b and 128=128 then begin
      b:=(b and 127)+1;
//      Log.Trace('Compressed run: '+inttostr(b));
      iSource.Read(ii,3);
      while b>0 do begin
        uint32(pp^):=ii;
//        move(ii,pp^,4);
        inc(pp,4);
        inc(x);
        if x=iWidth then begin
          x:=0;inc(y);
        end;
        dec(b);
      end;
    end else begin
      inc(b);
//          Log.Trace('Uncompressed run: '+inttostr(b));
      while b>0 do begin
//        iSource.Read(pp^,3);
        iSource.Read(ii,3);
        uint32(pp^):=ii;
        inc(pp,4);
        inc(x);
        if x=iWidth then begin
          x:=0;inc(y);
        end;
        dec(b);
      end;
    end;
  until y>=iHeight;
end;

procedure ReadTGA24RLEFlip(iSource:TStream;iWidth,iHeight:integer;var p);
//const Istr=Fstr+'ReadTGARLE24FLIP';
var pp:pointer;x,y,dif:integer;b:byte;ii:uint32;
begin
  x:=0;y:=0;
  pp:=@p+(iHeight-1)*iWidth*3;
  dif:=iWidth*-4*2;
  ii:=0;
  repeat
    iSource.Read(ii,4);  // Read the RLE indicator and the first pixel
    b:=ii and 255;
    ii:=ii>>8 or $FF000000;
    if b and 128=128 then begin  // Packed, so the read pixel is the fill color
      b:=(b and 127)+1;
      inc(x,b);
      while b>0 do begin
        uint32(pp^):=ii;
        inc(pp,4);
        dec(b);
      end;
      if x=iWidth then begin
        x:=0;inc(y);
        pp:=pp+dif;
      end;
    end else begin // Write first pixel, and then copy the rest
      // b is one less than the full required pixel number
      move(ii,pp^,4);
      inc(pp,3);
      inc(x);
      while b>0 do begin
        iSource.Read(ii,3);
        uint32(pp^):=ii;
        inc(x,1);
        inc(pp,4);
        dec(b);
      end;
      if x=iWidth then begin
        x:=0;inc(y);
        pp:=pp+dif;
      end;
    end;
  until y>=iHeight;
end;

procedure RearrangeAnimationV2H(src,trg:pointer;Vwidth,Vheight,framecount:integer);
var Hwidth,fhe,x,y:integer;
begin
  Hwidth:=Vwidth*FrameCount;
  fhe:=Vheight div FrameCount;
  for x:=0 to framecount-1 do
    for y:=0 to fhe-1 do
      move((src+(y+x*fhe)*Vwidth*4)^,(trg+(y*Hwidth+x*Vwidth)*4)^,Vwidth*4);
end;

procedure ReadTGA(pSource:TStream;out Width,Height:integer;out RawData:pointer;Animations:TAnimationDatas;out FontData:TFontData);
const
  ad2:array[0..7] of integer=(128,64,32,16,8,4,2,1);
  EXTRABUFSIZE=8+32;

var
  iif,cmap,itc,cmes,ips,idb:byte;
  cmo,cml,oX,oY,wi,he,extratype:integer;
  extra:array[0..EXTRABUFSIZE-1] of byte;
  tmp:word;
  i,j,x,y:integer;ii:uint32;
  pp:pointer;b:byte;
  pal:TARGBPalette;
  buf:pointer;
  atm:TAnimationData;

  function DecodePixel(source:integer):uint32;
  begin
    Result:=0;
    case itc of
      9:move(pal[source],Result,3);
     10:begin
          case ips of
            16:begin
                 pSource.Read(tmp,2);
                 case (idb and 15) of
                   0:begin
                       Result:=((tmp and $f800)>>8)+
                               ((tmp and $07e0)>>3)<<8+
                               ((tmp and $001e)<<3)<<16;
                     end;
                   1:begin
                       Result:=((tmp and $7c00)>>7)+
                               ((tmp and $03e0)>>2)<<8+
                               ((tmp and $001e)<<3)<<16;
                     end;
                 end;
               end;
            24,32:i:=source;
          end;
        end;
     11:begin
          Result:=source<<16+source<<8+source;
        end;
    end;
//    Log.Trace('DecodePixel('+inttostr(source)+')='+inttostr(Result));
  end;

begin
  cmo:=0;cml:=0;oX:=0;oY:=0;wi:=0;he:=0;
  iif:=0;cmap:=0;itc:=0;cmes:=0;ips:=0;idb:=0;
  pSource.Read(iif,1);   // Image Identifier Field
  pSource.Read(cmap,1);  // Color Map Type
  pSource.Read(itc,1);   // Image Type Code
  pSource.Read(cmo,2);   // Color Map Origin
  pSource.Read(cml,2);   // Color Map Length
  pSource.Read(cmes,1);  // Color Map Entry Size
  pSource.Read(oX,2);    // Origin X
  pSource.Read(oY,2);    // Origin Y
  pSource.Read(wi,2);    // Width
  pSource.Read(he,2);    // Height
  pSource.Read(ips,1);   // Image Pixel Size
  pSource.Read(idb,1);   // Image Descriptor Byte
  if (itc=10) then begin
    if not(ips in [16,24,32]) then
      raise Exception.Create(Format('ReadTGA: Illegal Image Pixel Size! (%d)',[ips]));
    if (ips=16) and not (idb and $0f in [0,1]) then
      raise Exception.Create(Format('ReadTGA: Illegal Image Descriptor Byte! (%d)',[idb]));
  end;

  case iif of
    0:extratype:=0;       // None
    2:extratype:=1;       // ATGA
    16:extratype:=2;      // ATGA2
    32:extratype:=3;      // TGAFont
    else extratype:=255;  // Unknown
  end;
  extra[0]:=0;
  if iif>0 then pSource.Read(extra[0],iif); // IIF read to extra

  Width:=wi;
  Height:=he;
//  Log.LogDebug(inttostr(_width)+', '+inttostr(_height),Istr);
  getmem(Rawdata,wi*he*4);
  pp:=Rawdata;
  pal[0,0]:=0;

//  LogTicks.LogTicks('TGA:ReadHeader');
  case itc of
    0:writeln('RTGA: No image data included in file!');
// ------------------ Paletted ------------------------
    1:begin
//        ReadPalEx(iSource,0,cml,pal);
        ii:=$ff000000;
        for i:=0 to cml-1 do begin
          pSource.Read(ii,3);
          move(ii,pal[i,0],4);
        end;
        getmem(buf,wi);
        for ii:=0 to he-1 do begin
          if (idb and $20)=0 then i:=he-ii-1
                             else i:=ii;
          pSource.Read(buf^,wi);
          for j:=0 to wi-1 do
            move(pal[byte((buf+j)^)],(pp+(i*wi+j)*4)^,4);
        end;
        freemem(buf,wi);
//        if ReadGlobalPal then move(pal,Palette,cml*4);
      end;
//------------------- Uncompressed RGB ------------------------------
    2:begin
        if (oX<>0) or (oY<>0) then writeln('RTGA: Ignoring Origin settings!');
        tmp:=0;
        for ii:=0 to he-1 do begin
          if (idb and $20)=0 then i:=he-ii-1
                             else i:=ii;
          for j:=0 to wi-1 do begin
            case ips of
             16:begin
                  pSource.Read(tmp,2);
                  case (idb and 15) of
                    0:begin
                        byte((pp+(i*wi+j)*4)^):=(tmp and $f800)>>8;
                        byte((pp+(i*wi+j)*4+1)^):=(tmp and $07e0)>>3;
                        byte((pp+(i*wi+j)*4+2)^):=(tmp and $001e)<<3;
                        byte((pp+(i*wi+j)*4+3)^):=255;
                      end;
                    1:begin
                        byte((pp+(i*wi+j)*4)^):=(tmp and $7c00)>>7;
                        byte((pp+(i*wi+j)*4+1)^):=(tmp and $03e0)>>2;
                        byte((pp+(i*wi+j)*4+2)^):=(tmp and $001e)<<3;
                        byte((pp+(i*wi+j)*4+3)^):=255;
                      end;
                    else begin writeln('RTGA: Illegal Image Descriptor Byte!');exit;end;
                  end;
                end;
             24:begin
                  pSource.Read((pp+(i*wi+j)*4)^,3);
                  byte((pp+(i*wi+j)*4+3)^):=255;
                end;
             32:pSource.Read((pp+(i*wi+j)*4)^,4);
              else begin writeln('RTGA: Illegal Image Pixel Size!');exit;end;
            end;
          end;
        end;
      end;
//------------------- 8-bit grayscaled ------------------------------
    3:begin
        if ips<>8 then begin writeln('Illegal Image Pixel Size!');exit;end;
        getmem(buf,wi);
        for ii:=0 to he-1 do begin
          if (idb and $20)=0 then i:=he-ii-1
                             else i:=ii;
          pSource.Read(buf^,wi);
          for j:=0 to wi-1 do begin
            byte((pp+(i*wi+j)*4)^):=byte((buf+j)^);
            byte((pp+(i*wi+j)*4+1)^):=byte((buf+j)^);
            byte((pp+(i*wi+j)*4+2)^):=byte((buf+j)^);
            byte((pp+(i*wi+j)*4+3)^):=255;
          end;
        end;
        freemem(buf,wi);
//        Log.DumpMemory(fRawdata^,0,wi*he*4,'','');
      end;
//-------------------------------------------------------------------
    10:begin
         if (idb and $20)=0 then
           ReadTGA24RLEFlip(pSource,wi,he,Rawdata^)
         else
           ReadTGA24RLE(pSource,wi,he,Rawdata^);
       end;
    9,11:begin
      j:=1;ii:=0;
//      Log.Trace('Pixel size: '+inttostr(j));
      x:=0;y:=0;b:=0;
      if (idb and $20)=0 then pp:=Rawdata+(he-1)*wi*4 else pp:=Rawdata;
      repeat
        pSource.Read(b,1);
        if b and 128=128 then begin
          b:=(b and 127)+1;
//          Log.Trace('Compressed run: '+inttostr(b));
          pSource.Read(ii,j);
          ii:=DecodePixel(ii) or $ff000000;
          while b>0 do begin
            move(ii,(pp+x*4)^,4);
            inc(x);
            if x=wi then begin
              x:=0;inc(y);
              if (idb and $20)=0 then pp:=pp-wi*4 else pp:=pp+wi*4;
            end;
            dec(b);
          end;
        end else begin
          inc(b);
//          Log.Trace('Uncompressed run: '+inttostr(b));
          while b>0 do begin
            pSource.Read(ii,j);
            ii:=DecodePixel(ii) or $ff000000;
            move(ii,(pp+x*4)^,4);
            inc(x);
            if x=wi then begin
              x:=0;inc(y);
              if (idb and $20)=0 then pp:=pp-wi*4 else pp:=pp+wi*4;
            end;
            dec(b);
          end;
        end;
      until y>=he;
    end;
   32:writeln('RTGA: Compressed images are not supported!');
   33:writeln('RTGA: Compressed images are not supported!');
   else writeln('RTGA: Unkown image type!');
  end;
//  LogTicks.LogTicks('TGA:ReadImageData');
  FontData:=nil;

  case extratype of
    1:begin  // ATGA
        i:=(extra[1] and 15)*256+extra[0];   // FrameCount
        if i=0 then i:=1;
        atm:=TAnimationData.Create(Width div i,Height);
        atm.Paused:=(extra[1] and 128)=128;
        atm.RandomStart:=(extra[1] and 32)=32;
        atm.Looped:=(extra[1] and 16)=16;
        for j:=0 to i-1 do atm.AddFrame(j*i,0);
        Animations.AddObject(atm.Name,atm);
      end;
    2:begin  // ATGA2
        i:=(extra[1] and 15)*256+extra[0];   // FrameCount
        if extra[6] and 128>0 then begin
          getmem(pp,Width*Height*4);
          RearrangeAnimationV2H(RawData,pp,width,height,i);
          freemem(RawData,Width*Height*4);
          RawData:=pp;
          Width:=Width*i;
          Height:=Height div i;
        end;
        atm:=TAnimationData.Create(Width div i,Height);
        atm.Paused:=(extra[1] and 128)=128;
        atm.RandomStart:=(extra[1] and 32)=32;
        atm.Looped:=(extra[1] and 16)=16;
        atm.FrameDelay:=extra[2];
        atm.LoopDelay:=extra[3];
        atm.StartFrame:=extra[4]+extra[5]*256;
        for j:=0 to i-1 do atm.AddFrame(j*i,0);
        Animations.AddObject(atm.Name,atm);
      end;
    3:begin  // TGAFont
        FontData:=TFontData.Create;
        for i:=0 to 255 do begin
          if extra[i div 8] and ad2[i mod 8]<>0 then begin
            pSource.Read(Extra[32],8);
            FontData.SetCharBox(i,
              extra[32]+extra[33]*256,
              extra[34]+extra[35]*256,
              extra[36]+extra[37]*256-(extra[32]+extra[33]*256)+1,
              extra[38]+extra[39]*256-(extra[34]+extra[35]*256)+1);
          end;
        end;
      end;
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  RegisterARGBImageReader('TGA',@ReadTGA,true);

end.
