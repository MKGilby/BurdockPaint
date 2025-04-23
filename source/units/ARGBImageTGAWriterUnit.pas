// MKSZTSZ TGA writer for TARGBImage class (for SDL2)
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
//   1.03 - Gilby - 2024.12.04
//     * Following changes in AnimationDataUnit and FontDataUnit
//   1.04 - Gilby - 2025.04.23
//     * Following changes in used units

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit ARGBImageTGAWriterUnit;

interface

implementation

uses Classes, SysUtils, ARGBImageUnit, MKToolBox, Logger, FastPaletteUnit,
  AnimationDataUnit, FontDataUnit;

const
  Fstr={$I %FILE%}+', ';
  Version='1.04';

procedure RearrangeAnimationH2V(src,trg:pointer;wi,he,framecount:integer);
var x,y,fwi:integer;
begin
  fwi:=wi div framecount;
  for x:=0 to framecount-1 do
    for y:=0 to he-1 do
      move((src+(y*wi+x*fwi)*4)^, (trg+(y+x*he)*fwi*4)^, fwi*4);
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

procedure WriteTGA(pTarget:TStream;pWidth,pHeight:integer;pRawData:pointer;pAnimations:TAnimationDatas;pFontData:TFontData);
const ad2:array[0..7] of integer=(128,64,32,16,8,4,2,1);
var s:string;extralength,i,j,lwi,lhe:integer;pp,pr:pointer;
    palcount:integer;
    r:byte;
    Grayscaled:boolean;
    pal,palettedimage:pointer;
    extra:array[0..31] of byte;
    atm:TCharRect;
    anim:TFrameBasedAnimationData;
begin
  if (pWidth<1) or (pHeight<1) then raise Exception.Create('Empty picture!');

  pr:=pRawdata;
  lwi:=pWidth;
  lhe:=pHeight;
  for i:=0 to 31 do extra[i]:=0;
  extralength:=0;
  if (pAnimations.Count>0) and (pAnimations.Items[0] is TFrameBasedAnimationData) then begin
    anim:=TFrameBasedAnimationData(pAnimations.Items[0]);
    if (anim.FrameDelay>0) or (anim.LoopDelay>0) then begin
      extralength:=16;
      extra[2]:=anim.FrameDelay;
      extra[3]:=anim.LoopDelay;
      extra[4]:=anim.StartFrame and $ff;
      extra[5]:=(anim.StartFrame>>8) and $ff;
    end else
      extralength:=2;
    j:=anim.FrameCount;
    if anim.Looped then j+=(1<<12);
    if anim.RandomStart then j+=(2<<12);
    if anim.Paused then j+=(4<<12);
    extra[0]:=j and 255;
    extra[1]:=(j>>8) and 255;
  end;
  if Assigned(pFontData) then begin
    extralength:=32;
    for i:=0 to 255 do
      if pFontData.Charboxes[i].Width>0 then
        extra[i div 8]:=extra[i div 8] or ad2[i mod 8];
  end;

  pTarget.Write(ExtraLength,1);

  GetPalettedImage2(pWidth,pHeight,pr,palettedimage,pal,palcount,Grayscaled);

  if palettedimage=nil then begin // True color
    s:=#0#2#0#0#0#0#24#0#0#0#0;
    pTarget.Write(s[1],length(s));
    pTarget.Write(lwi,2);
    pTarget.Write(lhe,2);
    s:=#24#32;
    pTarget.Write(s[1],length(s));
    if ExtraLength>0 then pTarget.Write(extra[0],ExtraLength);
    for i:=0 to pWidth*pHeight-1 do
      pTarget.Write((pr+i*4)^,3);
  end else begin // Paletted
    if Grayscaled then begin  // Grayscaled
      s:=#0+#3+#0#0+#0#0+#0+#0#0+#0#0;
      pTarget.Write(s[1],length(s));
      pTarget.Write(lwi,2);
      pTarget.Write(lhe,2);
      s:=#8#32;
      pTarget.Write(s[1],length(s));
      if ExtraLength>0 then pTarget.Write(extra[0],ExtraLength);
      pp:=pr;
      for i:=0 to pWidth*pHeight-1 do begin
        r:=byte(pp^);
        pTarget.Write(r,1);
        pp+=4;
      end;
    end else begin
      s:=#1#1#0#0;
      pTarget.Write(s[1],length(s));
      j:=palcount;
      pTarget.Write(j,2);
      s:=#24#0#0#0#0;
      pTarget.Write(s[1],length(s));
      pTarget.Write(lwi,2);
      pTarget.Write(lhe,2);
      s:=#8#32;
      pTarget.Write(s[1],length(s));
      if ExtraLength>0 then pTarget.Write(extra[0],ExtraLength);
      pTarget.Write(pal^,j*3);
      pTarget.Write(palettedimage^,pWidth*pHeight);
    end;
    Freemem(pal,768);
    Freemem(palettedimage,pWidth*pHeight);
  end;
  if extralength=32 then begin  // Write font chars data
    for i:=0 to 255 do
      if pFontData.Charboxes[i].Width>0 then begin
        atm:=pFontData.CharBoxes[i];
        pTarget.Write(atm.Left,2);
        pTarget.Write(atm.Top,2);
        j:=atm.Left+atm.Width-1;
        pTarget.Write(j,2);
        j:=atm.Top+atm.Height-1;
        pTarget.Write(j,2);
      end;
  end;
{  if (Extra._extratype=3) and (length(Extra._extrabytes)>32) then
//    pTarget.CopyFrom(Extra,Extra.Size-33);
    pTarget.Write(Extra._extrabytes[32],length(Extra._extrabytes)-32);}
  if pr<>pRawdata then freemem(pr,pWidth*pHeight*3);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  RegisterARGBImageWriter('TGA',@WriteTGA);


end.
