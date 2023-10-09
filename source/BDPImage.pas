{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of BurdockPaint.

  BurdockPaint is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  BurdockPaint is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  BurdockPaint. If not, see <https://www.gnu.org/licenses/>.
}

unit BDPImage;

{$mode Delphi}

interface

uses
  Classes, SysUtils, mk_sdl2, ARGBImageUnit;

type

  { TBDRegion }

  TBDRegion=class(TARGBImage)
    // Creates an empty region
    constructor Create(iWidth,iHeight:integer);
    // Creates region from stream (fileformats.txt - R-block)
    constructor CreateFromStream(iStream:TStream);
    // Free up entities
    destructor Destroy; override;

    // ---------------- Drawing operations -------------------
    // Puts source onto Self at x,y skipping pixels having colorkey value.
    procedure PutImage(x,y:integer;source:TARGBImage;colorkey:uint32); overload;

    // Draws a rectangle specified by its two corner.
    procedure RectangleXY(x1,y1,x2,y2:integer;color32:uint32);

    // -------------- Transforming operations --------------------
    // *** Move these to ARGBImage!
    // Recreates the image with the new dimensions.
    procedure Recreate(pWidth,pHeight:integer);

    // -------------- Rendering operations --------------------
    // Renders the image onto a Texture.
    // TextureLeft, TextureTop   : The topleft position of the image on the target texture
    // RenderWidth, RenderHeight : The dimensions of rendering in IMAGE pixels
    // ImageLeft, ImageTop       : The topleft position of rendering in the image
    // Zoom                      : Zoom level (1->1x, 2->2x, 3->4x, 4->8x)
    procedure RenderToTexture(Target:TStreamingTexture;
      TextureLeft,TextureTop,RenderWidth,RenderHeight,ImageLeft,ImageTop,Zoom:integer);

    // Renders the image onto a Texture skipping all pixels where alpha is not 255.
    // TextureLeft, TextureTop   : The topleft position of the image on the target texture
    // RenderWidth, RenderHeight : The dimensions of rendering in IMAGE pixels
    // ImageLeft, ImageTop       : The topleft position of rendering in the image
    // Zoom                      : Zoom level (1->1x, 2->2x, 3->4x, 4->8x)
    procedure RenderToTextureAsOverlay(Target:TStreamingTexture;
      TextureLeft,TextureTop,RenderWidth,RenderHeight,ImageLeft,ImageTop,Zoom:integer);

    // Renders the image onto the PrimaryWindow
    // TextureLeft, TextureTop   : The topleft position of the image on PrimaryWindow
    // RenderWidth, RenderHeight : The dimensions of rendering in IMAGE pixels
    // ImageLeft, ImageTop       : The topleft position of rendering in the image
    // Zoom                      : Zoom level (1->1x, 2->2x, 3->4x, 4->8x)
    //procedure RenderToScreen(ScreenLeft,ScreenTop,RenderWidth,RenderHeight,ImageLeft,ImageTop,Zoom:integer);

    // Renders the image onto the PrimaryWindow skipping all pixels
    // where alpha is not 255
    // TextureLeft, TextureTop   : The topleft position of the image on PrimaryWindow
    // RenderWidth, RenderHeight : The dimensions of rendering in IMAGE pixels
    // ImageLeft, ImageTop       : The topleft position of rendering in the image
    // Zoom                      : Zoom level (1->1x, 2->2x, 3->4x, 4->8x)
    //procedure RenderToScreenAsOverlay(ScreenLeft,ScreenTop,RenderWidth,RenderHeight,ImageLeft,ImageTop,Zoom:integer);

    // ------------- File/Stream operations ----------------
    // Saves region to file.
    procedure SaveToFile(pFilename:string);
    // Saves region to stream.
    procedure SaveToStream(Target:TStream);
    // Loads region from file.
    procedure LoadFromFile(pFilename:string);
    // Loads region from stream.
    procedure LoadFromStream(Source:TStream);
  protected
    // Image position (used for CELImage, leave on 0,0 otherwise)
    fLeft,fTop:integer;
    // Is the pixel data changed?
    fChanged:boolean;
  public
    property Left:integer read fLeft write fLeft;
    property Top:integer read fTop write fTop;
    property Changed:boolean read fChanged write fChanged;
  end;

implementation

uses MyZStreamUnit, Logger, BDPShared, MKToolbox, FastPaletteUnit;

{$i includes\ntsccol.inc}

const
  REGIONBLOCKID='RGN';


{ TBDRegion }

constructor TBDRegion.Create(iWidth,iHeight:integer);
begin
  inherited Create(iWidth,iHeight);
  fLeft:=0;
  fTop:=0;
  fChanged:=false;
end;

constructor TBDRegion.CreateFromStream(iStream:TStream);
begin
  inherited Create(16,16);
  LoadFromStream(iStream);
  fLeft:=0;
  fTop:=0;
  fChanged:=false;
end;

destructor TBDRegion.Destroy;
begin
  inherited Destroy;
end;

procedure TBDRegion.PutImage(x,y:integer; source:TARGBImage; colorkey:uint32);
var clipbox:TClipBox;i,j:integer;c:uint32;
begin
  if Assigned(source) then begin
    clipbox:=GetClipBox(0,0,Width,Height,x,y,source.Width,source.Height);
    if clipbox.x1<>-1 then with clipbox do begin
      for j:=0 to he-1 do
        for i:=0 to wi-1 do begin
          c:=uint32((source.RawData+(x2+i+(y2+j)*source.Width)*4)^);
          if c<>colorkey then uint32((fRawdata+(x1+i+(y1+j)*Width)*4)^):=c;
        end;
    end;
  end else
    raise Exception.Create('TBDImage.PutImage: Source is not assigned!');
end;

procedure TBDRegion.RectangleXY(x1,y1,x2,y2:integer; color32:uint32);
var i:integer;
begin
  if x1>x2 then begin i:=x1;x1:=x2;x2:=i;end;
  if y1>y2 then begin i:=y1;y1:=y2;y2:=i;end;
  Rectangle(x1,y1,x2-x1+1,y2-y1+1,color32);
end;

procedure TBDRegion.Recreate(pWidth,pHeight:integer);
begin
  if Assigned(fRawdata) then Freemem(fRawdata);
  fWidth:=pWidth;
  fHeight:=pHeight;
  fRawdata:=Getmem(fWidth*fHeight*4);
end;

procedure TBDRegion.RenderToTexture(Target:TStreamingTexture;
  TextureLeft,TextureTop,RenderWidth,RenderHeight,ImageLeft,ImageTop,Zoom:integer);
var x,y,zPixel,zRenderWidth,zRenderHeight:integer;
begin
  if not(Zoom in [1..MAXZOOMLEVEL]) then exit;
  zPixel:=1<<(Zoom-1);

  // RenderWidth and Height comes in real pixels, convert to image pixels.
  zRenderWidth:=RenderWidth div ZPixel;
  zRenderHeight:=RenderHeight div ZPixel;

  FillChar(Target.ARGBImage.Rawdata^,Target.ARGBImage.Width*Target.ARGBImage.Height*4,0);
  if (zRenderWidth>0) and (zRenderHeight>0) and
     (ImageLeft<Width) and (ImageLeft+zRenderWidth>0) and
     (ImageTop<Height) and (ImageTop+zRenderHeight>0) then begin
    // Still check for clipping
    if ImageLeft<0 then begin zRenderWidth+=ImageLeft;TextureLeft-=ImageLeft*zPixel;ImageLeft:=0;end;
    if ImageLeft+zRenderWidth>Width then zRenderWidth:=Width-ImageLeft;
    if ImageTop<0 then begin zRenderHeight+=ImageTop;TextureTop-=ImageTop*zPixel;ImageTop:=0;end;
    if ImageTop+zRenderHeight>Height then zRenderHeight:=Height-ImageTop;


    // Probably this could be a simple PutImagePart
    if zPixel=1 then begin
      for y:=0 to zRenderHeight-1 do
        for x:=0 to zRenderWidth-1 do
          Target.ARGBImage.PutPixel(TextureLeft+x,TextureTop+y,GetPixel(ImageLeft+x,ImageTop+y));
    end else begin
      for y:=0 to zRenderHeight-1 do
        for x:=0 to zRenderWidth-1 do
          Target.ARGBImage.Bar(TextureLeft+x*zPixel,TextureTop+y*zPixel,zPixel,zPixel,GetPixel(ImageLeft+x,ImageTop+y));
    end;

  end;
end;

procedure TBDRegion.RenderToTextureAsOverlay(Target:TStreamingTexture;
  TextureLeft,TextureTop,RenderWidth,RenderHeight,ImageLeft,ImageTop,Zoom:integer);
var x,y,zPixel,zRenderWidth,zRenderHeight:integer;p:uint32;
begin
  if not(Zoom in [1..MAXZOOMLEVEL]) then exit;
  zPixel:=1<<(Zoom-1);

  // RenderWidth and Height comes in real pixels, convert to image pixels.
  zRenderWidth:=RenderWidth div ZPixel;
  zRenderHeight:=RenderHeight div ZPixel;

  if (zRenderWidth>0) and (zRenderHeight>0) and
     (ImageLeft<Width) and (ImageLeft+zRenderWidth>0) and
     (ImageTop<Height) and (ImageTop+zRenderHeight>0) then begin
    // Still check for clipping
    if ImageLeft<0 then begin zRenderWidth+=ImageLeft;TextureLeft-=ImageLeft*zPixel;ImageLeft:=0;end;
    if ImageLeft+zRenderWidth>Width then zRenderWidth:=Width-ImageLeft;
    if ImageTop<0 then begin zRenderHeight+=ImageTop;TextureTop-=ImageTop*zPixel;ImageTop:=0;end;
    if ImageTop+zRenderHeight>Height then zRenderHeight:=Height-ImageTop;

    // Probably this could be a simple PutImagePart
    if zPixel=1 then begin
      for y:=0 to zRenderHeight-1 do
        for x:=0 to zRenderWidth-1 do begin
          p:=GetPixel(ImageLeft+x,ImageTop+y);
          if p and $ff000000<>0 then
            Target.ARGBImage.PutPixel(TextureLeft+x,TextureTop+y,GetPixel(ImageLeft+x,ImageTop+y));
        end;
    end else begin
      for y:=0 to zRenderHeight-1 do
        for x:=0 to zRenderWidth-1 do begin
          p:=GetPixel(ImageLeft+x,ImageTop+y);
          if p and $ff000000<>0 then
            Target.ARGBImage.Bar(TextureLeft+x*zPixel,TextureTop+y*zPixel,zPixel,zPixel,GetPixel(ImageLeft+x,ImageTop+y));
        end;
    end;

  end;
end;

procedure TBDRegion.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  SaveToStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDRegion.SaveToStream(Target:TStream);
var i:integer;curr:int64;Xs,Ys:TStream;s:string;
begin
  s:=REGIONBLOCKID+#1;
  curr:=Target.Position;
  i:=0;
  Target.Write(i,4);  // ID placeholder
  Target.Write(i,4);  // Size placeholder

  Xs:=TMemoryStream.Create;
  Xs.Write(fLeft,2);
  Xs.Write(fTop,2);
  Xs.Write(Width,2);
  Xs.Write(Height,2);
  Xs.Write(Rawdata^,Width*Height*4);
  Xs.Position:=0;
  Ys:=TMemoryStream.Create;
  CompressStream(Xs,Ys,Xs.Size);
  if Xs.Size<=Ys.Size then begin
    Xs.Position:=0;
    Target.CopyFrom(Xs,Xs.Size);
  end else begin
    s[1]:=chr(ord(s[1]) or $20);
    Ys.Position:=0;
    Target.CopyFrom(Ys,Ys.Size);
  end;
  FreeAndNil(Ys);
  FreeAndNil(Xs);
  i:=Target.Position-curr-8;
  Target.Position:=curr;
  Target.Write(s[1],4);
  Target.write(i,4);
  Target.Position:=Target.Position+i;
end;

procedure TBDRegion.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead or fmShareDenyNone);
  LoadFromStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDRegion.LoadFromStream(Source:TStream);
var curr,size:uint32;b:byte;s:string;Xs:TStream;
begin
  s:=#0#0#0#0;
  Source.Read(s[1],4);
  if uppercase(system.copy(s,1,3))<>REGIONBLOCKID then
    raise Exception.Create(Format('Region block id expected, got %s.',[system.copy(s,1,3)]));
  size:=0;
  Source.Read(size,4);
  curr:=Source.Position;
  b:=ord(s[4]);
  if b=1 then begin
    Xs:=TMemoryStream.Create;
    if ord(s[1]) and $20<>0 then
      UnCompressStream(Source,Xs)
    else
      Xs.CopyFrom(Source,Size);
    Xs.Position:=0;
    Xs.Read(fLeft,2);
    Xs.Read(fTop,2);
    Xs.Read(fWidth,2);
    Xs.Read(fHeight,2);
    Freemem(fRawdata);
    fRawdata:=Getmem(fWidth*fHeight*4);
    Xs.Read(fRawdata^,fWidth*fHeight*4);
    FreeAndNil(Xs);
  end else raise Exception.Create(Format('Invalid region block version! (%d)',[b]));
  Source.Position:=curr+size;
  fChanged:=false;
end;

end.

