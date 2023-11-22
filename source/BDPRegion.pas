{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPRegion;

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

    // ---------------- Drawing operations -------------------
    // Puts source onto Self at x,y skipping pixels having colorkey value.
    procedure PutImage(x,y:integer;source:TARGBImage;colorkey:uint32); overload;

    // Draws a rectangle specified by its two corner.
    procedure RectangleXY(x1,y1,x2,y2:integer;color32:uint32);

    // -------------- Transforming operations --------------------
    // *** Move these to ARGBImage!
    // Recreates the image with the new dimensions.
    procedure Recreate(pWidth,pHeight:integer);

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
  private
    procedure LoadFromStreamV1(pSource:TStream);
  public
    property Left:integer read fLeft write fLeft;
    property Top:integer read fTop write fTop;
    property Changed:boolean read fChanged write fChanged;
  end;

implementation

uses MyZStreamUnit, Logger, BDPShared, MKToolbox, FastPaletteUnit,
  BDPInternalFileFormat;

const
  REGIONBLOCKID='RGN';


{ TBDRegion }

constructor TBDRegion.Create(iWidth,iHeight:integer);
begin
  inherited Create(iWidth,iHeight);
  Bar(0,0,Width,Height,0,0,0,255);
  fLeft:=0;
  fTop:=0;
  fChanged:=false;
end;

constructor TBDRegion.CreateFromStream(iStream:TStream);
begin
  inherited Create(16,16);
  fLeft:=0;
  fTop:=0;
  LoadFromStream(iStream);
  fChanged:=false;
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

procedure TBDRegion.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  try
    SaveToStream(Xs);
  finally
    Xs.Free;
  end;
end;

procedure TBDRegion.SaveToStream(Target:TStream);
var Xs:TStream;
begin
  Xs:=TMemoryStream.Create;
  try
    Xs.Write(fLeft,2);
    Xs.Write(fTop,2);
    Xs.Write(Width,2);
    Xs.Write(Height,2);
    Xs.Write(Rawdata^,Width*Height*4);
    TInternalFileFormat.WriteBlock(Target,REGIONBLOCKID,1,Xs);
  finally
    Xs.Free;
  end;
end;

procedure TBDRegion.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Xs);
  finally
    Xs.Free;
  end;
end;

procedure TBDRegion.LoadFromStream(Source:TStream);
var tmp:TInternalBlock;
begin
  tmp:=TInternalFileFormat.ReadBlock(Source);
  try
    if tmp.BlockID<>REGIONBLOCKID then
      raise Exception.Create(Format('Region block expected, got %s.',[tmp.BlockID]));
    if tmp.Version=1 then LoadFromStreamV1(tmp.Data)
    else raise Exception.Create(Format('Unknown region block version! (%d)',[tmp.Version]));
  finally
    tmp.Free;
  end;
  fChanged:=false;
end;

procedure TBDRegion.LoadFromStreamV1(pSource:TStream);
begin
  pSource.Read(fLeft,2);
  pSource.Read(fTop,2);
  pSource.Read(fWidth,2);
  pSource.Read(fHeight,2);
  Freemem(fRawdata);
  fRawdata:=Getmem(fWidth*fHeight*4);
  pSource.Read(fRawdata^,fWidth*fHeight*4);
end;

end.

