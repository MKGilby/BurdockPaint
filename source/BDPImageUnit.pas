unit BDPImageUnit;

{$mode Delphi}

interface

uses
  Classes, mk_sdl2, BDPPaletteUnit;

type

  { TBDImage }

  TBDImage=class
    constructor Create(iWidth,iHeight:integer);
    destructor Destroy; override;
    // Changes one pixel in the frame. Sets changed area accordingly.
    procedure PutPixel(x,y:integer;ColorIndex:word);
    // Gets colorindex of pixel.
    function GetPixel(x,y:integer):word;
    // Draws a straight line. Sets changed area accordingly.
    // Uses Bresenham's line drawing algorithm.
    procedure Line(x1,y1,x2,y2:integer;ColorIndex:word);
    // Draws a circle. Sets changed area accordingly.
    // Uses Bresenham's circle drawing algorithm.
    procedure Circle(cx,cy,r:integer;ColorIndex:word);
    // Draws a filled circle. Sets changed area accordingly.
    // Uses Bresenham's circle drawing algorithm.
    procedure FilledCircle(cx,cy,r:integer;ColorIndex:word);
    // Draws a filled rectangle.Sets changed area accordingly.
    procedure Bar(x1,y1,x2,y2:integer;ColorIndex:word);
    // Draws a rectangle. Sets changed area accordingly.
    procedure Rectangle(x1,y1,x2,y2:integer;ColorIndex:word);
    // FloodFills starting from the given pixel. Sets changed area accordingly.
    procedure FloodFill(x,y:integer;ColorIndex:word);
    // Resets changed area data.
    procedure ResetChange;
    // Puts another image into image, using colorkey is specified.
    // Sets changed area accordingly.
    procedure PutImage(x,y:integer;frame:TBDImage;colorkey:word=65535);

    // Renders the image onto a Texture.
    // TextureLeft, TextureTop   : The topleft position of the image on the target texture
    // RenderWidth, RenderHeight : The dimensions of rendering in IMAGE pixels
    // ImageLeft, ImageTop       : The topleft position of rendering in the image
    // Zoom                      : Zoom level (1->1x, 2->2x, 3->4x, 4->8x)
    procedure RenderToTexture(Target:TStreamingTexture;TextureLeft,TextureTop,RenderWidth,RenderHeight,ImageLeft,ImageTop,Zoom:integer);

    // Renders the image onto the PrimaryWindow
    // TextureLeft, TextureTop   : The topleft position of the image on PrimaryWindow
    // RenderWidth, RenderHeight : The dimensions of rendering in IMAGE pixels
    // ImageLeft, ImageTop       : The topleft position of rendering in the image
    // Zoom                      : Zoom level (1->1x, 2->2x, 3->4x, 4->8x)
    procedure RenderToScreen(TextureLeft,TextureTop,RenderWidth,RenderHeight,ImageLeft,ImageTop,Zoom:integer);

    // Renders the image onto the PrimaryWindow skipping all pixels
    // where alpha is not 255
    // TextureLeft, TextureTop   : The topleft position of the image on PrimaryWindow
    // RenderWidth, RenderHeight : The dimensions of rendering in IMAGE pixels
    // ImageLeft, ImageTop       : The topleft position of rendering in the image
    // Zoom                      : Zoom level (1->1x, 2->2x, 3->4x, 4->8x)
    procedure RenderToScreenAsOverlay(TextureLeft,TextureTop,RenderWidth,RenderHeight,ImageLeft,ImageTop,Zoom:integer);

    // Saves the raw image data (=array of colorindices) into a file. Used for debugging.
    procedure SaveRawDataToFile(fn:string);
    // Saves image to file, including palette data.
    procedure SaveToFile(pFilename:string);
    // Saves image to stream, including palette data.
    procedure SaveToStream(Target:TStream);
    // Loads image from file, including palette data.
    procedure LoadFromFile(pFilename:string);
    // Loads image from stream, including palette data.
    procedure LoadFromStream(Source:TStream);
    //    procedure CopyWholeFrame(Target:TBDFrame);
    //    procedure CopyAreaTo(Target:TBDFrame;x,y,w,h:integer);
    //    procedure CopyAreaFrom(Source:TBDFrame;x,y,w,h:integer);
    //    procedure CopyChangedTo(aTarget:TBDFrame);
    //    procedure CopyChangedFrom(Source:TBDFrame);
    //    procedure DoClipping(aFx,aFy,aFw,aFh:integer;var aVx,aVy,aVw,aVh:integer);
  private
    // Image dimensions
    fWidth,fHeight:integer;
    // Memory needed for holding pixel data
    fDataSize:integer;
    // Pixel data
    fData:pointer;
    // Color palette (since image stores only color indices)
    fPalette:TBDPalette;
    // Is the pixel data changed?
    fChanged:boolean;
    // If pixel data changed what area was affected?
    fChangedArea:TRect;
    // Sub proc for loading data from version 1 file structure.
    procedure LoadFromStreamV1(Source:TStream);
  public
    property Width:integer read fWidth;
    property Height:integer read fHeight;
    property Changed:boolean read fChanged write fChanged;
    property RawData:pointer read fData;
    property Palette:TBDPalette read fPalette;
  end;

implementation

uses SysUtils, MyZStreamUnit, SDL2;

const
  IMAGEFOURCC='BDPI';
  IMAGESAVEVERSION=1;

type
  TClipBox=record
    x1,x2,y1,y2:integer;
    wi,he:integer;
  end;

function GetClipBox(x1,y1,w1,h1,x2,y2,w2,h2:integer):TClipBox;
begin
  if x1<x2 then begin
    if x1+w1<x2 then begin
      Result.x1:=-1;  // 1
    end else begin
      Result.x2:=0;
      Result.x1:=x2-x1;
      if x1+w1<=x2+w2 then begin  // 2
        Result.wi:=x1+w1-x2;
      end else begin  // 3
        Result.wi:=w2;
      end;
    end;
  end else begin
    if x1<x2+w2 then begin
      Result.x1:=0;
      Result.x2:=x1-x2;
      if x1+w1<=x2+w2 then begin
        Result.wi:=w2;
      end else begin
        Result.wi:=x2+w2-x1;
      end;
    end else begin
      Result.x1:=-1;  // 6
    end;
  end;
  if y1<y2 then begin
    if y1+h1<y2 then begin
      Result.x1:=-1;  // 1 - Must be x1, it indicates when the areas are not overlapping
    end else begin
      Result.y2:=0;
      Result.y1:=y2-y1;
      if y1+h1<=y2+h2 then begin  // 2
        Result.he:=y1+h1-y2;
      end else begin  // 3
        Result.he:=h2;
      end;
    end;
  end else begin
    if y1<y2+h2 then begin
      Result.y1:=0;
      Result.y2:=y1-y2;
      if y1+h1<=y2+h2 then begin
        Result.he:=h2;
      end else begin
        Result.he:=y2+h2-y1;
      end;
    end else begin
      Result.x1:=-1;  // 6 - Must be x1, it indicates when the areas are not overlapping
    end;
  end;
end;

{ TBDImage }

constructor TBDImage.Create(iWidth,iHeight:integer);
begin
  fWidth:=iWidth;
  fHeight:=iHeight;
  fDataSize:=fWidth*fHeight*2;
  fData:=getmem(fDataSize);
  fillchar(fData^,fDataSize,0);
  fPalette:=TBDPalette.Create;
  ResetChange;
end;

destructor TBDImage.Destroy;
begin
  if Assigned(fPalette) then FreeAndNil(fPalette);
  freemem(fData);
  inherited Destroy;
end;

procedure TBDImage.PutPixel(x,y:integer; ColorIndex:word);
begin
  if (x>=0) and (x<fWidth) and (y>=0) and (y<fHeight) then begin
    word((fData+(y*fWidth+x)*2)^):=ColorIndex;
    fChanged:=true;
    if fChangedArea.Left>x then fChangedArea.Left:=x;
    if fChangedArea.Right<x then fChangedArea.Right:=x;
    if fChangedArea.Top>y then fChangedArea.Top:=y;
    if fChangedArea.Bottom<y then fChangedArea.Bottom:=y;
  end;
end;

function TBDImage.GetPixel(x,y:integer):word;
begin
  if (x>=0) and (x<fWidth) and (y>=0) and (y<fHeight) then begin
    Result:=word((fData+(y*fWidth+x)*2)^);
  end else
    Result:=65535;
end;

{Bresenham's Line Algorithm.  Byte, March 1988, pp. 249-253.}
// Taken from http://www.efg2.com/Lab/Library/Delphi/Graphics/Bresenham.txt
// Stripped a few comments, variable names changed here and there...
procedure TBDImage.Line(x1,y1,x2,y2:integer; ColorIndex:word);
var
  a,b,d : integer;
  diag_inc, nondiag_inc : integer;
  dx_diag, dx_nondiag, dy_diag, dy_nondiag : integer;
  i,swap,x,y : integer;
begin
  x := x1;
  y := y1;
  {Determine drawing direction and step to the next pixel.}
  a := x2 - x1;
  b := y2 - y1;
  {Determine whether end point lies to right or left of start point.}
  if a < 0 then begin
    a := -a;
    dx_diag := -1;
  end else
    dx_diag := 1;
  {Determine whether end point lies above or below start point.}
  if b < 0 then begin
    b := -b;
    dy_diag := -1
  end else
    dy_diag := 1;
  {Identify octant containing end point.}
  if a < b then begin
    swap := a;
    a := b;
    b := swap;
    dx_nondiag := 0;
    dy_nondiag := dy_diag
  end else begin
    dx_nondiag := dx_diag;
    dy_nondiag := 0
  end;
  d := b + b - a;
  nondiag_inc := b + b;
  diag_inc    := b + b - a - a;
  for i := 0 to a do begin   {draw the a+1 pixels}
    PutPixel(x,y,colorindex);
    if d < 0 then begin
      x := x + dx_nondiag;
      y := y + dy_nondiag;
      d := d + nondiag_inc
    end else begin
      x := x + dx_diag;
      y := y + dy_diag;
      d := d + diag_inc
    end;
  end;
end;

// Taken from https://www.geeksforgeeks.org/bresenhams-circle-drawing-algorithm/
procedure TBDImage.Circle(cx,cy,r:integer; ColorIndex:word);

  procedure PutPixel8(x,y:integer);
  begin
    PutPixel(cx+x, cy+y, ColorIndex);
    PutPixel(cx-x, cy+y, ColorIndex);
    PutPixel(cx+x, cy-y, ColorIndex);
    PutPixel(cx-x, cy-y, ColorIndex);
    PutPixel(cx+y, cy+x, ColorIndex);
    PutPixel(cx-y, cy+x, ColorIndex);
    PutPixel(cx+y, cy-x, ColorIndex);
    PutPixel(cx-y, cy-x, ColorIndex);
  end;

var x,y,d:integer;

begin
  x:=0;
  y:=r;
  d:=3-2*r;
  PutPixel8(x,y);
  while (y>=x) do begin
    inc(x);

    // check for decision parameter
    // and correspondingly
    // update d, x, y
    if d>0 then begin
      dec(y);
      d:=d+4*(x-y)+10;
    end else
      d:=d+4*x+6;
    PutPixel8(x,y);
  end;
end;

// Taken from https://www.geeksforgeeks.org/bresenhams-circle-drawing-algorithm/
procedure TBDImage.FilledCircle(cx,cy,r:integer; ColorIndex:word);

  procedure HLine(x,y:integer);
  var i:integer;
  begin
    for i:=-x to +x do begin
      PutPixel(cx+i,cy+y,ColorIndex);
      PutPixel(cx+i,cy-y,ColorIndex);
    end;
    for i:=-y to +y do begin
      PutPixel(cx+i,cy+x,ColorIndex);
      PutPixel(cx+i,cy-x,ColorIndex);
    end;
  end;

var x,y,d:integer;

begin
  x:=0;
  y:=r;
  d:=3-2*r;
  HLine(x,y);
  while (y>=x) do begin
    inc(x);

    // check for decision parameter
    // and correspondingly
    // update d, x, y
    if d>0 then begin
      dec(y);
      d:=d+4*(x-y)+10;
    end else
      d:=d+4*x+6;
    HLine(x,y);
  end;
end;

procedure TBDImage.Bar(x1,y1,x2,y2:integer; ColorIndex:word);
var i,j:integer;p:pointer;
begin
  if x1>x2 then begin i:=x1;x1:=x2;x2:=i;end;
  if y1>y2 then begin i:=y1;y1:=y2;y2:=i;end;

  fChanged:=true;
  if fChangedArea.Left>x1 then fChangedArea.Left:=x1;
  if fChangedArea.Right<x2 then fChangedArea.Right:=x2;
  if fChangedArea.Top>y1 then fChangedArea.Top:=y1;
  if fChangedArea.Bottom<y2 then fChangedArea.Bottom:=y2;

  for j:=y1 to y2 do begin
    p:=fData+j*fWidth+x1;
    for i:=x1 to x2 do
      word((p+i*2)^):=ColorIndex;
  end;
end;

procedure TBDImage.Rectangle(x1,y1,x2,y2:integer; ColorIndex:word);
var i:integer;
begin
  if x1>x2 then begin i:=x1;x1:=x2;x2:=i;end;
  if y1>y2 then begin i:=y1;y1:=y2;y2:=i;end;
  fChanged:=true;
  if fChangedArea.Left>x1 then fChangedArea.Left:=x1;
  if fChangedArea.Right<x2 then fChangedArea.Right:=x2;
  if fChangedArea.Top>y1 then fChangedArea.Top:=y1;
  if fChangedArea.Bottom<y2 then fChangedArea.Bottom:=y2;
  for i:=y1 to y2 do begin
    word((fData+(i*fWidth+x1)*2)^):=ColorIndex;
    word((fData+(i*fWidth+x2)*2)^):=ColorIndex;
  end;
  for i:=x1+1 to x2-1 do begin
    word((fData+(y1*fWidth+i)*2)^):=ColorIndex;
    word((fData+(y2*fWidth+i)*2)^):=ColorIndex;
  end;
end;

procedure TBDImage.FloodFill(x,y:integer; ColorIndex:word);
var i,j,cc:integer;w:boolean;

  function FFCheckPixel(x,y:integer;src,trg:word):boolean;
  begin
    Result:=false;
    if (y>0) and (GetPixel(x,y-1)=src) then begin
      PutPixel(x,y-1,trg);
      Result:=true;
    end;
    if (x<fWidth-1) and (GetPixel(x+1,y)=src) then begin
      PutPixel(x+1,y,trg);
      Result:=true;
    end;
    if (y<fHeight-1) and (GetPixel(x,y+1)=src) then begin
      PutPixel(x,y+1,trg);
      Result:=true;
    end;
    if (x>0) and (GetPixel(x-1,y)=src) then begin
      PutPixel(x-1,y,trg);
      Result:=true;
    end;
  end;

begin
  cc:=GetPixel(x,y);
  PutPixel(x,y,ColorIndex);
  w:=false;
  repeat
    for j:=0 to fHeight-1 do
      for i:=0 to fWidth-1 do
        w:=w or FFCheckPixel(i,j,cc,ColorIndex);
    if w then begin
      w:=false;
      for j:=fHeight-1 downto 0 do
        for i:=0 to fWidth-1 do
          w:=w or FFCheckPixel(i,j,cc,ColorIndex);
    end;
    if w then begin
      w:=false;
      for j:=fHeight-1 downto 0 do
        for i:=fWidth-1 downto 0 do
          w:=w or FFCheckPixel(i,j,cc,ColorIndex);
    end;
    if w then begin
      w:=false;
      for j:=0 to fHeight-1 do
        for i:=fWidth-1 downto 0 do
          w:=w or FFCheckPixel(i,j,cc,ColorIndex);
    end;
  until not w;
end;

procedure TBDImage.ResetChange;
begin
  fChanged:=false;
  fChangedArea.Left:=fWidth;
  fChangedArea.Right:=-1;
  fChangedArea.Top:=fHeight;
  fChangedArea.Bottom:=-1;
end;

procedure TBDImage.PutImage(x,y:integer; frame:TBDImage; colorkey:word);
var atm:TClipBox;i,j,c:integer;
begin
  atm:=GetClipBox(0,0,fWidth,fHeight,x,y,frame.Width,frame.Height);
  if atm.x1<>-1 then with atm do begin
    if (colorkey<fPalette.Size) then begin
      for i:=0 to he-1 do
        for j:=0 to wi-1 do begin
          c:=word((frame.RawData+(x2+j+(y2+i)*frame.Width)*2)^);
          if c<>colorkey then word((fData+(x1+j+(y1+i)*fWidth)*2)^):=c;
        end;
    end else begin
      for i:=0 to he-1 do
        move((frame.RawData+(x2+(y2+i)*frame.Width)*2)^,(fData+(x1+(y1+i)*fWidth)*2)^,wi*2);
    end;
    if fChangedArea.Left>x1 then fChangedArea.Left:=atm.x1;
    if fChangedArea.Right<x1+wi-1 then fChangedArea.Right:=x1+wi-1;
    if fChangedArea.Top>y1 then fChangedArea.Top:=y1;
    if fChangedArea.Bottom<y1+he-1 then fChangedArea.Bottom:=y1+he-1;
    fChanged:=true;
  end;
end;

procedure TBDImage.RenderToTexture(Target:TStreamingTexture;
  TextureLeft,TextureTop,RenderWidth,RenderHeight,ImageLeft,ImageTop,Zoom:integer);
var x,y,tx,ty,zPixel,zRenderWidth,zRenderHeight:integer;
    p:pointer;pitch:integer;
begin
  if not(Zoom in [1..4]) then exit;
  zPixel:=1<<(Zoom-1);

  RenderWidth:=RenderWidth div ZPixel;
  RenderHeight:=RenderHeight div ZPixel;

  if ImageLeft<0 then begin
    tx:=TextureLeft-ImageLeft*zPixel;
    zRenderWidth:=fWidth;
    ImageLeft:=0;
    if RenderWidth<zRenderWidth then zRenderWidth:=RenderWidth;
  end else begin
    tx:=0;
    zRenderWidth:=RenderWidth;
    if zRenderWidth>fWidth-ImageLeft then zRenderWidth:=fWidth-ImageLeft;
  end;

  if ImageTop<0 then begin
    ty:=TextureTop-ImageTop*zPixel;
    zRenderHeight:=fHeight;
    ImageTop:=0;
    if RenderHeight<zRenderHeight then zRenderHeight:=RenderHeight;
  end else begin
    ty:=0;
    zRenderHeight:=RenderHeight;
    if zRenderHeight>fHeight-ImageTop then zRenderHeight:=fHeight-ImageTop;
  end;

  if zPixel=1 then begin
    for y:=0 to zRenderHeight-1 do
      for x:=0 to zRenderWidth-1 do
        Target.ARGBImage.PutPixel(tx+x,ty+y,fPalette[GetPixel(ImageLeft+x,ImageTop+y)]);
  end else begin
    for y:=0 to zRenderHeight-1 do
      for x:=0 to zRenderWidth-1 do
        Target.ARGBImage.Bar(tx+x*zPixel,ty+y*zPixel,zPixel,zPixel,fPalette[GetPixel(ImageLeft+x,ImageTop+y)]);
  end;
end;

procedure TBDImage.RenderToScreen(TextureLeft,TextureTop,RenderWidth,
  RenderHeight,ImageLeft,ImageTop,Zoom:integer);
var x,y,tx,ty,zPixel,zRenderWidth,zRenderHeight:integer;
    p:word;
begin
  if not(Zoom in [1..4]) then exit;
  zPixel:=1<<(Zoom-1);

  RenderWidth:=RenderWidth div ZPixel;
  RenderHeight:=RenderHeight div ZPixel;

  if ImageLeft<0 then begin
    tx:=TextureLeft-ImageLeft*zPixel;
    zRenderWidth:=fWidth;
    ImageLeft:=0;
    if RenderWidth<zRenderWidth then zRenderWidth:=RenderWidth;
  end else begin
    tx:=0;
    zRenderWidth:=RenderWidth;
    if zRenderWidth>fWidth-ImageLeft then zRenderWidth:=fWidth-ImageLeft;
  end;

  if ImageTop<0 then begin
    ty:=TextureTop-ImageTop*zPixel;
    zRenderHeight:=fHeight;
    ImageTop:=0;
    if RenderHeight<zRenderHeight then zRenderHeight:=RenderHeight;
  end else begin
    ty:=0;
    zRenderHeight:=RenderHeight;
    if zRenderHeight>fHeight-ImageTop then zRenderHeight:=fHeight-ImageTop;
  end;

  if zPixel=1 then begin
    for y:=0 to zRenderHeight-1 do
      for x:=0 to zRenderWidth-1 do begin
        p:=GetPixel(ImageLeft+x,ImageTop+y);
        SDL_SetRenderDrawColor(
          PrimaryWindow.Renderer,
          fPalette.ColorR[p],
          fPalette.ColorG[p],
          fPalette.ColorB[p],
          fPalette.ColorA[p]);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,tx+x,ty+y);
      end;
  end else begin
    for y:=0 to zRenderHeight-1 do
      for x:=0 to zRenderWidth-1 do begin
        p:=GetPixel(ImageLeft+x,ImageTop+y);
        mk_sdl2.bar(tx+x*zPixel,ty+y*zPixel,zPixel,zPixel,
          fPalette.ColorR[p],fPalette.ColorG[p],fPalette.ColorB[p],fPalette.ColorA[p]);
      end;
  end;
end;

procedure TBDImage.RenderToScreenAsOverlay(TextureLeft, TextureTop,
  RenderWidth, RenderHeight, ImageLeft, ImageTop, Zoom: integer);
var x,y,tx,ty,zPixel,zRenderWidth,zRenderHeight:integer;
    p:word;
begin
  if not(Zoom in [1..4]) then exit;
  zPixel:=1<<(Zoom-1);

  RenderWidth:=RenderWidth div ZPixel;
  RenderHeight:=RenderHeight div ZPixel;

  if ImageLeft<0 then begin
    tx:=TextureLeft-ImageLeft*zPixel;
    zRenderWidth:=fWidth;
    ImageLeft:=0;
    if RenderWidth<zRenderWidth then zRenderWidth:=RenderWidth;
  end else begin
    tx:=0;
    zRenderWidth:=RenderWidth;
    if zRenderWidth>fWidth-ImageLeft then zRenderWidth:=fWidth-ImageLeft;
  end;

  if ImageTop<0 then begin
    ty:=TextureTop-ImageTop*zPixel;
    zRenderHeight:=fHeight;
    ImageTop:=0;
    if RenderHeight<zRenderHeight then zRenderHeight:=RenderHeight;
  end else begin
    ty:=0;
    zRenderHeight:=RenderHeight;
    if zRenderHeight>fHeight-ImageTop then zRenderHeight:=fHeight-ImageTop;
  end;

  if zPixel=1 then begin
    for y:=0 to zRenderHeight-1 do
      for x:=0 to zRenderWidth-1 do begin
        p:=GetPixel(ImageLeft+x,ImageTop+y);
        if fPalette.ColorA[p]=255 then begin
          SDL_SetRenderDrawColor(
            PrimaryWindow.Renderer,
            fPalette.ColorR[p],
            fPalette.ColorG[p],
            fPalette.ColorB[p],
            fPalette.ColorA[p]);
          SDL_RenderDrawPoint(PrimaryWindow.Renderer,tx+x,ty+y);
        end;
      end;
  end else begin
    for y:=0 to zRenderHeight-1 do
      for x:=0 to zRenderWidth-1 do begin
        p:=GetPixel(ImageLeft+x,ImageTop+y);
        if fPalette.ColorA[p]=255 then begin
          mk_sdl2.bar(tx+x*zPixel,ty+y*zPixel,zPixel,zPixel,
            fPalette.ColorR[p],fPalette.ColorG[p],fPalette.ColorB[p],fPalette.ColorA[p]);
        end;
      end;
  end;
end;

procedure TBDImage.SaveRawDataToFile(fn:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(fn,fmCreate);
  Xs.Write(fData^,fDataSize);
  FreeAndNil(Xs);
end;

procedure TBDImage.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  SaveToStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDImage.SaveToStream(Target:TStream);
var s:string;i:integer;
begin
  s:=IMAGEFOURCC;
  Target.Write(s[1],4);
  i:=IMAGESAVEVERSION;
  Target.Write(i,1);
  fPalette.SaveToStream(Target);
  Target.Write(fWidth,2);
  Target.Write(fHeight,2);
  Compress(fData^,Target,fDataSize);
//  Target.Write(fData^,fDataSize);
end;

procedure TBDImage.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead or fmShareDenyNone);
  LoadFromStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDImage.LoadFromStream(Source:TStream);
var s:string;b:byte;
begin
  s:=#0#0#0#0;
  Source.Read(s[1],4);
  if s<>IMAGEFOURCC then raise Exception.Create('Not an image file!');
  b:=0;
  Source.Read(b,1);
  if b=1 then LoadFromStreamV1(Source)
  else raise Exception.Create(Format('Unknown image file version! (%d)',[b]));
end;

procedure TBDImage.LoadFromStreamV1(Source:TStream);
var Xs:TMemoryStream;
begin
  fPalette.LoadFromStream(Source);
  fWidth:=0;fHeight:=0;
  Source.Read(fWidth,2);
  Source.Read(fHeight,2);
  Freemem(fData);
  fDataSize:=fWidth*fHeight*2;
  fData:=Getmem(fDataSize);
  Xs:=TMemoryStream.Create;
  UnCompressStream(Source,Xs);
  Xs.Position:=0;
  Xs.Read(fData^,fDataSize);
  FreeAndNil(Xs);
end;

end.

