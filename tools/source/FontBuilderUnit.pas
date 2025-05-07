{ -[Name]-------------------------------------------

              MKSZTSZ Font Builder Class

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

   Written by Gilby/MKSZTSZ                Freeware
   Hungary, 2021

  --------------------------------------------------

  -[Description]------------------------------------

   Feed it with letter images then build a PNGFont.
   It assumes that all letters have the same Y position.

  --------------------------------------------------
}
// Version info:
//
//  V1.00: Gilby - 2021.11.07-09
//    * Initial creation
//  V1.01: Gilby - 2023.03.24
//    * Fixed with font didn't create image big enough for all chars.
//    * Sort didn't work.
//  V1.02: Gilby - 2024.08.16
//    * Added DontCrop parameter to TFontBuilder. It means that the character
//      images won't be cropped, they will be used as they are.
//  V1.03: Gilby - 2025.04.22
//    * An exception is raised when no characters added and BuildFont called.
//    * Cropping algorythm treats a pixel empty, when:
//       - alpha channel is 0, or
//       - r,g and b channels are all 0.

unit FontBuilderUnit;

{$mode delphi}

interface

uses ARGBImageUnit, fgl;

type

  { TCharImage }

  TCharImage=class
    constructor Create(iImage:TARGBImage;iChar:char;iDontCrop:boolean);
    destructor Destroy; override;
  private
    fImage:TARGBImage;
    fOffsetY:integer;
    fChar:char;
  published
    property Image:TARGBImage read fImage;
    property OffsetY:integer read fOffsetY;
    property Character:char read fChar;
  end;

  TCharImages=TFPGObjectList<TCharImage>;

  { TFontBuilder }

  TFontBuilder=class
    constructor Create(iIsSort,iIsFixedWidth,iIsColorkey,iDontCrop:boolean);
    destructor Destroy; override;
    procedure AddChar(pImage:TARGBImage;pChar:char);
    procedure BuildFont(pOutFilename:string);
  private
    fImages:TCharImages;
    fIsFixedWidth,
    fIsColorkey,
    fIsSort,
    fDontCrop:boolean;
    procedure SortLetters;
  end;

implementation

uses SysUtils, FontDataUnit, ARGBImagePNGWriterUnit, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.03';

// -   --  --- ----------------------------------------------[ TCharImage ] ---

constructor TCharImage.Create(iImage:TARGBImage; iChar:char; iDontCrop:boolean);
var
  i,j,Left,Top,Right,Bottom:integer;
  p:pointer;
begin
  fChar:=iChar;
  if not iDontCrop then begin
    Left:=MaxLongint;
    Top:=MaxLongint;
    Right:=-1;
    Bottom:=-1;
    p:=iImage.Rawdata;
    for j:=0 to iImage.Height-1 do
      for i:=0 to iImage.Width-1 do begin
        if (dword(p^) and $ffffff<>0) and (dword(p^) and $ff000000<>0) then begin
//        if (dword(p^) and $ffffff)>0 then begin
//        if (dword(p^))>0 then begin
          if Left>i then Left:=i;
          if Top>j then Top:=j;
          if Right<i then Right:=i;
          if Bottom<j then Bottom:=j;
        end;
        inc(p,4);
      end;
  end else begin
    Left:=0;
    Top:=0;
    Right:=iImage.Width-1;
    Bottom:=iImage.Height-1;
  end;
  fImage:=TARGBImage.Create(Right-Left+1,Bottom-Top+1);
  iImage.Copy(Left,Top,Right-Left+1,Bottom-Top+1,fImage);
  fOffsetY:=Top;
end;

destructor TCharImage.Destroy;
begin
  fImage.Free;
end;

// -   --  --- --------------------------------------------[ TFontBuilder ] ---

constructor TFontBuilder.Create(iIsSort,iIsFixedWidth,iIsColorkey,iDontCrop:boolean);
begin
  fImages:=TCharImages.Create;
  fIsFixedWidth:=iIsFixedWidth;
  fIsColorkey:=iIsColorkey;
  fIsSort:=iIsSort;
  fDontCrop:=iDontCrop;
end;

destructor TFontBuilder.Destroy;
begin
  FreeAndNil(fImages);
  inherited Destroy;
end;

procedure TFontBuilder.AddChar(pImage: TARGBImage; pChar: char);
begin
  fImages.Add(TCharImage.Create(pImage,pChar,fDontCrop));
end;

procedure TFontBuilder.BuildFont(pOutFilename: string);
var i,CommonTop,FixedWidth,FullWidth,FullHeight,CurrentLeft:integer;
  Font:TARGBImage;
begin
  if fImages.Count>0 then begin
    FullHeight:=0;
    FullWidth:=0;
    CommonTop:=MaxLongint;
    FixedWidth:=0;
    if fIsSort then SortLetters;
    for i:=0 to fImages.Count-1 do begin
      if fImages[i].fOffsetY<CommonTop then
        CommonTop:=fImages[i].fOffsetY;
      if fImages[i].fImage.Width>FixedWidth then
        FixedWidth:=fImages[i].Image.Width;
      FullWidth+=fImages[i].Image.Width;
      if fImages[i].OffsetY-CommonTop+fImages[i].Image.Height>FullHeight then
        FullHeight:=fImages[i].OffsetY-CommonTop+fImages[i].Image.Height;
    end;
    if fIsFixedWidth then FullWidth:=FixedWidth*fImages.Count;
    Font:=TARGBImage.Create(FullWidth,FullHeight);
    Font.Clear;
    CurrentLeft:=0;
    Font.FontData:=TFontData.Create;
    for i:=0 to fImages.Count-1 do begin
      if not fIsFixedWidth then begin
        fImages[i].Image.CopyTo(0,0,fImages[i].Image.Width,fImages[i].Image.Height,
          CurrentLeft,fImages[i].OffsetY-CommonTop,Font);
        Font.FontData.SetCharBox(ord(fImages[i].Character),
          CurrentLeft,
          fImages[i].OffsetY-CommonTop,
          fImages[i].Image.Width,
          fImages[i].Image.Height);
        CurrentLeft+=fImages[i].Image.Width;
  //      with Font.FontData.CharBoxes[ord(CharSet[i+1])] do
  //        Log.Trace(Format('Char: %d (L:%d, T:%d, W:%d, H:%d)',[ord(CharSet[i+1]),x,y,w,h]));
      end else begin
        fImages[i].Image.CopyTo(0,0,fImages[i].Image.Width,fImages[i].Image.Height,
          CurrentLeft+(FixedWidth-fImages[i].Image.Width) div 2,fImages[i].OffsetY-CommonTop,Font);
        Font.FontData.SetCharBox(ord(fImages[i].Character),
          CurrentLeft,
          fImages[i].OffsetY-CommonTop,
          FixedWidth,
          fImages[i].Image.Height);
        CurrentLeft+=FixedWidth;
      end;
    end;
  //  writeln('Saving font file...');
    if fIsColorKey then Font.SetColorKey(0,0,0);
    Font.WriteFile(pOutFilename,'PNG');
    FreeAndNil(Font);
  end else
    raise Exception.Create('No characters were added!');
end;

procedure TFontBuilder.SortLetters;
var i,j:integer;
begin
//  writeln('Sorting...');
  Log.Trace('====');
  for i:=0 to fImages.Count-1 do
    Log.Trace(ord(fImages[i].Character));
  Log.Trace('----');
  for i:=fImages.Count-2 downto 0 do
    for j:=0 to i do
      if fImages[j].Character>fImages[j+1].Character then begin
        fImages.Exchange(j,j+1);
      end;
  for i:=0 to fImages.Count-1 do
    Log.Trace(ord(fImages[i].Character));
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

