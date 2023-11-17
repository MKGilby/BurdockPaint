{ -[Name]-------------------------------------------

   MKSZTSZ Visual Component Collection for SDL2

                                 Simple Text Button

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

   Written by Gilby/MKSZTSZ               Freeware!
   Hungary, 2023

  --------------------------------------------------

  -[Description]------------------------------------

   It is a simple pushbutton.

   You can set it up
     - from an .INI file,
     - or from code, assigning values to properties.

   A valid INI section:

   [Button]
   Left=280
   Top=228
   Width=80
   Height=24
   TextAlignX=Center
   TextOffsetY=1
   Font=Font0
   Caption=Test
   BorderColor=$c0a080
   NormalColor=$403020
   HighlightedColor=$806040
   PushedColor=$201810

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2023.11.17
//    * Initial creation form vcc2_Button

{$mode delphi}
{$smartlink on}

unit vcc2_ButtonStatic;

interface

uses Classes, vcc2_ButtonLogicStatic, MKINIFile, Font2Unit, FontList2Unit, mk_sdl2;

type

  { TButton }

  TButton=class(TButtonLogic)
    constructor Create; overload;
    constructor Create(iINI:TINIFile;iSection:string;iFontList:TFontList); overload;
  protected
    fFont:TFont;
    fBorderColor,
    fNormalColor,
    fHighlightedColor,
    fPushedColor:uint32;
    procedure fSetTop(value:integer);
    procedure fSetHeight(value:integer); override;
    procedure fSetFont(font:TFont);
    procedure ReDraw; override;
  public
    property Top:integer read fTop write fSetTop;
    property Height:integer read fHeight write fSetHeight;
    property Font:TFont read fFont write fSetFont;
    property BorderColor:UInt32 read fBorderColor write fBorderColor;
    property NormalColor:UInt32 read fNormalColor write fNormalColor;
    property HighlightedColor:UInt32 read fHighlightedColor write fHighlightedColor;
    property PushedColor:UInt32 read fPushedColor write fPushedColor;
  end;
     
implementation

uses SysUtils, MKToolBox, Logger, ARGBImageUnit, sdl2;
     
const
  Fstr={$I %FILE%}+', ';
  Version='1.03';


{ TButton }

constructor TButton.Create;
begin
  inherited Create;
  fTextAlignX:=mjLeft;
  fTextAlignPointX:=fLeft+2;
  fTextAlignPointY:=fTop+2;
  fTextOffsetY:=0;
  fFont:=nil;
  fCaption:='OK';
  fBorderColor:=$ffC0C0C0;
  fNormalColor:=$ff202020;
  fHighlightedColor:=$ff808080;
  fPushedColor:=$ff010101;
end;

constructor TButton.Create(iINI:TINIFile; iSection:string; iFontList:TFontList);
var s:string;
begin
  inherited Create;
  Left:=iINI.ReadInteger(iSection,'Left',0);
  Top:=iINI.ReadInteger(iSection,'Top',0);
  Width:=iINI.ReadInteger(iSection,'Width',0);;
  Height:=iINI.ReadInteger(iSection,'Height',0);

  TextAlignX:=strtoint(decode(iINI.ReadString(iSection,'TextAlignX','Left'),'Center,1,Right,2,0'));
  TextOffsetY:=iINI.ReadInteger(iSection,'TextOffsetY',0);;

  s:=iINI.ReadString(iSection,'Font','not defined');
  Font:=iFontList[s];
  if Font=nil then raise Exception.Create(Format('Font not found! (%s)',[s]));

  Caption:=iINI.ReadString(iSection,'Caption','OK');

  BorderColor:=iINI.ReadUInt32(iSection,'BorderColor',$ffc0c0c0);
  NormalColor:=iINI.ReadUInt32(iSection,'NormalColor',$ff202020);
  HighlightedColor:=iINI.ReadUInt32(iSection,'HighlightedColor',$ff808080);
  PushedColor:=iINI.ReadUInt32(iSection,'PushedColor',$ff010101);
  OnMouseEnter:=Self.MouseEnter;
  OnMouseLeave:=Self.MouseLeave;
  OnMouseDown:=Self.MouseDown;
  OnMouseUp:=Self.MouseUp;

  fState:=cNormal;
  fClicked:=false;
end;

procedure TButton.fSetTop(value:integer);
begin
  fTop:=value;
  if Assigned(fFont) then
    fTextAlignPointY:=fTop+(fHeight-fFont.Height) div 2;
end;

procedure TButton.fSetHeight(value:integer);
begin
  inherited fSetHeight(value);
  if Assigned(fFont) then
    fTextAlignPointY:=fTop+(fHeight-fFont.Height) div 2;
end;

procedure TButton.fSetFont(font:TFont);
begin
  fFont:=font;
  fTextAlignPointY:=fTop+(fHeight-fFont.Height) div 2;
end;

procedure TButton.ReDraw;

  procedure DrawButton(color:uint32);
  begin
    fImage.Rectangle(0,0,fWidth,fHeight,fBorderColor);
    fImage.Bar(1,1,fWidth-2,fHeight-2,color);
  end;

begin
  case fState of
    cNormal:DrawButton(fNormalColor);
    cHighlighted:DrawButton(fHighlightedColor);
    cButtonDown:DrawButton(fPushedColor);
  end;
  if Assigned(fFont) then
    fFont.OutText(fImage,fCaption,fTextAlignPointX-fLeft,fTextAlignPointY+fTextOffsetY-fTop,fTextAlignX);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
