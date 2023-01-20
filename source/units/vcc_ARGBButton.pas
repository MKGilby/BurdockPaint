{ -[Name]-------------------------------------------

   MKSZTSZ Visual Component Collection for SDL2

                   Simple Text Button for ARGBImage

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2023

  --------------------------------------------------

  -[Description]------------------------------------

   It is a simple pushbutton logic.

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
//  V1.00: Gilby - 2023.01.16
//    * Initial creation from vcc_Button

{$mode delphi}
{$smartlink on}

unit vcc_ARGBButton;

interface

uses Classes, vcc_ButtonLogic2, MKINIFile, Font2Unit, FontList2Unit, ARGBImageUnit;

type

  { TARGBButton }

  TARGBButton=class(TButtonLogic)
    constructor Create(iTarget:TARGBImage); overload;
    constructor Create(iTarget:TARGBImage;iINI:TINIFile;iSection:string;
      iFontList:TFontList); overload;
    procedure Draw; override;
    function DefaultOnMouseEnter(Sender:TObject;{%H-}x,{%H-}y,{%H-}buttons:integer):boolean;
    function DefaultOnMouseLeave(Sender:TObject;{%H-}x,{%H-}y,{%H-}buttons:integer):boolean;
    function DefaultOnMouseDown(Sender:TObject;{%H-}x,{%H-}y,{%H-}buttons:integer):boolean;
    function DefaultOnMouseUp(Sender:TObject;{%H-}x,{%H-}y,{%H-}buttons:integer):boolean;
//    function OnClick(x,y,buttons:integer):boolean;
  protected
    fTarget:TARGBImage;
    fFont:TFont;
    fBorderColor,
    fNormalColor,
    fHighlightedColor,
    fPushedColor:uint32;
    procedure fSetTop(value:integer);
    procedure fSetHeight(value:integer);
    procedure fSetFont(font:TFont);
  public
    property Font:TFont read fFont write fSetFont;
    property BorderColor:UInt32 read fBorderColor write fBorderColor;
    property NormalColor:UInt32 read fNormalColor write fNormalColor;
    property HighlightedColor:UInt32 read fHighlightedColor write fHighlightedColor;
    property PushedColor:UInt32 read fPushedColor write fPushedColor;
  end;
     
implementation

uses SysUtils, MKToolBox, Logger;
     
const
  Fstr='vcc_ARGBButton.pas, ';
  Version='1.00';



constructor TARGBButton.Create(iTarget:TARGBImage);
begin
  inherited Create;
  fTarget:=iTarget;
  fLeft:=0;
  fTop:=0;
  fWidth:=64;
  fHeight:=24;
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

  OnMouseEnter:=Self.DefaultOnMouseEnter;
  OnMouseLeave:=Self.DefaultOnMouseLeave;
  OnMouseDown:=Self.DefaultOnMouseDown;
  OnMouseUp:=Self.DefaultOnMouseUp;

  fState:=cNormal;
  fClicked:=false;
end;

constructor TARGBButton.Create(iTarget:TARGBImage; iINI:TINIFile;
  iSection:string; iFontList:TFontList);
var s:string;
begin
  inherited Create;
  fTarget:=iTarget;
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
  OnMouseEnter:=Self.DefaultOnMouseEnter;
  OnMouseLeave:=Self.DefaultOnMouseLeave;
  OnMouseDown:=Self.DefaultOnMouseDown;
  OnMouseUp:=Self.DefaultOnMouseUp;

  fState:=cNormal;
  fClicked:=false;
end;

procedure TARGBButton.fSetTop(value:integer);
begin
  fTop:=value;
  if Assigned(fFont) then
    fTextAlignPointY:=fTop+(fHeight-fFont.Height) div 2;
end;

procedure TARGBButton.fSetHeight(value:integer);
begin
  fHeight:=value;
  if Assigned(fFont) then
    fTextAlignPointY:=fTop+(fHeight-fFont.Height) div 2;
end;

procedure TARGBButton.fSetFont(font:TFont);
begin
  fFont:=font;
  fTextAlignPointY:=fTop+(fHeight-fFont.Height) div 2;
end;

function TARGBButton.DefaultOnMouseEnter(Sender:TObject;x,y,buttons:integer):boolean;
begin
  fState:=cHighLighted;
  Result:=true;
end;

procedure TARGBButton.Draw;

  procedure DrawButton(color:uint32);
  begin
    fTarget.Rectangle(fLeft,fTop,fWidth,fHeight,fBorderColor);
    fTarget.Bar(fLeft+1,fTop+1,fWidth-2,fHeight-2,color);
  end;

begin
  case fState of
    cNormal:DrawButton(fNormalColor);
    cHighlighted:DrawButton(fHighlightedColor);
    cButtonDown:DrawButton(fPushedColor);
  end;
  fFont.OutText(fTarget,fCaption,fTextAlignPointX,fTextAlignPointY+fTextOffsetY,fTextAlignX);
end;

function TARGBButton.DefaultOnMouseLeave(Sender:TObject;x,y,buttons:integer):boolean;
begin
  fState:=cNormal;
  fClicked:=false;
  Result:=true;
end;

function TARGBButton.DefaultOnMouseDown(Sender:TObject;x,y,buttons:integer):boolean;
begin
  fState:=cButtonDown;
  Result:=true;
end;

function TARGBButton.DefaultOnMouseUp(Sender:TObject;x,y,buttons:integer):boolean;
begin
  fState:=cHighLighted;
  Result:=true;
end;

{function TARGBButton.OnClick(x,y,buttons:integer):boolean;
begin
end;}

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
