{ -[Name]-------------------------------------------

   MKSZTSZ Visual Component Collection for SDL2

                                Simple Button Logic

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2023

  --------------------------------------------------

  -[Description]------------------------------------

   It is a simple pushbutton logic, doesn't offer drawing.
   For visible button see vcc_ARGBImageButton.

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
   Caption=Test

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2023.01.16-20
//    * Initial creation from vcc_Button
//  V1.01: Gilby - 2023.03.09
//    * Following changes in MKMouse2

{$mode delphi}
{$smartlink on}

unit vcc_ButtonLogic2;

interface

uses Classes, MKMouse2, MKINIFile;

type

  { TButtonLogic }

  TButtonLogic=class(TMouseObject)
    constructor Create; overload;
    constructor Create(iINI:TINIFile;iSection:string); overload;
    procedure DefaultOnMouseEnter(Sender:TObject);
    procedure DefaultOnMouseLeave(Sender:TObject);
    function DefaultOnMouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    function DefaultOnMouseUp(Sender:TObject;x,y,buttons:integer):boolean;
//    function OnClick(x,y,buttons:integer):boolean;
  protected
    fState:(cNormal,cHighlighted,cButtonDown);
    fTextAlignX,
    fTextAlignPointX,
    fTextAlignPointY,
    fTextOffsetY:integer;
    fCaption:string;
    procedure fSetLeft(value:integer);
    procedure fSetTop(value:integer);
    procedure fSetWidth(value:integer);
    procedure fSetHeight(value:integer);
    procedure fSetTextAlignX(value:integer);
  public
    property Left:integer read fLeft write fSetLeft;
    property Top:integer read fTop write fSetTop;
    property Width:integer read fWidth write fSetWidth;
    property Height:integer read fHeight write fSetHeight;
    property TextAlignX:integer read fTextAlignX write fSetTextAlignX;
    property TextOffsetY:integer read fTextOffsetY write fTextOffsetY;
    property Caption:string read fCaption write fCaption;
  end;
     
implementation

uses SysUtils, Font2Unit, MKToolBox, Logger;
     
const
  Fstr='vcc_ButtonLogic.pas, ';
  Version='1.01';

constructor TButtonLogic.Create;
begin
  inherited Create;
  fLeft:=0;
  fTop:=0;
  fWidth:=64;
  fHeight:=24;
  fTextAlignX:=mjLeft;
  fTextAlignPointX:=fLeft+2;
  fTextAlignPointY:=fTop+2;
  fTextOffsetY:=0;
  fCaption:='OK';

  OnMouseEnter:=Self.DefaultOnMouseEnter;
  OnMouseLeave:=Self.DefaultOnMouseLeave;
  OnMouseDown:=Self.DefaultOnMouseDown;
  OnMouseUp:=Self.DefaultOnMouseUp;

  fState:=cNormal;
  fClicked:=false;
end;

constructor TButtonLogic.Create(iINI:TINIFile; iSection:string);
var s:string;
begin
  inherited Create;
  Left:=iINI.ReadInteger(iSection,'Left',0);
  Top:=iINI.ReadInteger(iSection,'Top',0);
  Width:=iINI.ReadInteger(iSection,'Width',0);;
  Height:=iINI.ReadInteger(iSection,'Height',0);

  TextAlignX:=strtoint(decode(iINI.ReadString(iSection,'TextAlignX','Left'),'Center,1,Right,2,0'));
  TextOffsetY:=iINI.ReadInteger(iSection,'TextOffsetY',0);;

  Caption:=iINI.ReadString(iSection,'Caption','OK');

  OnMouseEnter:=Self.DefaultOnMouseEnter;
  OnMouseLeave:=Self.DefaultOnMouseLeave;
  OnMouseDown:=Self.DefaultOnMouseDown;
  OnMouseUp:=Self.DefaultOnMouseUp;

  fState:=cNormal;
  fClicked:=false;
end;

procedure TButtonLogic.fSetLeft(value:integer);
begin
  fLeft:=value;
  case fTextAlignX of
    mjLeft:fTextAlignPointX:=fLeft;
    mjCenter:fTextAlignPointX:=fLeft+fWidth div 2;
    mjRight:fTextAlignPointX:=fLeft+fWidth-1;
  end;
end;

procedure TButtonLogic.fSetWidth(value:integer);
begin
  if value>0 then begin
    fWidth:=value;
    case fTextAlignX of
      mjLeft:fTextAlignPointX:=fLeft;
      mjCenter:fTextAlignPointX:=fLeft+fWidth div 2;
      mjRight:fTextAlignPointX:=fLeft+fWidth-1;
    end;
  end;
end;

procedure TButtonLogic.fSetTop(value:integer);
begin
  fTop:=value;
end;

procedure TButtonLogic.fSetHeight(value:integer);
begin
  fHeight:=value;
end;

procedure TButtonLogic.fSetTextAlignX(value:integer);
begin
  if (value<>1) and (value<>2) then value:=0;
  fTextAlignX:=value;
  case fTextAlignX of
    mjLeft:fTextAlignPointX:=fLeft;
    mjCenter:fTextAlignPointX:=fLeft+fWidth div 2;
    mjRight:fTextAlignPointX:=fLeft+fWidth-1;
  end;
end;

procedure TButtonLogic.DefaultOnMouseEnter(Sender:TObject);
begin
  fState:=cHighLighted;
end;

procedure TButtonLogic.DefaultOnMouseLeave(Sender:TObject);
begin
  fState:=cNormal;
  fClicked:=false;
end;

function TButtonLogic.DefaultOnMouseDown(Sender:TObject;x,y,buttons:integer):boolean;
begin
  fState:=cButtonDown;
  Result:=true;
end;

function TButtonLogic.DefaultOnMouseUp(Sender:TObject;x,y,buttons:integer):boolean;
begin
  fState:=cHighLighted;
  Result:=true;
end;

{function TButtonLogic.OnClick(x,y,buttons:integer):boolean;
begin
end;}

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
