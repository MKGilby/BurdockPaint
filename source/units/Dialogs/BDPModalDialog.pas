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

unit BDPModalDialog;

{$mode Delphi}

interface

uses
  SysUtils, mk_sdl2, vcc2_Container, MKMouse2;

type

  { TBDModalOverlay }

  TBDModalOverlay=class(TMouseObject)
    constructor Create;
    procedure Draw; override;
  private
    DarkBar:TTexture;
  end;

  { TBDModalDialog }

  TBDModalDialog=class(TContainer)
    constructor Create(iWidth,iHeight:integer);
    destructor Destroy; override;
    function KeyDown(Sender:TObject;key:integer):boolean;
    function KeyUp(Sender:TObject;key:integer):boolean;
  protected
    fModalOverlay:TBDModalOverlay;
    fCaption:string;
    procedure ReDraw; override;
  private
    procedure fSetCaption(value:string);
  public
    property Caption:string read fCaption write fSetCaption;
  end;

implementation

uses BDPShared;

{ TBDModalOverlay }

constructor TBDModalOverlay.Create;
begin
  inherited Create;
  SetBounds(0,0,WINDOWWIDTH,WINDOWHEIGHT);
  ZIndex:=MODALDIALOG_ZINDEX-1;
  fName:='ModalOverlay';
  DarkBar:=MM.Textures.ItemByName['DarkBar'];
end;

procedure TBDModalOverlay.Draw;
begin
  PutTexture(0,0,DarkBar);
  ; // Just override the abstract method in TMouseObject
end;

{ TBDModalDialog }

constructor TBDModalDialog.Create(iWidth,iHeight:integer);
begin
  inherited Create;
  fModalOverlay:=TBDModalOverlay.Create;
  AddChild(fModalOverlay);
  fLeft:=(WINDOWWIDTH-iWidth) div 2;
  fTop:=(WINDOWHEIGHT-iHeight) div 2;
  Width:=iWidth;
  Height:=iHeight;
  OnKeyDown:=KeyDown;
  OnKeyUp:=KeyUp;
  ZIndex:=MODALDIALOG_ZINDEX;
  Visible:=false;
end;

destructor TBDModalDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TBDModalDialog.ReDraw;
begin
  if Assigned(fTexture) then with fTexture do begin
    // Panel border
    ARGBImage.Bar(0,0,Width,MODALDIALOGCAPTIONHEIGHT,SystemPalette[SYSTEMCOLORDARK]);
    ARGBImage.Bar(0,Height-3,fTexture.ARGBImage.Width,3,SystemPalette[SYSTEMCOLORDARK]);
    ARGBImage.Bar(0,MODALDIALOGCAPTIONHEIGHT,3,Height-3-MODALDIALOGCAPTIONHEIGHT,SystemPalette[SYSTEMCOLORDARK]);
    ARGBImage.Bar(Width-3,MODALDIALOGCAPTIONHEIGHT,3,ARGBImage.Height-3-MODALDIALOGCAPTIONHEIGHT,SystemPalette[SYSTEMCOLORDARK]);
    // Panel caption
    MM.Fonts['Black'].OutText(ARGBImage,fCaption,Width div 2,3,1);
    // Panel background
    ARGBImage.Bar(3,MODALDIALOGCAPTIONHEIGHT,Width-6,Height-3-MODALDIALOGCAPTIONHEIGHT,SystemPalette[SYSTEMCOLORMID]);
    // Update texture
    Update;
  end;
end;

function TBDModalDialog.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

function TBDModalDialog.KeyUp(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

procedure TBDModalDialog.fSetCaption(value:string);
begin
  if fCaption<>value then begin
    fCaption:=value;
    fNeedRedraw:=true;
  end;
end;

end.

