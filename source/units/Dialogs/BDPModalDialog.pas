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
  end;

  { TBDModalDialog }

  TBDModalDialog=class(TContainer)
    constructor Create(iWidth,iHeight:integer);
    destructor Destroy; override;
    procedure ReDraw; override;
    function KeyDown(Sender:TObject;key:integer):boolean;
    function KeyUp(Sender:TObject;key:integer):boolean;
  protected
    fModalOverlay:TBDModalOverlay;
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
end;

procedure TBDModalOverlay.Draw;
begin
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
  if Assigned(fTexture.ARGBImage) then begin
    with fTexture.ARGBImage do Bar(0,0,Width,Height,0,0,0,0);
    fTexture.Update;
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

end.
