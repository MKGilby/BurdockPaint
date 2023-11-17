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

unit BDPButton;

{$mode Delphi}

interface

uses SysUtils, vcc2_ButtonStatic, Font2Unit, ARGBImageUnit, BDPMessage;

type

  { TBDButton }

  TBDButton=class(TButton)
    constructor Create(iX,iY,iWidth,iHeight:integer;iCaption,iHint:string;
          iAssignedobject:TObject=nil); overload;
    constructor Create; overload;
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
    procedure Click(Sender:TObject;x, y, buttons: integer);
  protected
    procedure ReDraw; override;
  private
    fHint:string;
    fAssignedObject:TObject;
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    fFont2:TFont;
    fMessage:TMessage;
  published
    property Hint:string read fHint write fHint;
    property AssignedObject:TObject read fAssignedObject write fAssignedObject;
  public
    property Message:TMessage read fMessage write fMessage;
  end;



implementation

uses BDPShared, mk_sdl2, sdl2;

{ TBDButton }

constructor TBDButton.Create(iX,iY,iWidth,iHeight:integer;
  iCaption,iHint:string; iAssignedobject:TObject);
begin
  Create;
  Left:=iX;
  Top:=iY;
  Width:=iWidth;
  Height:=iHeight;
  TextAlignX:=mjCenter;
  TextOffsetY:=2;
  Font:=MM.Fonts['Black'];
  fFont2:=MM.Fonts['Red'];
  fCaption:=iCaption;
  fName:=fCaption;
  fHint:=iHint;
  fAssignedObject:=iAssignedobject;
  fMessage:=TMessage.Init(MSG_NONE,0,0);
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  fNeedRedraw:=true;
end;

constructor TBDButton.Create;
begin
  inherited Create;
  fSelected:=false;
  OnMouseEnter:=Self.MouseEnter;
  OnMouseLeave:=Self.MouseLeave;
  OnClick:=Self.Click;
end;

procedure TBDButton.MouseEnter(Sender:TObject);
begin
  InfoBar.ShowText(fHint);
end;

procedure TBDButton.MouseLeave(Sender:TObject);
begin
  InfoBar.ShowText('');
end;

procedure TBDButton.Click(Sender:TObject; x,y,buttons:integer);
begin
  case buttons of
    1:begin  // Left click
        if fMessage.TypeID<>MSG_NONE then MessageQueue.AddMessage(fMessage);
      end;
  end;
end;

procedure TBDButton.ReDraw;
var tmp:TARGBImage;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  tmp:=TARGBImage.Create(fWidth,fHeight);
  try
    with tmp do begin
      Bar(8,0,Width-16,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(8,Height-3,fWidth-16,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(0,8,3,Height-16,SystemPalette[SYSTEMCOLORDARK]);
      Bar(Width-3,8,3,Height-16,SystemPalette[SYSTEMCOLORDARK]);
      if fSelected then
        Bar(3,3,Width-6,Height-6,SystemPalette[SYSTEMCOLORLIGHT])
      else begin
        if fEnabled then
          Bar(3,3,Width-6,Height-6,SystemPalette[SYSTEMCOLORMID])
        else
          Bar(3,3,Width-6,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      end;
    end;
    if Assigned(fTLImage) then
      fTLImage.CopyTo(0,0,fTLImage.Width,fTLImage.Height,0,0,tmp,true);
    if Assigned(fTRImage) then
      fTRImage.CopyTo(0,0,fTRImage.Width,fTRImage.Height,fWidth-8,0,tmp,true);
    if Assigned(fBLImage) then
      fBLImage.CopyTo(0,0,fBLImage.Width,fBLImage.Height,0,fHeight-8,tmp,true);
    if Assigned(fBRImage) then
      fBRImage.CopyTo(0,0,fBRImage.Width,fBRImage.Height,fWidth-8,fHeight-8,tmp,true);
    if not fSelected then begin
      if Assigned(fFont) then
        fFont.OutText(tmp,fCaption,fTextAlignPointX-fLeft,fTextAlignPointY+fTextOffsetY-fTop,fTextAlignX)
    end else begin
      if Assigned(fFont2) then
        fFont2.OutText(tmp,fCaption,fTextAlignPointX-fLeft,fTextAlignPointY+fTextOffsetY-fTop,fTextAlignX);
    end;

    fTexture:=TStaticTexture.Create(tmp);
    SDL_SetTextureBlendMode(fTexture.Texture,SDL_BLENDMODE_BLEND);
  finally
    tmp.Free;
  end;
end;

end.

