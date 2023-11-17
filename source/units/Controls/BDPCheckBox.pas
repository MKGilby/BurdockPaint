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

unit BDPCheckBox;

{$mode Delphi}

interface

uses SysUtils, vcc2_ButtonStatic, ARGBImageUnit, BDPMessage, MKMouse2;

type

  { TBDCheckBox }

  TBDCheckBox=class(TButton)
    constructor Create(iX,iY,iWidth,iHeight:integer;iHint:string;
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
    fMessage:TMessage;
  published
    property Hint:string read fHint write fHint;
    property AssignedObject:TObject read fAssignedObject write fAssignedObject;
  public
    OnChange:TSimpleEvent;
    property Message:TMessage read fMessage write fMessage;
  end;

implementation

uses BDPShared, sdl2, mk_sdl2;

{ TBDCheckBox }

constructor TBDCheckBox.Create(iX,iY,iWidth,iHeight:integer; iHint:string;
  iAssignedobject:TObject);
begin
  Create;
  Left:=iX;
  Top:=iY;
  Width:=iWidth;
  Height:=iHeight;
  fHint:=iHint;
  fAssignedObject:=iAssignedobject;
  fMessage:=TMessage.Init(MSG_NONE,0,0);
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  fNeedRedraw:=true;
  Font:=MM.Fonts['Red'];
end;

constructor TBDCheckBox.Create;
begin
  inherited Create;
  fSelected:=false;
  OnMouseEnter:=Self.MouseEnter;
  OnMouseLeave:=Self.MouseLeave;
  OnClick:=Self.Click;
  OnChange:=nil;
end;

procedure TBDCheckBox.MouseEnter(Sender:TObject);
begin
  InfoBar.ShowText(fHint);
end;

procedure TBDCheckBox.MouseLeave(Sender:TObject);
begin
  InfoBar.ShowText('');
end;

procedure TBDCheckBox.Click(Sender:TObject; x,y,buttons:integer);
begin
  case buttons of
    SDL_BUTTON_LEFT:begin  // Left click
      Selected:=not Selected;
      if Assigned(OnChange) then OnChange(Self);
//      if fMessage.TypeID<>MSG_NONE then MessageQueue.AddMessage(fMessage);
    end;
  end;
end;

procedure TBDCheckBox.ReDraw;
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
    if fSelected and Assigned(fFont) then
      fFont.OutText(tmp,#134,Width div 2,Height div 2-8,1);
    fTexture:=TStaticTexture.Create(tmp);
    SDL_SetTextureBlendMode(fTexture.Texture,SDL_BLENDMODE_BLEND);
  finally
    tmp.Free;
  end;
end;

end.

