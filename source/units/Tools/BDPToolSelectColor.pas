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

unit BDPToolSelectColor;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolSelectColor }

  TBDToolSelectColor=class(TBDTool)
    constructor Create; override;
    procedure Move(x,y:integer); override;
    //
    function Click(x,y,button:integer):boolean; override;
    // Tell the color under the mouse to the tool.
    // It remembers the color and updates the InfoBar accordingly.
    procedure SetColor(colorindex:uint32;pRMBText:string='');
  private
    fColorIndex:integer;
  end;

implementation

uses sdl2, BDPShared;

{ TBDToolSelectColor }

constructor TBDToolSelectColor.Create;
begin
  inherited Create;
  fName:='SELCOL';
  fHint:=uppercase('Select color to draw.');
  fColorIndex:=-1;
end;

procedure TBDToolSelectColor.Move(x,y:integer);
begin
  inherited Move(x,y);
  if (fX>=0) and (fX<Project.CurrentImage.Width) and (fY>=0) and (fY<Project.CurrentImage.Height) then
    SetColor(Project.CurrentImage.GetPixel(fX,fY),'CLOSE PAL. ED.')
  else
    SetColor(0);
end;

function TBDToolSelectColor.Click(x,y,button:integer):boolean;
begin
  if button=SDL_BUTTON_LEFT then begin
    if (x>=0) and (x<Project.CurrentImage.Width) and (y>=0) and (y<Project.CurrentImage.Height) then
      Settings.ActiveColor:=Project.CurrentImage.GetPixel(x,y);
  end else
  if button=SDL_BUTTON_RIGHT then begin
    MessageQueue.AddMessage(MSG_COLOREDITORRESP);
  end;
  Result:=true;
end;

procedure TBDToolSelectColor.SetColor(colorindex:uint32; pRMBText:string);
begin
{  if (colorindex>=0) and (colorindex<Project.CurrentPalette.Size) then begin
    fColorIndex:=colorindex;
    if colorindex>-1 then begin
      InfoBar.ShowText(Format('COLOR INDEX=%d (R=%d, G=%d, B=%d, A=%d) '#132'SELECT '#133+pRMBText,
        [fColorIndex,
         Project.CurrentPalette.ColorR[fColorIndex],
         Project.CurrentPalette.ColorG[fColorIndex],
         Project.CurrentPalette.ColorB[fColorIndex],
         Project.CurrentPalette.ColorA[fColorIndex]]))
    end else begin
      InfoBar.ShowText('');
    end;
  end;}
end;

end.

