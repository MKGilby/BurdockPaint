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

unit BDPCursor;

{$mode Delphi}

interface

type

  { TBDCursor }

  TBDCursor=class
    procedure Draw(x,y,zoomlevel:integer);  // x,y is the center (where the mouse points)
  end;

implementation

uses BDPShared, mk_sdl2, SDL2;

{ TBDCursor }

procedure TBDCursor.Draw(x,y,zoomlevel:integer);
var r:TSDL_Rect;
begin
  case zoomlevel of
    1:begin
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,
          SystemPalette.ColorR[3],
          SystemPalette.ColorG[3],
          SystemPalette.ColorB[3],
          255);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x-4,y);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x-3,y);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y-4);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y-3);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x+3,y);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x+4,y);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y+3);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y+4);
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,
          SystemPalette.ColorR[0],
          SystemPalette.ColorG[0],
          SystemPalette.ColorB[0],
          255);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x-2,y);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y-2);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x+2,y);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y+2);
        SDL_SetRenderDrawColor(
          PrimaryWindow.Renderer,
          Project.CurrentImage.Palette.ColorR[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorG[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorB[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorA[Settings.ActiveColorIndex]);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y);
      end;
    2:begin
        x-=x mod 2;
        y-=y mod 2;
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,
          SystemPalette.ColorR[3],
          SystemPalette.ColorG[3],
          SystemPalette.ColorB[3],
          255);
        r.x:=x-8;r.y:=y;r.w:=4;r.h:=2;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+6;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-8;r.w:=2;r.h:=4;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+6;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,
          SystemPalette.ColorR[0],
          SystemPalette.ColorG[0],
          SystemPalette.ColorB[0],
          255);
        r.x:=x-4;r.y:=y;r.w:=2;r.h:=2;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+4;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-4;r.w:=2;r.h:=2;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+4;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        Bar(x,y,2,2,
          Project.CurrentImage.Palette.ColorR[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorG[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorB[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorA[Settings.ActiveColorIndex]);
      end;
    3:begin
        x-=x mod 4;
        y-=y mod 4;
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,
          SystemPalette.ColorR[3],
          SystemPalette.ColorG[3],
          SystemPalette.ColorB[3],
          255);
        r.x:=x-16;r.y:=y;r.w:=8;r.h:=4;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+12;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-16;r.w:=4;r.h:=8;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+12;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,
          SystemPalette.ColorR[0],
          SystemPalette.ColorG[0],
          SystemPalette.ColorB[0],
          255);
        r.x:=x-8;r.y:=y;r.w:=4;r.h:=4;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+8;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-8;r.w:=4;r.h:=4;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+8;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        Bar(x,y,4,4,
          Project.CurrentImage.Palette.ColorR[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorG[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorB[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorA[Settings.ActiveColorIndex]);
      end;
    4:begin
        x-=x mod 8;
        y-=y mod 8;
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,
          SystemPalette.ColorR[3],
          SystemPalette.ColorG[3],
          SystemPalette.ColorB[3],
          255);
        r.x:=x-32;r.y:=y;r.w:=16;r.h:=8;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+24;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-32;r.w:=8;r.h:=16;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+24;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,
          SystemPalette.ColorR[0],
          SystemPalette.ColorG[0],
          SystemPalette.ColorB[0],
          255);
        r.x:=x-16;r.y:=y;r.w:=8;r.h:=8;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+16;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-16;r.w:=8;r.h:=8;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+16;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        Bar(x,y,8,8,
          Project.CurrentImage.Palette.ColorR[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorG[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorB[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorA[Settings.ActiveColorIndex]);
      end;
    5:begin
        x-=x mod 16;
        y-=y mod 16;
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,
          SystemPalette.ColorR[3],
          SystemPalette.ColorG[3],
          SystemPalette.ColorB[3],
          255);
        r.x:=x-64;r.y:=y;r.w:=32;r.h:=16;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+48;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-64;r.w:=16;r.h:=32;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+48;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,
          SystemPalette.ColorR[0],
          SystemPalette.ColorG[0],
          SystemPalette.ColorB[0],
          255);
        r.x:=x-32;r.y:=y;r.w:=16;r.h:=16;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+32;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-32;r.w:=16;r.h:=16;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+32;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        Bar(x,y,16,16,
          Project.CurrentImage.Palette.ColorR[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorG[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorB[Settings.ActiveColorIndex],
          Project.CurrentImage.Palette.ColorA[Settings.ActiveColorIndex]);
      end;
  end;
end;

end.

