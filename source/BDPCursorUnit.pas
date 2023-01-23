unit BDPCursorUnit;

{$mode Delphi}

interface

type

  { TBDCursor }

  TBDCursor=class
    procedure Draw(x,y,zoomlevel:integer);  // x,y is the center (where the mouse points)
  end;

implementation

uses BDPSharedUnit, mk_sdl2, SDL2;

{ TBDCursor }

procedure TBDCursor.Draw(x,y,zoomlevel:integer);
var r:TSDL_Rect;
begin
  case zoomlevel of
    1:begin
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B,255);
//        Bar(x-4,y,2,1,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x-4,y);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x-3,y);
//        Bar(x,y-4,1,2,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y-4);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y-3);
//        Bar(x+3,y,2,1,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x+3,y);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x+4,y);
//        Bar(x,y+3,1,2,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y+3);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y+4);
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B,255);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x-2,y);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y-2);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x+2,y);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y+2);
        SDL_SetRenderDrawColor(
          PrimaryWindow.Renderer,
          MainImage.Palette.ColorR[ActiveColorIndex],
          MainImage.Palette.ColorG[ActiveColorIndex],
          MainImage.Palette.ColorB[ActiveColorIndex],
          MainImage.Palette.ColorA[ActiveColorIndex]);
        SDL_RenderDrawPoint(PrimaryWindow.Renderer,x,y);
      end;
    2:begin
        x-=x mod 2;
        y-=y mod 2;
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B,255);
        r.x:=x-8;r.y:=y;r.w:=4;r.h:=2;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+6;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-8;r.w:=2;r.h:=4;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+6;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B,255);
        r.x:=x-4;r.y:=y;r.w:=2;r.h:=2;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+4;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-4;r.w:=2;r.h:=2;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+4;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        Bar(x,y,2,2,
          MainImage.Palette.ColorR[ActiveColorIndex],
          MainImage.Palette.ColorG[ActiveColorIndex],
          MainImage.Palette.ColorB[ActiveColorIndex],
          MainImage.Palette.ColorA[ActiveColorIndex]);
      end;
    3:begin
        x-=x mod 4;
        y-=y mod 4;
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B,255);
        r.x:=x-16;r.y:=y;r.w:=8;r.h:=4;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+12;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-16;r.w:=4;r.h:=8;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+12;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B,255);
        r.x:=x-8;r.y:=y;r.w:=4;r.h:=4;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+8;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-8;r.w:=4;r.h:=4;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+8;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
{        Bar(x-16,y,8,4,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x,y-16,4,8,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x+12,y,8,4,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x,y+12,4,8,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x-8,y,4,4,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y-8,4,4,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x+8,y,4,4,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y+8,4,4,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);}
        Bar(x,y,4,4,
          MainImage.Palette.ColorR[ActiveColorIndex],
          MainImage.Palette.ColorG[ActiveColorIndex],
          MainImage.Palette.ColorB[ActiveColorIndex],
          MainImage.Palette.ColorA[ActiveColorIndex]);
      end;
    4:begin
        x-=x mod 8;
        y-=y mod 8;
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B,255);
        r.x:=x-32;r.y:=y;r.w:=16;r.h:=8;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+24;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-32;r.w:=8;r.h:=16;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+24;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        SDL_SetRenderDrawColor(PrimaryWindow.Renderer,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B,255);
        r.x:=x-16;r.y:=y;r.w:=8;r.h:=8;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x+16;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.x:=x;r.y:=y-16;r.w:=8;r.h:=8;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
        r.y:=y+16;
        SDL_RenderFillRect(PrimaryWindow.Renderer,@r);
{        Bar(x-32,y,16,8,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x,y-32,8,16,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x+24,y,16,8,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x,y+24,8,16,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x-16,y,8,8,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y-16,8,8,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x+16,y,8,8,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y+16,8,8,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);}
        Bar(x,y,8,8,
          MainImage.Palette.ColorR[ActiveColorIndex],
          MainImage.Palette.ColorG[ActiveColorIndex],
          MainImage.Palette.ColorB[ActiveColorIndex],
          MainImage.Palette.ColorA[ActiveColorIndex]);
      end;
  end;
end;

end.

