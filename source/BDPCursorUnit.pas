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
begin
  case zoomlevel of
    1:begin
        Bar(x-4,y,2,1,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x,y-4,1,2,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x+3,y,2,1,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x,y+3,1,2,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
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
        Bar(x-8,y,4,2,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x,y-8,2,4,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x+6,y,4,2,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x,y+6,2,4,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x-4,y,2,2,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y-4,2,2,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x+4,y,2,2,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y+4,2,2,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y,2,2,
          MainImage.Palette.ColorR[ActiveColorIndex],
          MainImage.Palette.ColorG[ActiveColorIndex],
          MainImage.Palette.ColorB[ActiveColorIndex],
          MainImage.Palette.ColorA[ActiveColorIndex]);
      end;
    3:begin
        x-=x mod 4;
        y-=y mod 4;
        Bar(x-16,y,8,4,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x,y-16,4,8,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x+12,y,8,4,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x,y+12,4,8,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x-8,y,4,4,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y-8,4,4,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x+8,y,4,4,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y+8,4,4,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y,4,4,
          MainImage.Palette.ColorR[ActiveColorIndex],
          MainImage.Palette.ColorG[ActiveColorIndex],
          MainImage.Palette.ColorB[ActiveColorIndex],
          MainImage.Palette.ColorA[ActiveColorIndex]);
      end;
    4:begin
        x-=x mod 8;
        y-=y mod 8;
        Bar(x-32,y,16,8,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x,y-32,8,16,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x+24,y,16,8,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x,y+24,8,16,SystemColors[3].R,SystemColors[3].G,SystemColors[3].B);
        Bar(x-16,y,8,8,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y-16,8,8,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x+16,y,8,8,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y+16,8,8,SystemColors[0].R,SystemColors[0].G,SystemColors[0].B);
        Bar(x,y,8,8,
          MainImage.Palette.ColorR[ActiveColorIndex],
          MainImage.Palette.ColorG[ActiveColorIndex],
          MainImage.Palette.ColorB[ActiveColorIndex],
          MainImage.Palette.ColorA[ActiveColorIndex]);
      end;
  end;
end;

end.

