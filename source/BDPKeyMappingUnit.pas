unit BDPKeyMappingUnit;

{$mode Delphi}

interface

uses MKINIFile;

const
  KEYCOUNT=16;

  KEY_QUIT=0;
  KEY_YES=1;
  KEY_NO=2;
  KEY_PANNINGUP1=3;
  KEY_PANNINGUP2=4;
  KEY_PANNINGDOWN1=5;
  KEY_PANNINGDOWN2=6;
  KEY_PANNINGLEFT1=7;
  KEY_PANNINGLEFT2=8;
  KEY_PANNINGRIGHT1=9;
  KEY_PANNINGRIGHT2=10;
  KEY_ZOOMIN=11;
  KEY_ZOOMOUT=12;
  KEY_CENTER=13;
  KEY_GETCEL=14;
  KEY_PUTCEL=15;

var
  KeyMap:array [0..KEYCOUNT-1] of integer;

procedure LoadKeyMap(INI:TINIFile);

implementation

uses SDL2;

const
  KEYMAPSECTION='Keymapping';

procedure LoadKeyMap(INI:TINIFile);
begin
  KeyMap[KEY_QUIT]:=INI.ReadInteger(KEYMAPSECTION,'Quit',SDL_SCANCODE_Q);
  KeyMap[KEY_YES]:=INI.ReadInteger(KEYMAPSECTION,'Yes',SDL_SCANCODE_Y);
  KeyMap[KEY_NO]:=INI.ReadInteger(KEYMAPSECTION,'No',SDL_SCANCODE_N);
  KeyMap[KEY_PANNINGUP1]:=INI.ReadInteger(KEYMAPSECTION,'PanningUp1',SDL_SCANCODE_UP);
  KeyMap[KEY_PANNINGUP2]:=INI.ReadInteger(KEYMAPSECTION,'PanningUp2',SDL_SCANCODE_W);
  KeyMap[KEY_PANNINGDOWN1]:=INI.ReadInteger(KEYMAPSECTION,'PanningDown1',SDL_SCANCODE_DOWN);
  KeyMap[KEY_PANNINGDOWN2]:=INI.ReadInteger(KEYMAPSECTION,'PanningDown2',SDL_SCANCODE_S);
  KeyMap[KEY_PANNINGLEFT1]:=INI.ReadInteger(KEYMAPSECTION,'PanningLeft1',SDL_SCANCODE_LEFT);
  KeyMap[KEY_PANNINGLEFT2]:=INI.ReadInteger(KEYMAPSECTION,'PanningLeft2',SDL_SCANCODE_A);
  KeyMap[KEY_PANNINGRIGHT1]:=INI.ReadInteger(KEYMAPSECTION,'PanningRight1',SDL_SCANCODE_RIGHT);
  KeyMap[KEY_PANNINGRIGHT2]:=INI.ReadInteger(KEYMAPSECTION,'PanningRight2',SDL_SCANCODE_D);
  KeyMap[KEY_ZOOMIN]:=INI.ReadInteger(KEYMAPSECTION,'ZoomIn',SDL_SCANCODE_KP_PLUS);
  KeyMap[KEY_ZOOMOUT]:=INI.ReadInteger(KEYMAPSECTION,'ZoomOut',SDL_SCANCODE_KP_MINUS);
  KeyMap[KEY_CENTER]:=INI.ReadInteger(KEYMAPSECTION,'Center',SDL_SCANCODE_C);
  KeyMap[KEY_GETCEL]:=INI.ReadInteger(KEYMAPSECTION,'GetCEL',SDL_SCANCODE_ESCAPE);
  KeyMap[KEY_PUTCEL]:=INI.ReadInteger(KEYMAPSECTION,'PutCEL',SDL_SCANCODE_GRAVE);
end;

procedure FillDefaultKeyMap;
begin
  KeyMap[KEY_QUIT]:=SDL_SCANCODE_Q;
  KeyMap[KEY_YES]:=SDL_SCANCODE_Y;
  KeyMap[KEY_NO]:=SDL_SCANCODE_N;
  KeyMap[KEY_PANNINGUP1]:=SDL_SCANCODE_UP;
  KeyMap[KEY_PANNINGUP2]:=SDL_SCANCODE_W;
  KeyMap[KEY_PANNINGDOWN1]:=SDL_SCANCODE_DOWN;
  KeyMap[KEY_PANNINGDOWN2]:=SDL_SCANCODE_S;
  KeyMap[KEY_PANNINGLEFT1]:=SDL_SCANCODE_LEFT;
  KeyMap[KEY_PANNINGLEFT2]:=SDL_SCANCODE_A;
  KeyMap[KEY_PANNINGRIGHT1]:=SDL_SCANCODE_RIGHT;
  KeyMap[KEY_PANNINGRIGHT2]:=SDL_SCANCODE_D;
  KeyMap[KEY_ZOOMIN]:=SDL_SCANCODE_KP_PLUS;
  KeyMap[KEY_ZOOMOUT]:=SDL_SCANCODE_KP_MINUS;
  KeyMap[KEY_CENTER]:=SDL_SCANCODE_C;
  KeyMap[KEY_GETCEL]:=SDL_SCANCODE_ESCAPE;
  KeyMap[KEY_PUTCEL]:=SDL_SCANCODE_GRAVE;
end;

initialization
  FillDefaultKeyMap;

end.

