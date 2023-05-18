unit BDPKeyMapping;

{$mode Delphi}

interface

uses MKINIFile;

const
  KEYCOUNT=22;

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
  KEY_ZOOMIN1=11;
  KEY_ZOOMIN2=12;
  KEY_ZOOMOUT1=13;
  KEY_ZOOMOUT2=14;
  KEY_CENTER=15;
  KEY_GETCEL=16;
  KEY_PUTCEL=17;
  KEY_GETCOLOR=18;
  KEY_TOGGLEFILLSHAPES=19;
  KEY_TOGGLEKEYCOLOR=20;
  KEY_TOGGLEDITHER=21;

var
  KeyMap:array [0..KEYCOUNT-1] of integer;

procedure LoadKeyMap(INI:TINIFile);
procedure SaveKeyMap(INI:TINIFile);

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
  KeyMap[KEY_ZOOMIN1]:=INI.ReadInteger(KEYMAPSECTION,'ZoomIn1',SDL_SCANCODE_KP_PLUS);
  KeyMap[KEY_ZOOMIN2]:=INI.ReadInteger(KEYMAPSECTION,'ZoomIn2',SDL_SCANCODE_KP_PLUS);
  KeyMap[KEY_ZOOMOUT1]:=INI.ReadInteger(KEYMAPSECTION,'ZoomOut1',SDL_SCANCODE_KP_MINUS);
  KeyMap[KEY_ZOOMOUT2]:=INI.ReadInteger(KEYMAPSECTION,'ZoomOut2',SDL_SCANCODE_KP_MINUS);
  KeyMap[KEY_CENTER]:=INI.ReadInteger(KEYMAPSECTION,'Center',SDL_SCANCODE_C);
  KeyMap[KEY_GETCEL]:=INI.ReadInteger(KEYMAPSECTION,'GetCEL',SDL_SCANCODE_ESCAPE);
  KeyMap[KEY_PUTCEL]:=INI.ReadInteger(KEYMAPSECTION,'PutCEL',SDL_SCANCODE_GRAVE);
  KeyMap[KEY_GETCOLOR]:=INI.ReadInteger(KEYMAPSECTION,'GetColor',SDL_SCANCODE_F1);
  KeyMap[KEY_TOGGLEFILLSHAPES]:=INI.ReadInteger(KEYMAPSECTION,'ToggleFillShapes',SDL_SCANCODE_F2);
  KeyMap[KEY_TOGGLEKEYCOLOR]:=INI.ReadInteger(KEYMAPSECTION,'ToggleKeyColor',SDL_SCANCODE_F3);
  KeyMap[KEY_TOGGLEDITHER]:=INI.ReadInteger(KEYMAPSECTION,'ToggleDither',SDL_SCANCODE_F4);
end;

procedure SaveKeyMap(INI:TINIFile);
begin
  INI.WriteInteger(KEYMAPSECTION,'Quit',KeyMap[KEY_QUIT]);
  INI.WriteInteger(KEYMAPSECTION,'Yes',KeyMap[KEY_YES]);
  INI.WriteInteger(KEYMAPSECTION,'No',KeyMap[KEY_NO]);
  INI.WriteInteger(KEYMAPSECTION,'PanningUp1',KeyMap[KEY_PANNINGUP1]);
  INI.WriteInteger(KEYMAPSECTION,'PanningUp2',KeyMap[KEY_PANNINGUP2]);
  INI.WriteInteger(KEYMAPSECTION,'PanningDown1',KeyMap[KEY_PANNINGDOWN1]);
  INI.WriteInteger(KEYMAPSECTION,'PanningDown2',KeyMap[KEY_PANNINGDOWN2]);
  INI.WriteInteger(KEYMAPSECTION,'PanningLeft1',KeyMap[KEY_PANNINGLEFT1]);
  INI.WriteInteger(KEYMAPSECTION,'PanningLeft2',KeyMap[KEY_PANNINGLEFT2]);
  INI.WriteInteger(KEYMAPSECTION,'PanningRight1',KeyMap[KEY_PANNINGRIGHT1]);
  INI.WriteInteger(KEYMAPSECTION,'PanningRight2',KeyMap[KEY_PANNINGRIGHT2]);
  INI.WriteInteger(KEYMAPSECTION,'ZoomIn1',KeyMap[KEY_ZOOMIN1]);
  INI.WriteInteger(KEYMAPSECTION,'ZoomIn2',KeyMap[KEY_ZOOMIN2]);
  INI.WriteInteger(KEYMAPSECTION,'ZoomOut1',KeyMap[KEY_ZOOMOUT1]);
  INI.WriteInteger(KEYMAPSECTION,'ZoomOut2',KeyMap[KEY_ZOOMOUT2]);
  INI.WriteInteger(KEYMAPSECTION,'Center',KeyMap[KEY_CENTER]);
  INI.WriteInteger(KEYMAPSECTION,'GetCEL',KeyMap[KEY_GETCEL]);
  INI.WriteInteger(KEYMAPSECTION,'PutCEL',KeyMap[KEY_PUTCEL]);
  INI.WriteInteger(KEYMAPSECTION,'GetColor',KeyMap[KEY_GETCOLOR]);
  INI.WriteInteger(KEYMAPSECTION,'ToggleFillShapes',KeyMap[KEY_TOGGLEFILLSHAPES]);
  INI.WriteInteger(KEYMAPSECTION,'ToggleKeyColor',KeyMap[KEY_TOGGLEKEYCOLOR]);
  INI.WriteInteger(KEYMAPSECTION,'ToggleDither',KeyMap[KEY_TOGGLEDITHER]);
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
  KeyMap[KEY_ZOOMIN1]:=SDL_SCANCODE_KP_PLUS;
  KeyMap[KEY_ZOOMIN2]:=SDL_SCANCODE_PAGEUP;
  KeyMap[KEY_ZOOMOUT1]:=SDL_SCANCODE_KP_MINUS;
  KeyMap[KEY_ZOOMOUT2]:=SDL_SCANCODE_PAGEDOWN;
  KeyMap[KEY_CENTER]:=SDL_SCANCODE_C;
  KeyMap[KEY_GETCEL]:=SDL_SCANCODE_ESCAPE;
  KeyMap[KEY_PUTCEL]:=SDL_SCANCODE_GRAVE;
  KeyMap[KEY_GETCOLOR]:=SDL_SCANCODE_F1;
  KeyMap[KEY_TOGGLEFILLSHAPES]:=SDL_SCANCODE_F2;
  KeyMap[KEY_TOGGLEKEYCOLOR]:=SDL_SCANCODE_F3;
  KeyMap[KEY_TOGGLEDITHER]:=SDL_SCANCODE_F4;
end;

initialization
  FillDefaultKeyMap;

end.

