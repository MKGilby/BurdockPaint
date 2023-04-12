unit BDPSharedUnit;

{$mode Delphi}{$H+}

interface

uses GFXManagerUnit, mk_sdl2, ARGBImageUnit, BDPInfoBarUnit, BDPImageUnit,
  BDPSettingsUnit, BDPMessageUnit, BDPCursorUnit, BDPToolsUnit, BDPInksUnit,
  BDPPaletteUnit, BDPUndoUnit, PNGFont2Unit, BDPColorClusterUnit, BDPProjectUnit;

const
  WINDOWWIDTH=1280;
  WINDOWHEIGHT=720;
  INFOBARHEIGHT=24;
  CONTROLSHEIGHT=96;
  COORDSWIDTH=111;
  COORDSLEFT=WINDOWWIDTH-COORDSWIDTH;
  COORDSCENTER=COORDSLEFT+COORDSWIDTH div 2;
  NORMALBUTTONWIDTH=127;
  NORMALBUTTONHEIGHT=27;
  SMALLBUTTONWIDTH=35;
  UNDOBUTTONSLEFT=3;
  UNDOBUTTONSTOP=36;
  TOOLBUTTONSLEFT=UNDOBUTTONSLEFT+NORMALBUTTONWIDTH+3;
  TOOLBUTTONSTOP=6;
  INKBUTTONSLEFT=720;
  INKBUTTONSTOP=6;
  COLORSELECTORLEFT=TOOLBUTTONSLEFT+2*NORMALBUTTONWIDTH+12;
  COLORSELECTORTOP=6;
  COLORSELECTORBOXSIZE=36;
  COLORSELECTORCOLORS=8;
  COLORSELECTORGAP=12;
  TOGGLEBUTTONSLEFT=INKBUTTONSLEFT+2*(NORMALBUTTONWIDTH+3);
  TOGGLEBUTTONSTOP=6;
  TOPMENUHEIGHT=8*3;
  SUBMENULINEHEIGHT=24;

  PALETTEEDITORHEIGHT=294;
  PALETTESOCKETWIDTH=38;
  PALETTESOCKETHEIGHT=26;
  PALETTESOCKETSTOP=PALETTEEDITORHEIGHT-213;
  PALETTESOCKETSLEFT=3;
  COLORSLIDERWIDTH=240;
  COLORSLIDERHEIGHT=33;
  COLORSLIDERSLEFT=200;
  COLORSLIDERSTOP=PALETTESOCKETSTOP-COLORSLIDERHEIGHT-3;
  COLORBOXWIDTH=72;
  COLORBOXHEIGHT=72;
  COLORBOXLEFT=WINDOWWIDTH-COLORBOXWIDTH-3;
  COLORBOXTOP=6;

  COLORCLUSTERWIDTH=240;
  COLORCLUSTERHEIGHT=33;

  MAXPALETTEENTRIES=2048;  // Palette color count hard limit
  POSTPROCESSCOLOR=$FFF0;

  // ZIndex of DrawArea
  DRAWAREA_ZINDEX=0;
  // ZIndex on Controls, PaletteEditor, Menu
  LEVEL1CONTROLS_ZINDEX=100;
  // ZIndex of popup dialogs (ConfirmQuit, etc.)
  MODALDIALOG_ZINDEX=9999;


  DATAFILE='BurdockPaint.data';
  STATEFILE='state.bps';
  SETTINGSFILE='BurdockPaint.ini';
  STATEDATAID=$53;

  // Message typeID constants

  // ------- System Messages ------- Range: 0-99 -------
  // Don't want to send message but buttons need one? Give them this.
  MSG_NONE=0;
  // ConfirmQUIT dialog finished, IntValue is 1 if really quit, 0 if not.
  MSG_QUIT=1;
  // Toggle visibility of main Controls panel and MainMenu.
  MSG_TOGGLECONTROLS=2;
  // Hide Controls, show PaletteEditor
  MSG_ACTIVATEPALETTEEDITOR=3;
  // Hide PaletteEditor, show Controls and MainMenu
  MSG_DEACTIVATEPALETTEEDITOR=4;
  // Set Undo/Redo buttons' state depending on ImageUndosystem state.
  MSG_SETIMAGEUNDOREDOBUTTON=5;
  // Set Undo/Redo buttons' state depending on PaletteUndosystem state.
  MSG_SETPALETTEUNDOREDOBUTTON=6;

  // ------- Tool Messages ------- Range: 100-199 -------
  // PICKCOL finished, IntValue holds the selected color index or -1 if no color selected.
  MSG_PICKEDCOLOR=100;
  // GETCEL or PUTCEL finished, reactivate selected tool.
  MSG_RESTORECONTROLS=101;
  // ActiveColorIndex changed. Used in PaletteEditor
  MSG_ACTIVECOLORINDEXCHANGED=102;
  // Show CEL image. Hides Controls and MainMenu then activates SHOWCEL tool.
  MSG_SHOWCEL=103;
  // GetCEL tool finished *successfully*. We should enable menus associated with CEL.
  MSG_GETCELFINISHED=104;
  // KEY_GETCOLOR pressed, select color value under the cursor (if over drawarea).
  MSG_SELECTCOLOR=105;

  // ------- Menu Messages ------- Range: 200-299 -------
  // Magnify CEL image. A dialog window appears where you can select beetween
  // 2x 3x and 5x magnification and can cancel the whole operation. Menu:CEL/Magnify
  MSG_OPENMAGNIFYCELDIALOG=200;
  // Response from Magnify CEL dialog. Data contains magnification or 0 if cancelled.
  MSG_MAGNIFYCEL=201;
  // Rotate CEL 90, 180 or 270 degrees clockwise. A dialog window appears... Menu:CEL/Rotate
  MSG_OPENROTATECELDIALOG=202;
  // Response from Rotate CEL dialog. Rotate data*90 degrees clockwise.
  MSG_ROTATECEL=203;
  // Clear image to key color. Menu:Picture/Clear
  MSG_CLEARPICTURE=204;
  // Release CEL image. Menu:CEL/Release
  MSG_RELEASECEL=205;
  // Get CEL image. Menu:CEL/Get
  MSG_GETCEL=206;
  // Put CEL image. Menu:CEL/Put
  MSG_PUTCEL=207;
  // Load CEL file. Menu:CEL/Load
  MSG_LOADCEL=208;
  // Save CEL image. Menu:CEL/Save
  MSG_SAVECEL=209;
  // Flip the CEL. 0 - Vertical, 1 - Horizontal. Menu:CEL/FLIP V and /FLIP H
  MSG_FLIPCEL=210;

var
  MM:TGFXManager;  // MediaManager to hold fonts and internal images
  InfoBar:TBDInfoBar;  // The information bar on the top of the screen
  MainImage:TBDImage;  // The image we are working on
  OverlayImage:TBDImage;  // The image where the tools draw its things
  CELImage:TBDImage;  // The "clipboard" of image
  CELHelperImage:TBDImage;  // Helper image for PUTCel
  Settings:TSettings;  // All settings in one place
  MessageQueue:TMessageQueue;  // Messaging queue for classes who doesn't know each other
  Cursor:TBDCursor;  // The cursor on drawing area
  VibroColors:TBDVibroColors; // The flashing color for helping the Tools

  Tools:TBDTools;  // All tools are loaded into this list
  ActiveTool:TBDTool;  // This is the selected tool

  Inks:TBDInks;  // All inks are loaded into this list
  ActiveInk:TBDInk;  // This is the selected ink

  ImageUndoSystem:TBDImageUndoSystem;  // Handles undo and redo things for Images
  PaletteUndoSystem:TBDPaletteUndoSystem;  // Handles undo and redo things for Palettes

  ColorClusters:TColorClusters;
  ActiveColorClusterIndex:integer;
//  ActiveCluster:TColorCluster;  // The selected color cluster

  // Load assets and create shared objects
  procedure LoadAssets;

  // Free assets and shared objects
  procedure FreeAssets;

implementation

uses Classes, SysUtils, MKRFont2Unit, Logger, MKStream;

const
  ArchModern=
    '.....xxx'+
    '...xxxxx'+
    '..xxxxxx'+
    '.xxxxxx '+
    '.xxxx   '+
    'xxxx    '+
    'xxxx    '+
    'xxx     ';
  ArchOriginal=
    '......xx'+
    '......xx'+
    '......xx'+
    '...xxx  '+
    '...xxx  '+
    '...xxx  '+
    'xxx     '+
    'xxx     ';

procedure CreateArches;
var x,y:integer;c:uint32;TLImage,TRImage,BLImage,BRImage:TARGBImage;s:string;
begin
  TLImage:=TARGBImage.Create(8,8);
  TLImage.Bar(0,0,TLImage.Width,TLImage.Height,0,0,0,0);
  TRImage:=TARGBImage.Create(8,8);
  TRImage.Bar(0,0,TRImage.Width,TRImage.Height,0,0,0,0);
  BLImage:=TARGBImage.Create(8,8);
  BLImage.Bar(0,0,BLImage.Width,BLImage.Height,0,0,0,0);
  BRImage:=TARGBImage.Create(8,8);
  BRImage.Bar(0,0,BRImage.Width,BRImage.Height,0,0,0,0);
  if Settings.ModernGraphics then s:=ArchModern else s:=ArchOriginal;
  c:=0;
  for y:=0 to 7 do
    for x:=0 to 7 do begin
      case s[x+y*8+1] of
        '.':c:=OverlayImage.Palette[3];
        'x':c:=OverlayImage.Palette[2];
        ' ':c:=0;
      end;
      TLImage.PutPixel(x,y,c);
      TRImage.PutPixel(7-x,y,c);
      BLImage.PutPixel(x,7-y,c);
      BRImage.PutPixel(7-x,7-y,c);
    end;
  MM.AddImage(TLImage,'ArchTopLeft');
  MM.AddImage(TRImage,'ArchTopRight');
  MM.AddImage(BLImage,'ArchBottomLeft');
  MM.AddImage(BRImage,'ArchBottomRight');
  // Don't free images, MM will do that!
end;

procedure LoadSystemFont(pR,pG,pB:integer;pName:string);
begin
  if Settings.ModernGraphics then begin
    MM.Fonts.Add(TPNGFont.Create('bdpfont.png'),pName);
    MM.Fonts[pName].LetterSpace:=3;
    MM.Fonts[pName].SpaceSpace:=15;
  end else begin
    MM.Fonts.Add(TMKRFont.Create('system.mkr'),pName);
    MM.Fonts[pName].Size:=3;
    MM.Fonts[pName].LetterSpace:=1;
    MM.Fonts[pName].SpaceSpace:=5;
  end;
  MM.Fonts[pName].SetRecolorExcludeChars(#132#133);
  MM.Fonts[pName].SetColorKey(0,0,0);
  MM.Fonts[pName].SetColor(pR,pG,pB);
end;

procedure LoadStateV1(pStream:TStream);
var flags:byte;
begin
  flags:=0;
  pStream.Read(flags,1);
  MainImage.LoadFromStream(pStream);
  if flags and 1>0 then begin
    CELImage:=TBDImage.Create(16,16);
    CELImage.LoadFromStream(pStream);
  end;
  if flags and 2>0 then ImageUndoSystem.LoadFromStream(pStream);
  if flags and 4>0 then PaletteUndoSystem.LoadFromStream(pStream);
end;

procedure LoadStateV2(pStream:TStream);
var flags:byte;
begin
  flags:=0;
  pStream.Read(flags,1);
  MainImage.LoadFromStream(pStream);
  if flags and 1>0 then begin
    CELImage:=TBDImage.Create(16,16);
    CELImage.LoadFromStream(pStream);
  end;
  if flags and 2>0 then ImageUndoSystem.LoadFromStream(pStream);
  if flags and 4>0 then PaletteUndoSystem.LoadFromStream(pStream);
  if flags and 8>0 then ColorClusters.LoadFromStream(pStream);
end;

procedure LoadState;
var size:int64;b:byte;State:TStream;
begin
  if not FileExists(STATEFILE) then exit;
  State:=TFileStream.Create(STATEFILE,fmOpenRead or fmShareDenyNone);
  try
    b:=0;
    State.Read(b,1);
    if b<>STATEDATAID then raise Exception.Create(Format('ID is not for System state data! (%.2x)',[b]));
    size:=0;
    State.Read(Size,4);
    State.Read(b,1);
    if b=1 then LoadStateV1(State)
    else if b=2 then LoadStateV2(State)
    else raise Exception.Create(Format('Unknown system state version! (%d)',[b]));
  finally
    FreeAndNil(State);
  end;
end;

procedure LoadAssets;
var i:integer;
begin
  Log.LogStatus('Loading settings...');
  Settings:=TSettings.Create;
  Settings.LoadFromFile(SETTINGSFILE);
  Log.LogStatus('Loading and creating assets...');
  MM:=TGFXManager.Create;
  Log.LogStatus('  Loading fonts...');
  LoadSystemFont(4,4,4,'Black');
  LoadSystemFont($c7,4,4,'Red');
  LoadSystemFont($40,4,4,'DarkRed');
  LoadSystemFont($ee,$ee,$ee,'White');
  LoadSystemFont($ee,$aa,$cc,'Pinky');
  LoadSystemFont($9a,$9a,$9a,'LightGray');
  LoadSystemFont($40,$40,$40,'DarkGray');
  MM.Load('logofont.png','LogoFont');
  MM.LoadImage('burdock.png','Burdock');
  MM.Images.ItemByName['Burdock'].Resize2x;
  Log.LogStatus('  Creating message queue...');
  MessageQueue:=TMessageQueue.Create(32);
  Log.LogStatus('  Creating main image...');
  MainImage:=TBDImage.Create(320,200);
  MainImage.Palette.LoadCOL('files\ntsc.col',0);
  for i:=1 to 15 do
    MainImage.Circle(i*20,random(160)+20,random(10)+15,i);
  Log.LogStatus('  Creating overlay image...');
  OverlayImage:=TBDImage.Create(320,200);
  OverlayImage.Palette.Colors[0]:=$00000000;
  OverlayImage.Palette.Colors[1]:=$ff040404;
  OverlayImage.Palette.Colors[2]:=$ff5d5d5d;
  OverlayImage.Palette.Colors[3]:=$ff9a9a9a;
  OverlayImage.Palette.Colors[4]:=$ffc7c7c7;
  OverlayImage.Palette.Colors[5]:=$ffc70404;
  OverlayImage.Palette.Colors[6]:=$ff202020;
  OverlayImage.Palette.Colors[7]:=$ff505050;
  OverlayImage.Palette.Colors[8]:=$ff808080;
  OverlayImage.Palette.Colors[9]:=$ffb0b0b0;
  OverlayImage.Palette.Colors[10]:=$ffe0e0e0;
  OverlayImage.Bar(0,0,OverlayImage.Width,OverlayImage.Height,0);
  Log.LogStatus('  Creating CEL helper image...');
  CELHelperImage:=TBDImage.Create(320,200);
  CELHelperImage.Bar(0,0,CELHelperImage.Width,CELHelperImage.Height,0);
  Log.LogStatus('  Creating information bar...');
  InfoBar:=TBDInfoBar.Create;
  Log.LogStatus('  Creating UI gfx...');
  CreateArches;
  Log.LogStatus('  Creating cursor...');
  Cursor:=TBDCursor.Create;
  VibroColors:=TBDVibroColors.Create(6,10);
  Log.LogStatus('  Creating inks...');
  Inks:=TBDInks.Create;
  Log.LogStatus('  Creating tools...');
  Tools:=TBDTools.Create;
  Log.LogStatus('  Initializing Undo system...');
  ImageUndoSystem:=TBDImageUndoSystem.Create;
  PaletteUndoSystem:=TBDPaletteUndoSystem.Create;
  MessageQueue.AddMessage(MSG_SETIMAGEUNDOREDOBUTTON);
  MessageQueue.AddMessage(MSG_SETPALETTEUNDOREDOBUTTON);
  Log.LogStatus('  Initializing color clusters...');
  ColorClusters:=TColorClusters.Create;
  ColorClusters.FreeObjects:=true;
  ColorClusters.Add(TColorCluster.Create(96,111));
  ActiveColorClusterIndex:=0;

  Log.LogStatus('Loading previous session data...');
  LoadState;
end;

procedure WriteStateV1;
var i,curr:integer;State:TStream;
begin
  if not (Assigned(MainImage) and Assigned(ImageUndoSystem)) then exit;
  State:=TFileStream.Create(STATEFILE,fmCreate);
  i:=STATEDATAID;
  State.Write(i,1);
  curr:=State.Position;
  i:=0;
  State.Write(i,4);
  i:=1;
  State.Write(i,1);
  i:=0;
  if Assigned(CELImage) then i:=i or 1;
  if ImageUndoSystem.Count>0 then i:=i or 2;
  if PaletteUndoSystem.Count>0 then i:=i or 4;
  State.Write(i,1);
  MainImage.SaveToStream(State);
  if Assigned(CELImage) then CELImage.SaveToStream(State);
  if ImageUndoSystem.Count>0 then ImageUndoSystem.SaveToStream(State);
  if PaletteUndoSystem.Count>0 then PaletteUndoSystem.SaveToStream(State);
  i:=State.Position-curr-4;
  State.Position:=curr;
  State.write(i,4);
  FreeAndNil(State);
end;

procedure WriteState;
var i,curr:integer;State:TStream;
begin
  if not (Assigned(MainImage) and Assigned(ImageUndoSystem)) then exit;
  State:=TFileStream.Create(STATEFILE,fmCreate);
  i:=STATEDATAID;
  State.Write(i,1);
  curr:=State.Position;
  i:=0;
  State.Write(i,4);
  i:=2;
  State.Write(i,1);
  i:=0;
  if Assigned(CELImage) then i:=i or 1;
  if ImageUndoSystem.Count>0 then i:=i or 2;
  if PaletteUndoSystem.Count>0 then i:=i or 4;
  if ColorClusters.Count>0 then i:=i or 8;
  State.Write(i,1);
  MainImage.SaveToStream(State);
  if Assigned(CELImage) then CELImage.SaveToStream(State);
  if ImageUndoSystem.Count>0 then ImageUndoSystem.SaveToStream(State);
  if PaletteUndoSystem.Count>0 then PaletteUndoSystem.SaveToStream(State);
  if ColorClusters.Count>0 then ColorClusters.SaveToStream(State);
  i:=State.Position-curr-4;
  State.Position:=curr;
  State.write(i,4);
  FreeAndNil(State);
end;

procedure FreeAssets;
begin
  WriteState;
  if Assigned(ColorClusters) then FreeAndNil(ColorClusters);
  if Assigned(CELHelperImage) then FreeAndNil(CELHelperImage);
  if Assigned(CELImage) then FreeAndNil(CELImage);
  if Assigned(Settings) then begin
    Settings.SaveToFile(SETTINGSFILE);
    FreeAndNil(Settings);
  end;
  if Assigned(PaletteUndoSystem) then FreeAndNil(PaletteUndoSystem);
  if Assigned(ImageUndoSystem) then FreeAndNil(ImageUndoSystem);
  if Assigned(Tools) then FreeAndNil(Tools);
  if Assigned(Inks) then FreeAndNil(Inks);
  if Assigned(VibroColors) then FreeAndNil(VibroColors);
  if Assigned(Cursor) then FreeAndNil(Cursor);
  if Assigned(OverlayImage) then FreeAndNil(OverlayImage);
  if Assigned(MainImage) then FreeAndNil(MainImage);
  if Assigned(MessageQueue) then FreeAndNil(MessageQueue);
  if Assigned(InfoBar) then FreeAndNil(InfoBar);
  if Assigned(MM) then FreeAndNil(MM);
end;

end.

