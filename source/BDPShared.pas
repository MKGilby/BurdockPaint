unit BDPShared;

{$mode Delphi}{$H+}

interface

uses GFXManagerUnit, mk_sdl2, ARGBImageUnit, BDPInfoBar, BDPImage,
  BDPSettings, BDPMessage, BDPCursor, BDPTools, BDPInks,
  BDPPalette, PNGFont2Unit, BDPProject;

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
  CONTROLUNDOBUTTONSLEFT=3;
  CONTROLUNDOBUTTONSTOP=36;
  TOOLBUTTONSLEFT=CONTROLUNDOBUTTONSLEFT+NORMALBUTTONWIDTH+3;
  TOOLBUTTONSTOP=6;
  INKBUTTONSLEFT=720;
  INKBUTTONSTOP=6;
  COLORSELECTORLEFT=TOOLBUTTONSLEFT+2*NORMALBUTTONWIDTH+12;
  COLORSELECTORTOP=6;
  COLORSELECTORBOXSIZE=36;
  COLORSELECTORCOLORS=9;
  COLORSELECTORGAP=12;
  IMAGECOUNTSLIDERHEIGHT=33;
  IMAGESCOUNTLIDERWIDTH=313;
  IMAGECOUNTSLIDERLEFT=COLORSELECTORLEFT;
  IMAGECOUNTSLIDERTOP=CONTROLSHEIGHT-IMAGECOUNTSLIDERHEIGHT-3;

  TOGGLEBUTTONSLEFT=INKBUTTONSLEFT+2*(NORMALBUTTONWIDTH+3);
  TOGGLEBUTTONSTOP=6;
  TOPMENUHEIGHT=8*3;
  SUBMENULINEHEIGHT=24;

  PALETTEEDITORHEIGHT=294;
  PALETTESOCKETWIDTH=38;
  PALETTESOCKETHEIGHT=26;
  PALETTESOCKETSTOP=PALETTEEDITORHEIGHT-213;
  PALETTESOCKETSLEFT=3;
  PALETTEUNDOBUTTONSLEFT=3;
  PALETTEUNDOBUTTONSTOP=6;
  PALETTEUNDOBUTTONHEIGHT=36;
  PALETTEREDOBUTTONHEIGHT=33;
  PALETTECOLORSELECTORLEFT=PALETTEUNDOBUTTONSLEFT+NORMALBUTTONWIDTH+3+COLORSELECTORGAP;
  COLORSLIDERWIDTH=240;
  COLORSLIDERHEIGHT=33;
  COLORSLIDERSLEFT=PALETTEUNDOBUTTONSLEFT+NORMALBUTTONWIDTH+3;
  COLORSLIDERSTOP=PALETTESOCKETSTOP-COLORSLIDERHEIGHT-3;
  COLORBOXWIDTH=72;
  COLORBOXHEIGHT=72;
  COLORBOXLEFT=WINDOWWIDTH-COLORBOXWIDTH-3;
  COLORBOXTOP=6;

  COLORCLUSTERLEFT=720;
  COLORCLUSTERBASEWIDTH=240;    // The real visible color cluster
  COLORCLUSTERHEIGHT=36;
  COLORCLUSTERWIDTH=6+COLORCLUSTERBASEWIDTH+COLORCLUSTERHEIGHT;

  MAXPALETTEENTRIES=2048;  // Palette color count hard limit
  POSTPROCESSCOLOR=$FFF0;

  // ZIndex of DrawArea
  DRAWAREA_ZINDEX=0;
  // ZIndex on Controls, PaletteEditor, Menu
  LEVEL1CONTROLS_ZINDEX=100;
  // ZIndex of popup dialogs (ConfirmQuit, etc.)
  MODALDIALOG_ZINDEX=9999;

  MAXZOOMLEVEL=5;


  DATAFILE='BurdockPaint.data';
  TEMPPROJECTFILE='temp.bpprj';
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
  // Response from Magnify CEL dialog. Data contains magnification or 0 if cancelled.
  MSG_MAGNIFYCELRESP=7;
  // Response from Rotate CEL dialog. Rotate data*90 degrees clockwise.
  MSG_ROTATECELRESP=8;
  // Response from About dialog.
  MSG_ABOUTRESP=9;
  // Response from MessageBox. Data is the pressed button number.
  MSG_MESSAGEBOXRESP=10;

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
  // Project image count changed. Used to refresh Controls' slider.
  MSG_PROJECTIMAGECOUNTCHANGED=106;

  // ------- Menu Messages ------- Range: 200-299 -------
  {$i MenuMessages.inc}

var
  MM:TGFXManager;  // MediaManager to hold fonts and internal images
  InfoBar:TBDInfoBar;  // The information bar on the top of the screen

  Project:TBDProject;  // The project we are working on

  SystemPalette:TBDPalette;  // The palette holding system colors

  CELHelperImage:TBDImage;  // Helper image for PUTCel
  Settings:TSettings;  // All settings in one place
  MessageQueue:TMessageQueue;  // Messaging queue for classes who doesn't know each other
  Cursor:TBDCursor;  // The cursor on drawing area
  VibroColors:TBDVibroColors; // The flashing color for helping the Tools

  Tools:TBDTools;  // All tools are loaded into this list
  ActiveTool:TBDTool;  // This is the selected tool

  Inks:TBDInks;  // All inks are loaded into this list
  ActiveInk:TBDInk;  // This is the selected ink

  ActiveColorClusterIndex:integer;

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
        '.':c:=SystemPalette[3];
        'x':c:=SystemPalette[2];
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

procedure LoadAssets;
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
  Log.LogStatus('  Creating overlay palette...');
  SystemPalette:=TBDPalette.Create(16);
  SystemPalette.Colors[0]:=$00000000;
  SystemPalette.Colors[1]:=$ff040404;
  SystemPalette.Colors[2]:=$ff5d5d5d;
  SystemPalette.Colors[3]:=$ff9a9a9a;
  SystemPalette.Colors[4]:=$ffc7c7c7;
  SystemPalette.Colors[5]:=$ffc70404;
  SystemPalette.Colors[6]:=$ff202020;
  SystemPalette.Colors[7]:=$ff505050;
  SystemPalette.Colors[8]:=$ff808080;
  SystemPalette.Colors[9]:=$ffb0b0b0;
  SystemPalette.Colors[10]:=$ffe0e0e0;
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
  ActiveColorClusterIndex:=0;
  Log.LogStatus('Loading previous session data...');
  if FileExists(TEMPPROJECTFILE) then
    Project:=TBDProject.CreateFromFile(TEMPPROJECTFILE)
  else
    Project:=TBDProject.Create;
  MessageQueue.AddMessage(MSG_SETIMAGEUNDOREDOBUTTON);
  MessageQueue.AddMessage(MSG_SETPALETTEUNDOREDOBUTTON);

end;

procedure FreeAssets;
begin
  if Assigned(Project) then begin
    Project.SaveToFile(TEMPPROJECTFILE);
    FreeAndNil(Project);
  end;
  if Assigned(CELHelperImage) then FreeAndNil(CELHelperImage);
  if Assigned(Settings) then begin
    Settings.SaveToFile(SETTINGSFILE);
    FreeAndNil(Settings);
  end;
  if Assigned(Tools) then FreeAndNil(Tools);
  if Assigned(Inks) then FreeAndNil(Inks);
  if Assigned(VibroColors) then FreeAndNil(VibroColors);
  if Assigned(Cursor) then FreeAndNil(Cursor);
  if Assigned(SystemPalette) then FreeAndNil(SystemPalette);
  if Assigned(MessageQueue) then FreeAndNil(MessageQueue);
  if Assigned(InfoBar) then FreeAndNil(InfoBar);
  if Assigned(MM) then FreeAndNil(MM);
end;

end.

