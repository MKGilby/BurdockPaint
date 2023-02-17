unit BDPSharedUnit;

{$mode Delphi}{$H+}

interface

uses MediaManagerUnit, mk_sdl2, ARGBImageUnit, BDPInfoBarUnit, BDPImageUnit,
  BDPSettingsUnit, BDPMessageUnit, BDPCursorUnit, BDPToolsUnit, BDPInksUnit,
  BDPPaletteUnit;

type
  TSystemColor=record
    r,g,b:byte;c32:uint32;
  end;

  TColorCluster=record
    startindex,endindex:integer;
    reverse,pingpong:boolean;
  end;

const
  WINDOWWIDTH=1280;
  WINDOWHEIGHT=720;
  CONTROLSHEIGHT=96;
  COORDSWIDTH=111;
  COORDSLEFT=WINDOWWIDTH-COORDSWIDTH;
  COORDSCENTER=COORDSLEFT+COORDSWIDTH div 2;
  NORMALBUTTONWIDTH=127;
  SMALLBUTTONWIDTH=35;  // 27
  TOOLBUTTONSLEFT=3;
  TOOLBUTTONSTOP=6;
  INKBUTTONSLEFT=643;
  INKBUTTONSTOP=6;
  COLORSELECTORLEFT=TOOLBUTTONSLEFT+2*NORMALBUTTONWIDTH+12;
  COLORSELECTORTOP=6;
  COLORSELECTORBOXSIZE=36;
  COLORSELECTORCOLORS=8;
  COLORSELECTORGAP=12;
  TOGGLEBUTTONSLEFT=INKBUTTONSLEFT+2*(NORMALBUTTONWIDTH+3);
  TOGGLEBUTTONSTOP=6;
  MAXPALETTEENTRIES=2048;  // Palette color count hard limit
  POSTPROCESSCOLOR=$FFF0;

  VIBROCOLORS:array[0..15] of integer=(6,6,7,7,8,8,9,9,10,10,9,9,8,8,7,7);
  TEMPIMAGEFILE='temp.bdp';
  TEMPCELIMAGEFILE='tempcel.bdp';
  SETTINGSFILE='BurdockPaint.ini';

  // Message typeID constants
  MSG_NONE=0;
  MSG_TOGGLECONTROLS=1;
  MSG_ACTIVATETOOL=2;
  MSG_ACTIVATEINK=3;
  MSG_QUIT=4;
  MSG_GETCELFINISHED=5;
  MSG_MOUSECOORDS=6;


var
  MM:TMediaManager;  // MediaManager to hold fonts and internal images
  InfoBar:TBDInfoBar;  // The information bar on the top of the screen
  MainImage:TBDImage;  // The image we are working on
  OverlayImage:TBDImage;  // The image where the tools draw its things
  CELImage:TBDImage;  // The "clipboard" of image
  CELHelperImage:TBDImage;  // Helper image for PUTCel
  Settings:TSettings;  // All settings in one place
  MessageQueue:TMessageQueue;  // Messaging queue for classes who doesn't know each other
  Cursor:TBDCursor;  // The cursor on drawing area

  Tools:TBDTools;  // All tools are loaded into this list
  ActiveTool:TBDTool;  // This is the selected tool

  Inks:TBDInks;  // All inks are loaded into this list
  ActiveInk:TBDInk;  // This is the selected ink

//  ActiveColorIndex:integer;  // The selected color index
  ActiveCluster:TColorCluster;  // The selected color cluster

  // Load assets and create shared objects
  procedure LoadAssets;

  // Free assets and shared objects
  procedure FreeAssets;

implementation

uses SysUtils, MKRFont2Unit, Logger, MKStream;

procedure LoadSystemFont(pR,pG,pB:integer;pName:string);
begin
  MM.Fonts.Add(TMKRFont.Create('system.mkr'),pName);
  MM.Fonts[pName].SetColor(pR,pG,pB);
  MM.Fonts[pName].SetColorKey(0,0,0);
  MM.Fonts[pName].SpaceSpace:=5;
  MM.Fonts[pName].LetterSpace:=1;
  MM.Fonts[pName].Size:=3;
end;

procedure CreateButtonGFX;
const
{  Arch='.....xxx'+
       '...xxxxx'+
       '..xxxxxx'+
       '.xxxxxx '+
       '.xxxx   '+
       'xxxx    '+
       'xxxx    '+
       'xxx     ';}
  Arch='......xx'+
       '......xx'+
       '......xx'+
       '...xxx  '+
       '...xxx  '+
       '...xxx  '+
       'xxx     '+
       'xxx     ';
var x,y:integer;c:uint32;fLeftImage,fRightImage:TARGBImage;
begin
  fLeftImage:=TARGBImage.Create(8,27);
  fLeftImage.Bar(0,0,fLeftImage.Width,fLeftImage.Height,0,0,0,0);
  fRightImage:=TARGBImage.Create(8,27);
  fRightImage.Bar(0,0,fRightImage.Width,fRightImage.Height,0,0,0,0);
  for y:=0 to 7 do
    for x:=0 to 7 do begin
      case Arch[x+y*8+1] of
      '.':c:=OverlayImage.Palette[3];
      'x':c:=OverlayImage.Palette[2];
      ' ':c:=0;
      end;
      fLeftImage.PutPixel(x,y,c);
      fLeftImage.PutPixel(x,26-y,c);
      fRightImage.PutPixel(7-x,y,c);
      fRightImage.PutPixel(7-x,26-y,c);
    end;
  fLeftImage.Bar(0,8,3,11,OverlayImage.Palette[2]);
  fRightImage.Bar(5,8,3,11,OverlayImage.Palette[2]);
  MM.AddImage(fLeftImage,'ButtonLeft');
  MM.AddImage(fRightImage,'ButtonRight');
  // Don't free images, MM will do that!
end;

procedure LoadAssets;
var i:integer;
begin
  Log.LogStatus('Loading and creating assets...');
  MM:=TMediaManager.Create;
  Log.LogStatus('  Loading fonts...');
  LoadSystemFont(4,4,4,'Black');
  LoadSystemFont($c7,4,4,'Red');
  LoadSystemFont($40,4,4,'DarkRed');
  LoadSystemFont($ee,$ee,$ee,'White');
  LoadSystemFont($ee,$aa,$cc,'Pinky');
  LoadSystemFont($40,$40,$40,'DarkGray');
  MM.Load('logofont.png','LogoFont');
  MM.LoadImage('burdock.png','Burdock');
  MM.Images.ItemByName['Burdock'].Resize2x;
  Log.LogStatus('  Creating message queue...');
  MessageQueue:=TMessageQueue.Create(32);
  Log.LogStatus('  Creating main image...');
  MainImage:=TBDImage.Create(320,200);
  MainImage.Palette.LoadCOL('ntsc.col',0);
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
  Log.LogStatus('  Creating button gfx...');
  CreateButtonGFX;
  Log.LogStatus('  Creating cursor...');
  Cursor:=TBDCursor.Create;
  Log.LogStatus('  Creating inks...');
  Inks:=TBDInks.Create;
  Log.LogStatus('  Creating tools...');
  Tools:=TBDTools.Create;
  Log.LogStatus('Loading settings...');
  Settings:=TSettings.Create;
  Settings.LoadFromFile(SETTINGSFILE);

  Log.LogStatus('Loading previous session data...');
  Log.LogStatus('  Image...');
  if FileExists(TEMPIMAGEFILE) then MainImage.LoadFromFile(TEMPIMAGEFILE);
  Log.LogStatus('  CEL...');
  if FileExists(TEMPCELIMAGEFILE) then begin
    CELImage:=TBDImage.Create(16,16);
    CELImage.LoadFromFile(TEMPCELIMAGEFILE);
  end else CELImage:=nil;
//  ActiveInk:=Inks[Settings.ActiveInk];
end;

procedure FreeAssets;
begin
  if Assigned(CELHelperImage) then FreeAndNil(CELHelperImage);
  if Assigned(CELImage) then begin
    CELImage.SaveToFile(TEMPCELIMAGEFILE);
//    CELImage.ExportToPNG('CELtemp.png');
    FreeAndNil(CELImage);
  end;
  if Assigned(Settings) then begin
    Settings.SaveToFile(SETTINGSFILE);
    FreeAndNil(Settings);
  end;
  if Assigned(Tools) then FreeAndNil(Tools);
  if Assigned(Inks) then FreeAndNil(Inks);
  if Assigned(Cursor) then FreeAndNil(Cursor);
  if Assigned(OverlayImage) then FreeAndNil(OverlayImage);
  if Assigned(MainImage) then begin
    MainImage.SaveToFile(TEMPIMAGEFILE);
    FreeAndNil(MainImage);
  end;
  if Assigned(MessageQueue) then FreeAndNil(MessageQueue);
  if Assigned(InfoBar) then FreeAndNil(InfoBar);
  if Assigned(MM) then FreeAndNil(MM);
end;

end.

