unit BDPSharedUnit;

{$mode Delphi}{$H+}

interface

uses MediaManagerUnit, mk_sdl2, ARGBImageUnit, BDPInfoBarUnit, BDPImageUnit,
  BDPSettingsUnit, BDPMessageUnit, BDPCursorUnit{, BDPToolsUnit}, BDPInksUnit,
  BDPPaletteUnit;

type
  TSystemColor=record
    r,g,b:byte;c32:uint32;
  end;

  TColorCluster=record
    startindex,endindex:integer;
  end;

const
  WINDOWWIDTH=1280;
  WINDOWHEIGHT=720;
  CONTROLSHEIGHT=96;
  NORMALBUTTONWIDTH=127;
  SMALLBUTTONWIDTH=27;
  MAXPALETTEENTRIES=2048;  // Palette color count hard limit
  POSTPROCESSCOLOR=$FFF0;

  SYSTEMCOLORCOUNT=5;
  SYSTEMCOLORS:array[0..SYSTEMCOLORCOUNT-1] of TSystemColor=
      ((r:  4; g:  4; b:  4; c32:$ff040404),
       (r: 93; g: 93; b: 93; c32:$ff5d5d5d),
       (r:154; g:154; b:154; c32:$ff9a9a9a),
       (r:215; g:215; b:215; c32:$ffc7c7c7),
       (r:215; g:  4; b:  4; c32:$ffc70404));
  TEMPIMAGEFILE='temp.bdp';
  SETTINGSFILE='BurdockPaint.ini';
  SYSTEMPALETTEFILE='system.bdpp';

  // Message typeID constants
  MSG_TOGGLECONTROLS=1;

var
  MM:TMediaManager;  // MediaManager to hold fonts and internal images
  InfoBar:TBDInfoBar;  // The information bar on the top of the screen
  MainImage:TBDImage;  // The image we are working on
  OverlayImage:TBDImage;  // The image where the tools draw its things
  Settings:TSettings;  // All settings in one place
  MessageQueue:TMessageQueue;  // Messaging queue for classes who doesn't know each other
  Cursor:TBDCursor;  // The cursor on drawing area

//  Tools:TBDTools;  // All tools are loaded into this list
//  ActiveTool:TBDTool;  // This is the selected tool

  Inks:TBDInks;  // All inks are loaded into this list
  ActiveInk:TBDInk;  // This is the selected ink

  FillShapes:boolean;  // Fill shapes (if applicable?)

  ActiveColorIndex:integer;  // The selected color index
  ActiveCluster:TColorCluster;  // The selected color cluster
  SystemPalette:TBDPalette;

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
  Arch='.....xxx'+
       '...xxxxx'+
       '..xxxxxx'+
       '.xxxxxx '+
       '.xxxx   '+
       'xxxx    '+
       'xxxx    '+
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
      '.':c:=SystemColors[2].c32;
      'x':c:=SystemColors[1].c32;
      ' ':c:=0;
      end;
      fLeftImage.PutPixel(x,y,c);
      fLeftImage.PutPixel(x,26-y,c);
      fRightImage.PutPixel(7-x,y,c);
      fRightImage.PutPixel(7-x,26-y,c);
    end;
  fLeftImage.Bar(0,8,3,11,SystemColors[1].c32);
  fRightImage.Bar(5,8,3,11,SystemColors[1].c32);
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
  LoadSystemFont($ee,$ee,$ee,'White');
  LoadSystemFont($ee,$aa,$cc,'Pinky');
  LoadSystemFont($5d,$5d,$5d,'DarkGray');
  Log.LogStatus('  Loading system palette...');
  SystemPalette:=TBDPalette.Create(5);
  if MKStreamOpener.FileExists(SYSTEMPALETTEFILE) then
    SystemPalette.LoadFromFile(SYSTEMPALETTEFILE)
  else begin
    SystemPalette.Colors[0]:=$ff040404;
    SystemPalette.Colors[1]:=$ff5d5d5d;
    SystemPalette.Colors[2]:=$ff9a9a9a;
    SystemPalette.Colors[3]:=$ffc7c7c7;
    SystemPalette.Colors[4]:=$ffc70404;
  end;
  Log.LogStatus('  Creating button gfx...');
  CreateButtonGFX;
  Log.LogStatus('  Creating information bar...');
  InfoBar:=TBDInfoBar.Create;
  Log.LogStatus('  Creating message queue...');
  MessageQueue:=TMessageQueue.Create(32);
  Log.LogStatus('  Creating main image...');
  MainImage:=TBDImage.Create(320,200);
  MainImage.Palette.LoadCOL('ntsc.col',0);
  for i:=1 to 15 do
    MainImage.Circle(i*20,random(160)+20,random(10)+15,i);
  if FileExists(TEMPIMAGEFILE) then MainImage.LoadFromFile(TEMPIMAGEFILE);
  Log.LogStatus('  Creating cursor...');
  Cursor:=TBDCursor.Create;
  Log.LogStatus('Loading settings...');
  Settings:=TSettings.Create;
  Settings.LoadFromFile(SETTINGSFILE);
end;

procedure FreeAssets;
begin
  if Assigned(Settings) then begin
    Settings.SaveToFile(SETTINGSFILE);
    FreeAndNil(Settings);
  end;
  if Assigned(Cursor) then FreeAndNil(Cursor);
  if Assigned(MainImage) then begin
    MainImage.SaveToFile(TEMPIMAGEFILE);
    FreeAndNil(MainImage);
  end;
  if Assigned(MessageQueue) then FreeAndNil(MessageQueue);
  if Assigned(InfoBar) then FreeAndNil(InfoBar);
  if Assigned(SystemPalette) then begin
    SystemPalette.SaveToFile(SYSTEMPALETTEFILE);
    FreeAndNil(SystemPalette);
  end;
  if Assigned(MM) then FreeAndNil(MM);
end;

end.

