{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPShared;

{$mode Delphi}{$H+}

interface

uses GFXManagerUnit, mk_sdl2, ARGBImageUnit, PNGFont2Unit, MKMouse2,
  BDPInfoBar, BDPPalette, BDPMessage, BDPCursor, BDPSettings, BDPToolBase,
  BDPInkBase, BDPRegion, BDPProject, BDPModalDialog, BDPGradient;

const
  WINDOWWIDTH=1280;
  WINDOWHEIGHT=720;
  INFOBARHEIGHT=24;
  CONTROLSHEIGHT=96;
  COORDSWIDTH=111;
  COORDSLEFT=WINDOWWIDTH-COORDSWIDTH;
  COORDSCENTER=COORDSLEFT+COORDSWIDTH div 2;
  MODALDIALOGCAPTIONHEIGHT=21;
  NORMALBUTTONWIDTH=127;
  NORMALBUTTONHEIGHT=27;
  NORMALSLIDERHEIGHT=33;
  SMALLBUTTONWIDTH=35;
  CONTROLUNDOBUTTONSLEFT=3;
  CONTROLUNDOBUTTONSTOP=36;
  CONTROLUNDOBUTTONWIDTH=NORMALBUTTONWIDTH-2*18; // 2 chars less
  TOOLBUTTONSLEFT=CONTROLUNDOBUTTONSLEFT+CONTROLUNDOBUTTONWIDTH+3;
  TOOLBUTTONSTOP=6;
  CONTROLCOLORSELECTORLEFT=TOOLBUTTONSLEFT+2*NORMALBUTTONWIDTH+9;
  CONTROLCOLORSELECTORTOP=6;
  COLORSELECTORBOXSIZE=36;
  COLORSELECTORCOLORS=8;
  COLORSELECTORGAP=12;
  IMAGECOUNTSLIDERHEIGHT=33;
  IMAGECOUNTSLIDERWIDTH=313-33;
  IMAGECOUNTSLIDERLEFT=CONTROLCOLORSELECTORLEFT;
  IMAGECOUNTSLIDERTOP=CONTROLSHEIGHT-IMAGECOUNTSLIDERHEIGHT-3;
  INKBUTTONSLEFT=CONTROLCOLORSELECTORLEFT+IMAGECOUNTSLIDERWIDTH+6;
  INKBUTTONSTOP=6+30;
  CONTROLGRADIENTWIDTH=NORMALBUTTONWIDTH*3+6;

  TOGGLEBUTTONSLEFT=INKBUTTONSLEFT+3*(NORMALBUTTONWIDTH+3);
  TOGGLEBUTTONSTOP=6;
  TOPMENUHEIGHT=8*3;
  SUBMENULINEHEIGHT=24;

  DIALOGSLIDERWIDTH=300;
  DIALOGSLIDERHEIGHT=33;


  MAXPALETTEENTRIES=256;  // Palette color count hard limit
  POSTPROCESSCOLOR=$00FF00FF;

  // ZIndex of DrawArea
  DRAWAREA_ZINDEX=0;
  // ZIndex on Controls, ColorEditor, Menu
  LEVEL1CONTROLS_ZINDEX=100;
  // ZIndex of popup dialogs (ConfirmQuit, etc.)
  MODALDIALOG_ZINDEX=9999;

  SYSTEMCOLORTRANSPARENT=0;
  SYSTEMCOLORBLACK=1;
  SYSTEMCOLORDARK=2;
  SYSTEMCOLORMID=3;
  SYSTEMCOLORLIGHT=4;
  SYSTEMCOLORHIGHLIGHT=5;

  MAXZOOMLEVEL=5;

  TEMPPROJECTFILE:string='temp.bpprj';
  PROJECTBASEPATH:string='.\';
  SETTINGSFILE='BurdockPaint.ini';

  // Message typeID constants

  // ------- System Messages ------- Range: 0-199 -------
  // Don't want to send message but buttons need one? Give them this.
  MSG_NONE=0;

  // ConfirmQUIT dialog finished.
  //   IntValue is 1 if really quit, 0 if not.
  MSG_QUIT=1;

  // Toggle visibility of main Controls panel and MainMenu.
  MSG_TOGGLECONTROLS=2;

  // GETCEL or PUTCEL finished, reactivate selected tool.
  MSG_RESTORECONTROLS=3;

  // Set TOOLS menu item states based on Settings.SelectedTools
  MSG_SETTOOLSMENU=4;

  // Set INKS menu item states based on Settings.SelectedInks
  MSG_SETINKSMENU=5;

  // Set Undo/Redo buttons' state depending on ImageUndosystem state.
//  MSG_SETIMAGEUNDOREDOBUTTON=6;

  // KEY_GETCOLOR pressed, select color value under the cursor (if over drawarea).
  MSG_SELECTCOLOR=7;


  // ActiveColor changed.
  //   UInt32 value is the new color.
  MSG_ACTIVECOLORCHANGED=10;

  // Project active image (or image count) changed. Every control should refresh
  // what is needed.
  MSG_ACTIVEIMAGECHANGED=11;

  // Active gradient changed, refresh gradients.
  //   IntValue is the new gradient index in Project.CurrentGradientList.
  MSG_ACTIVEGRADIENTCHANGED=12;

  // The active palette's one color is changed.
  //   IntValue is the palette index.
  //   UInt32 value is the new color.
  MSG_PALETTECHANGED=13;


  // Show CEL image. Hides Controls and MainMenu then activates SHOWCEL tool.
  MSG_SHOWCEL=20;

  // GetCEL tool finished *successfully*. We should enable menus associated with CEL.
  MSG_GETCELFINISHED=21;

  // Response from MessageBox.
  //   IntValue is the pressed button number.
  MSG_MESSAGEBOXRESP=22;

  // Open ColorEditor dialog.
  //   IntValue is one of the PARM_COL constants. (Where it was called from)
  //   UInt32 value is the color to set the sliders to.
  MSG_OPENCOLOREDITOR=23;

  // Hide ColorEditor, show Controls.
  //   IntValue is one of the PARM_COL constants. (Where it was called from)
  //   UInt32 value is the selected color, except when it is the POSTPROCESSCOLOR.
  //     In that case no color was selected.
  MSG_COLOREDITORRESP=24;

  // Open DitherDialog.
  MSG_OPENDITHERDIALOG=25;

  // Open GradientSelector.
  MSG_OPENGRADIENTSELECTOR=26;

  // Open ConfigureTintDialog.
  MSG_OPENCONFIGURETINTDIALOG=27;

  // Open ConfigureRGradDialog.
  MSG_OPENCONFIGURERGRADDIALOG=28;

  // Hide controls and select RGradCenter tool.
  MSG_CONFIGRGRADCENTER=29;

  // RGrad configuration "Center" finished. (Data is stored in Settings.TempRGradCenter<X|Y>
  MSG_CONFIGRGRADCENTERRESP=30;

  // Activate GradientEditor
  MSG_OPENGRADIENTEDITOR=31;

  // GradientEditor response
  MSG_GRADIENTEDITORRESP=32;

  // Open ConfigureSoftenDialog.
  MSG_OPENCONFIGURESOFTENDIALOG=33;

  // Open ConfigureCircleDialog.
  MSG_OPENCONFIGURECIRCLEDIALOG=34;


  // ------- Menu Messages ------- Range: 200-299 -------
  {$i includes\menu.inc}


  // PARM_COL constants. They mark places where the ColorEditor was invoked from.
  PARM_COL_SELECTOR_MAIN=1;
  PARM_COL_SELECTOR_LEFT=2;
  PARM_COL_SELECTOR_RIGHT=3;
  PARM_COL_GRADEDIT_LEFT=4;
  PARM_COL_GRADEDIT_RIGHT=5;
  PARM_COL_GRADEDIT_COLOR3=6;
  PARM_COL_GRADEDIT_COLOR4=7;
  PARM_COL_GRADEDIT_COLOR5=8;

  // PARM_GRAD constants. They mark places where the GradientEditor was invoked from.
  PARM_GRAD_SELECTOR=1;   // Message's uint32 parameters holds the gradient index.

var
  MM:TGFXManager;  // MediaManager to hold fonts and internal images
  Settings:TSettings;  // All settings in one place
  InfoBar:TBDInfoBar;  // The information bar on the top of the screen

  SystemPalette:TBDPalette;  // The palette holding system colors
  VibroColors:TBDVibroColors; // The flashing color for helping the Tools
  MessageQueue:TMessageQueue;  // Messaging queue for classes who doesn't know each other
  Cursor:TBDCursor;  // The cursor on drawing area
  ModalOverlay:TMouseObject;  // Darkening layer behind modal dialogs, used to
                              // prevent using other controls while dialog is visible.

  Tools:TBDTools;  // All tools are loaded into this list
  ActiveTool:TBDTool;  // This is the selected tool

  Inks:TBDInks;  // All inks are loaded into this list
  ActiveInk:TBDInk;  // This is the selected ink

  Project:TBDProject;  // The project we are working on
  GradientEditorGradient:TGradient;

  OverlayImage:TBDRegion;  // The image to draw tools' helper lines, etc.

  DrawAreaX,DrawAreaY:integer;  // The coordinates when mouse over drawarea, or -1 when not.
  ColorUnderMouse:uint32;  // Color under mouse. Primarily when over drawarea.

  // Load assets and create shared objects
  procedure LoadAssets;

  // Free assets and shared objects
  procedure FreeAssets;

implementation

uses Classes, SysUtils, MKRFont2Unit, Logger, MKStream, MKToolbox, Font2Unit;

{$i includes\fonts.inc}
{$i includes\burdock.inc}

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
  s:=ArchModern;
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

const
  Knob:string=
    '....x....' +
    '...xxx...' +
    '..xxxxx..' +
    '.xxx.xxx.' +
    'xxx...xxx' +
    'xxx...xxx' +
    'xxx...xxx' +
    'xxx...xxx' +
    'xxx...xxx' +
    'xxx...xxx' +
    'xxx...xxx' +
    'xxx...xxx' +
    'xxxxxxxxx' +
    'xxxxxxxxx' +
    'xxxxxxxxx';

procedure CreateKnob;
var tmpI:TARGBImage;
    x,y:integer;
    c:uint32;
begin
  tmpI:=TARGBImage.Create(9,15);
  for y:=0 to 14 do
    for x:=0 to 8 do begin
      case Knob[x+y*9+1] of
        '.':c:=SystemPalette[3];
        'x':c:=SystemPalette[2];
        ' ':c:=0;
      end;
      tmpI.PutPixel(x,y,c);
    end;
  MM.AddImage(tmpI,'Knob');
end;

procedure LoadSystemFont(pR,pG,pB:integer;pName:string;pFlags:integer);
var Xs:TStream;
begin
  Xs:=TStringStream.Create(bdpfont);
  try
    MM.Fonts.Add(TPNGFont.Create(Xs,pFlags),pName);
    MM.Fonts[pName].LetterSpace:=3;
    MM.Fonts[pName].SpaceSpace:=15;
    MM.Fonts[pName].SetRecolorExcludeChars(#132#133);
    MM.Fonts[pName].SetColorKey(0,0,0);
    MM.Fonts[pName].SetColor(pR,pG,pB);
  finally
    Xs.Free;
  end;
end;

procedure LoadSmallFont(pR,pG,pB:integer;pName:string;pFlags:integer);
var Xs:TStream;
begin
  Xs:=TStringStream.Create(SmallFont);
  try
    MM.Fonts.Add(TPNGFont.Create(Xs,pFlags),pName);
    MM.Fonts[pName].LetterSpace:=2;
    MM.Fonts[pName].SpaceSpace:=12;
    MM.Fonts[pName].SetColorKey(0,0,0);
    MM.Fonts[pName].SetColor(pR,pG,pB);
  finally
    Xs.Free;
  end;
end;

procedure LoadImage(incstring,name:string;pFlags:integer=0);
var Xs:TStream;atm:TARGBImage;
begin
  Xs:=TStringStream.Create(incstring);
  atm:=TARGBImage.Create;
  try
    atm.ReadFile(Xs,'PNG');
    MM.AddImage(atm,name,pFlags);
  finally
    Xs.Free;
  end;
end;

procedure CreateDarkBar;
var atm:TARGBImage;
begin
  atm:=TARGBImage.Create(64,64);
  atm.Bar(0,0,atm.Width,atm.Height,0,0,0,128);
  MM.AddImage(atm,'DarkBar',MM_CREATETEXTUREONLY);
end;

procedure CreateAlphaBack;
const CHECKERSIZE=8;
var atm:TARGBImage;i,j:integer;
begin
  atm:=TARGBImage.Create(CHECKERSIZE*4,CHECKERSIZE*4);
  atm.Bar(0,0,atm.Width,atm.Height,64,64,64,255);
  for j:=0 to atm.Width div CHECKERSIZE-1 do
    for i:=0 to atm.Width div CHECKERSIZE-1 do
      if (i+j) mod 2=0 then
        atm.Bar(i*CHECKERSIZE,j*CHECKERSIZE,CHECKERSIZE,CHECKERSIZE,192,192,192,255);
  MM.AddImage(atm,'AlphaBack',MM_CREATETEXTUREONLY);
end;

procedure LoadAssets;
begin
  Log.LogStatus('Loading and creating assets...');
  MM:=TGFXManager.Create;
  Log.Trace('...'+inttostr(GetHeapStatus.TotalAllocated));
  Log.LogStatus('  Loading fonts...');
  LoadSystemFont(4,4,4,'Black',FONT_CREATE_BOTH);
  LoadSmallFont(4,4,4,'SmallBlack',FONT_CREATE_BOTH);
  LoadSystemFont($c7,4,4,'Red',FONT_CREATE_ARGB);
  LoadSystemFont($40,4,4,'DarkRed',FONT_CREATE_BOTH);
  LoadSmallFont($40,4,4,'SmallDarkRed',FONT_CREATE_BOTH);
  LoadSystemFont($9a,$9a,$9a,'LightGray',FONT_CREATE_ARGB);
  LoadSystemFont($40,$40,$40,'DarkGray',FONT_CREATE_ARGB);
  LoadImage(LogoFont,'LogoFont',MM_DONTCREATETEXTUREFROMFONT);
  MM.Fonts['LogoFont'].SetColorkey(0,0,0);
  LoadImage(BurdockPNG,'Burdock');
  MM.Images.ItemByName['Burdock'].Resize2x;
  Log.Trace('...'+inttostr(GetHeapStatus.TotalAllocated));
  Log.LogStatus('  Creating message queue...');
  MessageQueue:=TMessageQueue.Create(32);
  Log.Trace('...'+inttostr(GetHeapStatus.TotalAllocated));
  Log.LogStatus('  Creating overlay palette...');
  SystemPalette:=TBDPalette.Create(16);
  SystemPalette.Colors[SYSTEMCOLORTRANSPARENT]:=$00000000;
  SystemPalette.Colors[SYSTEMCOLORBLACK]:=$ff040404;
  SystemPalette.Colors[SYSTEMCOLORDARK]:=$ff5d5d5d;
  SystemPalette.Colors[SYSTEMCOLORMID]:=$ff9a9a9a;
  SystemPalette.Colors[SYSTEMCOLORLIGHT]:=$ffc7c7c7;
  SystemPalette.Colors[SYSTEMCOLORHIGHLIGHT]:=$ffc70404;
  OverlayImage:=nil;
  Log.Trace('...'+inttostr(GetHeapStatus.TotalAllocated));
  Log.LogStatus('  Creating information bar...');
  InfoBar:=TBDInfoBar.Create;
  Log.Trace('...'+inttostr(GetHeapStatus.TotalAllocated));
  Log.LogStatus('  Creating UI gfx...');
  CreateArches;
  Log.Trace('Arches...'+inttostr(GetHeapStatus.TotalAllocated));
  CreateKnob;
  Log.Trace('Knob...'+inttostr(GetHeapStatus.TotalAllocated));
  CreateDarkBar;
  Log.Trace('DarkBar...'+inttostr(GetHeapStatus.TotalAllocated));
  CreateAlphaBack;
  Log.Trace('Checkered back...'+inttostr(GetHeapStatus.TotalAllocated));
  ModalOverlay:=TBDModalOverlay.Create;
  MouseObjects.Add(ModalOverlay);
  Log.Trace('ModalOverlay...'+inttostr(GetHeapStatus.TotalAllocated));
  Log.LogStatus('  Creating cursor...');
  Cursor:=TBDCursor.Create;
  VibroColors:=TBDVibroColors.Create($FF202020,$FFD0D0D0);
  Log.Trace('...'+inttostr(GetHeapStatus.TotalAllocated));
  Log.LogStatus('  Creating inks...');
  Inks:=TBDInks.Create;
  Log.Trace('...'+inttostr(GetHeapStatus.TotalAllocated));
  Log.LogStatus('  Creating tools...');
  Tools:=TBDTools.Create;
  Log.Trace('...'+inttostr(GetHeapStatus.TotalAllocated));
  Log.LogStatus('  Creating GradientEditor helper...');
  GradientEditorGradient:=TGradient.Create($ff000000,$ffffffff);
  Log.Trace('...'+inttostr(GetHeapStatus.TotalAllocated));
  if FileExists(TEMPPROJECTFILE) then begin
    Log.LogStatus(Format('Loading previous project (%s)...',[TEMPPROJECTFILE]));
    Project:=TBDProject.CreateFromFile(TEMPPROJECTFILE)
  end else begin
    Log.LogStatus('Creating new project...');
    Project:=TBDProject.Create;
  end;
  Log.Trace('...'+inttostr(GetHeapStatus.TotalAllocated));
//  MessageQueue.AddMessage(MSG_SETIMAGEUNDOREDOBUTTON);
//  MessageQueue.AddMessage(MSG_SETPALETTEUNDOREDOBUTTON);

end;

procedure FreeAssets;
begin
  if Assigned(Project) then begin
    Project.SaveToFile(TEMPPROJECTFILE);
    Project.Free;
  end;
  if Assigned(GradientEditorGradient) then GradientEditorGradient.Free;
  if Assigned(Tools) then Tools.Free;
  if Assigned(Inks) then Inks.Free;
  if Assigned(VibroColors) then VibroColors.Free;
  if Assigned(Cursor) then Cursor.Free;
  if Assigned(ModalOverlay) then ModalOverlay.Free;
  if Assigned(SystemPalette) then SystemPalette.Free;
  if Assigned(MessageQueue) then MessageQueue.Free;
  if Assigned(InfoBar) then InfoBar.Free;
  if Assigned(MM) then MM.Free;
end;

end.

