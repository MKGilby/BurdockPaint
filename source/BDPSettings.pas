{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPSettings;

{$mode Delphi}

interface

uses
  SysUtils, MKINIFile;

type
  TStringArray8=array[0..7] of string;

  { TSettings }

  TSettings=class
    constructor Create;
    procedure LoadFromFile(pFilename:String);
    procedure SaveToFile(pFilename:String);
  private
    procedure LoadFromFileV0(INI:TINIFile);
    procedure LoadFromFileV1(INI:TINIFile);
  public
    TempInt01,TempInt02:integer;  // Used in dialogs when values don't have to be saved.
    TempBool01:boolean;

  // System settings
  private
    fWindowWidth,fWindowHeight:integer;
    procedure fSetWindowWidth(value:integer);
    procedure fSetWindowHeight(value:integer);
  public
    ShowSplash:Boolean;
    UndoLimit:uint32;
    property WindowWidth:integer read fWindowWidth write fSetWindowWidth;
    property WindowHeight:integer read fWindowHeight write fSetWindowHeight;

  // Backup settings
  private
    fBackupIntervalTicks:uint64;  // in milliseconds
    fBackupInterval:integer;
    fBackupUnitSize:integer;  // 0 - Kbyte, 1 - Mbyte
    fBackupFolderMaxSize:integer;  // in kbytes or mbytes, depending on fBackupUnitSize (0/1)
    procedure fSetBackupInterval(value:integer);
    procedure fSetBackupUnitSize(value:integer);
    procedure fSetBackupFolderMaxSize(value:integer);
    function fGetRealBackupFolderMaxSize:integer;
    function fGetRealBackupFolderRetentionTime:integer;
    function fGetRealBackupFolderMaxFileCount:integer;
  public
    CreateBackups:boolean;
    LimitBackupFolderMaxSize:boolean;
    LimitBackupFolderRetentionTime:boolean;
    BackupFolderRetentionTime:integer; // in days
    LimitBackupFolderMaxFileCount:boolean;
    BackupFolderMaxFileCount:integer;
    property BackupIntervalTicks:uint64 read fBackupIntervalTicks;
    property BackupInterval:integer read fBackupInterval write fSetBackupInterval;
    property BackupUnitSize:integer read fBackupUnitSize write fSetBackupUnitSize;
    property BackupFolderMaxSize:integer read fBackupFolderMaxSize write fSetBackupFolderMaxSize;
    property RealBackupFolderMaxSize:integer read fGetRealBackupFolderMaxSize;
    property RealBackupFolderRetentionTime:integer read fGetRealBackupFolderRetentionTime;
    property RealBackupFolderMaxFileCount:integer read fGetRealBackupFolderMaxFileCount;

  // DrawArea settings
  private
    fZoom:integer;
    procedure fSetZoom(value:integer);
  public
    ZoomLeft,ZoomTop:integer;
    ShowGrid:boolean;
    property Zoom:integer read fZoom write fSetZoom;

  // Basic Control settings
  private
    fSelectedTools:TStringArray8;
    fActiveTool:integer;  // within fSelectedTools
    fSelectedInks:TStringArray8;
    fActiveInk:integer;  // within fSelectedInks
    function fGetSelectedTool(index:integer):string;
    procedure fSetSelectedTool(index:integer;value:string);
    function fGetSelectedInk(index:integer):string;
    procedure fSetSelectedInk(index:integer;value:string);
  public
    FillShapes:boolean;
    ClearKeyColor:boolean;
    property SelectedTools[index:integer]:string read fGetSelectedTool write fSetSelectedTool;
    property ActiveTool:integer read fActiveTool write fActiveTool;
    property SelectedInks[index:integer]:string read fGetSelectedInk write fSetSelectedInk;
    property ActiveInk:integer read fActiveInk write fActiveInk;

  // Colors settings
  private
    fActiveColor:uint32;
    procedure fSetActiveColor(value:uint32);
  public
    ColorSelectorMainColor:uint32;
    ColorSelectorLeftColor:uint32;
    ColorSelectorRightColor:uint32;
    property ActiveColor:uint32 read fActiveColor write fSetActiveColor;

  // Inks settings
  private
    fTintStrength:integer;
    fRealTintStrength:double;
    procedure fSetTintStrength(value:integer);
  public
    CGradCenterX,CGradCenterY,CGradRadius:integer;
    RGradCenterX,RGradCenterY,RGradRepetitions,RGradRotation:integer;
    TempRGradCenterX,TempRGradCenterY:integer;
    TintCELAsMask:boolean;
    SoftenCenterWeight:integer;
    SoftenAlphaToo:boolean;
    property TintStrength:integer read fTintStrength write fSetTintStrength;
    property RealTintStrength:double read fRealTintStrength;

  // Tools settings
  public
    CircleMode:integer; // 0 - Center+radius, 1 - BoundingBox
    SepBoxed:boolean;

  end;


implementation

uses BDPKeyMapping, BDPShared, Logger;

{ TSettings }

constructor TSettings.Create;
begin
  fZoom:=2;
  ZoomLeft:=0;
  ZoomTop:=0;
  fSelectedTools[0]:='DRAW';
  fSelectedTools[1]:='BOX';
  fSelectedTools[2]:='LINE';
  fSelectedTools[3]:='CIRCLE';
  fSelectedTools[4]:='SEP.';
  fSelectedTools[5]:='FILL';
  fSelectedTools[6]:='EDGE';
  fSelectedTools[7]:='FILLTO';
  fActiveTool:=0;
  fSelectedInks[0]:='OPAQUE';
  fSelectedInks[1]:='H GRAD';
  fSelectedInks[2]:='V GRAD';
  fSelectedInks[3]:='SOFTEN';
  fSelectedInks[4]:='L GRAD';
  fSelectedInks[5]:='C GRAD';
  fSelectedInks[6]:='R GRAD';
  fSelectedInks[7]:='RANDOM';
  fActiveInk:=0;
  ColorSelectorMainColor:=$FFFF0000;
  ColorSelectorLeftColor:=$FF000000;
  ColorSelectorRightColor:=$FFFFFFFF;
  fActiveColor:=$FFFF0000;
  UndoLimit:=16;
  TintStrength:=10;
  TintCELAsMask:=true;
  CGradCenterX:=0;
  CGradCenterY:=0;
  CGradRadius:=32;
  RGradCenterX:=0;
  RGradCenterY:=0;
  RGradRepetitions:=1;
  RGradRotation:=0;
  CreateBackups:=true;
  BackupInterval:=60;
  LimitBackupFolderMaxSize:=false;
  BackupUnitSize:=1;
  fBackupFolderMaxSize:=2;
  LimitBackupFolderRetentionTime:=false;
  BackupFolderRetentionTime:=0;
  LimitBackupFolderMaxFileCount:=false;
  BackupFolderMaxFileCount:=0;
  SoftenCenterWeight:=1;
  SoftenAlphaToo:=false;
  CircleMode:=0;
  fWindowWidth:=MINIMUMWINDOWWIDTH;
  fWindowHeight:=MINIMUMWINDOWHEIGHT;
end;

procedure TSettings.LoadFromFile(pFilename:String);
var INI:TIniFile;v:integer;
begin
  if not FileExists(pFilename) then exit;
  INI:=TIniFile.Create(pFilename);
  try
    v:=INI.ReadInteger('Settings','Version',0);
    case v of
      0:LoadFromFileV0(INI);
      1:LoadFromFileV1(INI);
      else Log.LogError(Format('Unknown settings version (%d), using default settings!',[v]));
    end;
  finally
    INI.Free;
  end;
end;

procedure TSettings.SaveToFile(pFilename:String);
var INI:TIniFile;i:integer;
begin
  if FileExists(pFilename) then DeleteFile(pFilename);
  INI:=TIniFile.Create(pFilename,false);
  try
    INI.WriteInteger('Settings','Version',1);
    // System settings
    INI.WriteInteger('Settings','WindowWidth',fWindowWidth);
    INI.WriteInteger('Settings','WindowHeight',fWindowHeight);
    INI.WriteBool('Settings','ShowSplash',ShowSplash);
    INI.WriteInteger('Settings','UndoLimit',UndoLimit);
    // Backup settings
    INI.WriteBool('Backup','CreateBackups',CreateBackups);
    INI.WriteInteger('Backup','BackupInterval',fBackupInterval);
    INI.WriteBool('Backup','LimitBackupFolderMaxSize',LimitBackupFolderMaxSize);
    INI.WriteInteger('Backup','BackupFolderMaxSize',fBackupFolderMaxSize);
    INI.WriteInteger('Backup','BackupUnitSize',fBackupUnitSize);
    INI.WriteBool('Backup','LimitBackupFolderRetentionTime',LimitBackupFolderRetentionTime);
    INI.WriteInteger('Backup','BackupFolderRetentionTime',BackupFolderRetentionTime);
    INI.WriteBool('Backup','LimitBackupFolderMaxFileCount',LimitBackupFolderMaxFileCount);
    INI.WriteInteger('Backup','BackupFolderMaxFileCount',BackupFolderMaxFileCount);
    // DrawArea state
    INI.WriteInteger('DrawArea','Zoom',fZoom);
    INI.WriteInteger('DrawArea','ZoomLeft',ZoomLeft);
    INI.WriteInteger('DrawArea','ZoomTop',ZoomTop);
    INI.WriteBool('DrawArea','ShowGrid',ShowGrid);
    // Controls state
    for i:=0 to length(fSelectedTools)-1 do
      INI.WriteString('BasicControls','Tool'+inttostr(i),fSelectedTools[i]);
    INI.WriteInteger('BasicControls','ActiveTool',fActiveTool);
    for i:=0 to length(fSelectedInks)-1 do
      INI.WriteString('BasicControls','Ink'+inttostr(i),fSelectedInks[i]);
    INI.WriteInteger('BasicControls','ActiveInk',fActiveInk);
    INI.WriteBool('BasicControls','FillShapes',FillShapes);
    INI.WriteBool('BasicControls','ClearKeyColor',ClearKeyColor);
    // Color selector state
    INI.WriteUInt32('Colors','Main',ColorSelectorMainColor);
    INI.WriteUInt32('Colors','Left',ColorSelectorLeftColor);
    INI.WriteUInt32('Colors','Right',ColorSelectorRightColor);
    INI.WriteUInt32('Colors','ActiveColor',fActiveColor);
    // Inks' settings
    INI.WriteInteger('Inks','TintStrength',fTintStrength);
    INI.WriteBool('Inks','TintCELAsMask',TintCELAsMask);
    INI.WriteInteger('Inks','CGradCenterX',CGradCenterX);
    INI.WriteInteger('Inks','CGradCenterY',CGradCenterY);
    INI.WriteInteger('Inks','CGradRadius',CGradRadius);
    INI.WriteInteger('Inks','RGradCenterX',RGradCenterX);
    INI.WriteInteger('Inks','RGradCenterY',RGradCenterY);
    INI.WriteInteger('Inks','RGradRepetitions',RGradRepetitions);
    INI.WriteInteger('Inks','RGradRotation',RGradRotation);
    INI.WriteInteger('Inks','SoftenCenterWeight',SoftenCenterWeight);
    INI.WriteBool('Inks','SoftenAlphaToo',SoftenAlphaToo);
    // Tools' settings
    INI.WriteInteger('Tools','CircleMode',CircleMode);
    INI.WriteBool('Tools','SepBoxed',SepBoxed);
    // Keymap
    SaveKeyMap(INI);
  finally
    INI.Free;
  end;
end;

procedure TSettings.LoadFromFileV0(INI:TINIFile);
var i:integer;
begin
  // System settings
  WindowWidth:=INI.ReadInteger('Settings','WindowWidth',MINIMUMWINDOWWIDTH);
  if WindowWidth<MINIMUMWINDOWWIDTH then WindowWidth:=MINIMUMWINDOWWIDTH;
  WindowHeight:=INI.ReadInteger('Settings','WindowHeight',MINIMUMWINDOWHEIGHT);
  if WindowHeight<MINIMUMWINDOWHEIGHT then WindowHeight:=MINIMUMWINDOWHEIGHT;
  ShowSplash:=INI.ReadBool('Settings','ShowSplash',false);
  UndoLimit:=INI.ReadInteger('Settings','UndoLimit',16);
  CreateBackups:=INI.ReadBool('Settings','CreateBackups',true);
  BackupInterval:=INI.ReadInteger('Settings','BackupInterval',60);
  i:=INI.ReadInteger('Settings','BackupFolderMaxSize',2*1024*1024) div 1024;
  if i>1024 then begin  // More than 1 megabyte
    i:=i div 1024;
    if i>1024 then i:=1024; // More than 1 gigabyte, this is the limit
    fBackupFolderMaxSize:=i;
    BackupUnitSize:=1;
    LimitBackupFolderMaxSize:=true;
  end else
  if i>0 then begin  // It's in the kilobytes range
    BackupUnitSize:=0;
    fBackupFolderMaxSize:=i;
    LimitBackupFolderMaxSize:=true;
  end else begin  // Don't limit but set 64 kilobytes as default value
    BackupUnitSize:=0;
    fBackupFolderMaxSize:=64;
    LimitBackupFolderMaxSize:=false;
  end;
  BackupFolderRetentionTime:=INI.ReadInteger('Settings','BackupFolderRetentionTime',0);
  LimitBackupFolderRetentionTime:=BackupFolderRetentionTime>0;
  BackupFolderMaxFileCount:=INI.ReadInteger('Settings','BackupFolderMaxFileCount',0);
  LimitBackupFolderMaxFileCount:=BackupFolderMaxFileCount>0;
  // DrawArea state
  fZoom:=INI.ReadInteger('DrawArea','Zoom',2);
  ZoomLeft:=INI.ReadInteger('DrawArea','ZoomLeft',0);
  ZoomTop:=INI.ReadInteger('DrawArea','ZoomTop',0);
  ShowGrid:=INI.ReadBool('DrawArea','ShowGrid',false);
  // Controls state
  fSelectedTools[0]:=INI.ReadString('BasicControls','Tool0','DRAW');
  fSelectedTools[1]:=INI.ReadString('BasicControls','Tool1','BOX');
  fSelectedTools[2]:=INI.ReadString('BasicControls','Tool2','LINE');
  fSelectedTools[3]:=INI.ReadString('BasicControls','Tool3','CIRCLE');
  fSelectedTools[4]:=INI.ReadString('BasicControls','Tool4','SEP.');
  fSelectedTools[5]:=INI.ReadString('BasicControls','Tool5','FILL');
  fSelectedTools[6]:=INI.ReadString('BasicControls','Tool6','EDGE');
  fSelectedTools[7]:=INI.ReadString('BasicControls','Tool7','FILLTO');
  fActiveTool:=INI.ReadInteger('BasicControls','ActiveTool',0);
  if (fActiveTool<0) or (fActiveTool>7) then fActiveTool:=0;
  fSelectedInks[0]:=INI.ReadString('BasicControls','Ink0','OPAQUE');
  fSelectedInks[1]:=INI.ReadString('BasicControls','Ink1','H GRAD');
  fSelectedInks[2]:=INI.ReadString('BasicControls','Ink2','V GRAD');
  fSelectedInks[3]:=INI.ReadString('BasicControls','Ink3','SOFTEN');
  fSelectedInks[4]:=INI.ReadString('BasicControls','Ink4','L GRAD');
  fSelectedInks[5]:=INI.ReadString('BasicControls','Ink5','C GRAD');
  fSelectedInks[6]:=INI.ReadString('BasicControls','Ink6','R GRAD');
  fSelectedInks[7]:=INI.ReadString('BasicControls','Ink7','RANDOM');
  fActiveInk:=INI.ReadInteger('BasicControls','ActiveInk',0);
  if (fActiveInk<0) or (fActiveInk>7) then fActiveInk:=0;
  FillShapes:=INI.ReadBool('BasicControls','FillShapes',false);
  ClearKeyColor:=INI.ReadBool('BasicControls','ClearKeyColor',false);
  // Colors selector state
  ColorSelectorMainColor:=INI.ReadUInt32('Colors','Main',$FFFF0000);
  ColorSelectorLeftColor:=INI.ReadUInt32('Colors','Left',$FF000000);
  ColorSelectorRightColor:=INI.ReadUInt32('Colors','Right',$FFFFFFFF);
  fActiveColor:=INI.ReadUInt32('Colors','ActiveColor',$FFFF0000);
  // Inks' settings
  TintStrength:=INI.ReadInteger('Inks','TintStrength',10);
  TintCELAsMask:=INI.ReadBool('Inks','TintCELAsMask',true);
  CGradCenterX:=INI.ReadInteger('Inks','CGradCenterX',0);
  CGradCenterY:=INI.ReadInteger('Inks','CGradCenterY',0);
  CGradRadius:=INI.ReadInteger('Inks','CGradRadius',32);
  RGradCenterX:=INI.ReadInteger('Inks','RGradCenterX',0);
  RGradCenterY:=INI.ReadInteger('Inks','RGradCenterY',0);
  RGradRepetitions:=INI.ReadInteger('Inks','RGradRepetitions',1);
  RGradRotation:=INI.ReadInteger('Inks','RGradRotation',0);
  SoftenCenterWeight:=INI.ReadInteger('Inks','SoftenCenterWeight',1);
  SoftenAlphaToo:=INI.ReadBool('Inks','SoftenAlphaToo',false);
  // Tools' settings
  CircleMode:=INI.ReadInteger('Tools','CircleMode',0);
  SepBoxed:=INI.ReadBool('Tools','SepBoxed',false);
  // Keymap
  LoadKeyMap(INI);
end;

procedure TSettings.LoadFromFileV1(INI:TINIFile);
begin
  // System settings
  WindowWidth:=INI.ReadInteger('Settings','WindowWidth',MINIMUMWINDOWWIDTH);
  if WindowWidth<MINIMUMWINDOWWIDTH then WindowWidth:=MINIMUMWINDOWWIDTH;
  WindowHeight:=INI.ReadInteger('Settings','WindowHeight',MINIMUMWINDOWHEIGHT);
  if WindowHeight<MINIMUMWINDOWHEIGHT then WindowHeight:=MINIMUMWINDOWHEIGHT;
  ShowSplash:=INI.ReadBool('Settings','ShowSplash',false);
  UndoLimit:=INI.ReadInteger('Settings','UndoLimit',16);
  // Backup settings
  CreateBackups:=INI.ReadBool('Backup','CreateBackups',true);
  BackupInterval:=INI.ReadInteger('Backup','BackupInterval',60);
  LimitBackupFolderMaxSize:=INI.ReadBool('Backup','LimitBackupFolderMaxSize',true);
  BackupUnitSize:=INI.ReadInteger('Backup','BackupUnitSize',0);
  fBackupFolderMaxSize:=INI.ReadInteger('Backup','BackupFolderMaxSize',64);
  LimitBackupFolderRetentionTime:=INI.ReadBool('Backup','LimitBackupFolderRetentionTime',false);
  BackupFolderRetentionTime:=INI.ReadInteger('Backup','BackupFolderRetentionTime',0);
  LimitBackupFolderMaxFileCount:=INI.ReadBool('Backup','LimitBackupFolderMaxFileCount',false);
  BackupFolderMaxFileCount:=INI.ReadInteger('Backup','BackupFolderMaxFileCount',0);
  // DrawArea state
  fZoom:=INI.ReadInteger('DrawArea','Zoom',2);
  ZoomLeft:=INI.ReadInteger('DrawArea','ZoomLeft',0);
  ZoomTop:=INI.ReadInteger('DrawArea','ZoomTop',0);
  ShowGrid:=INI.ReadBool('DrawArea','ShowGrid',false);
  // Controls state
  fSelectedTools[0]:=INI.ReadString('BasicControls','Tool0','DRAW');
  fSelectedTools[1]:=INI.ReadString('BasicControls','Tool1','BOX');
  fSelectedTools[2]:=INI.ReadString('BasicControls','Tool2','LINE');
  fSelectedTools[3]:=INI.ReadString('BasicControls','Tool3','CIRCLE');
  fSelectedTools[4]:=INI.ReadString('BasicControls','Tool4','SEP.');
  fSelectedTools[5]:=INI.ReadString('BasicControls','Tool5','FILL');
  fSelectedTools[6]:=INI.ReadString('BasicControls','Tool6','EDGE');
  fSelectedTools[7]:=INI.ReadString('BasicControls','Tool7','FILLTO');
  fActiveTool:=INI.ReadInteger('BasicControls','ActiveTool',0);
  if (fActiveTool<0) or (fActiveTool>7) then fActiveTool:=0;
  fSelectedInks[0]:=INI.ReadString('BasicControls','Ink0','OPAQUE');
  fSelectedInks[1]:=INI.ReadString('BasicControls','Ink1','H GRAD');
  fSelectedInks[2]:=INI.ReadString('BasicControls','Ink2','V GRAD');
  fSelectedInks[3]:=INI.ReadString('BasicControls','Ink3','SOFTEN');
  fSelectedInks[4]:=INI.ReadString('BasicControls','Ink4','L GRAD');
  fSelectedInks[5]:=INI.ReadString('BasicControls','Ink5','C GRAD');
  fSelectedInks[6]:=INI.ReadString('BasicControls','Ink6','R GRAD');
  fSelectedInks[7]:=INI.ReadString('BasicControls','Ink7','RANDOM');
  fActiveInk:=INI.ReadInteger('BasicControls','ActiveInk',0);
  if (fActiveInk<0) or (fActiveInk>7) then fActiveInk:=0;
  FillShapes:=INI.ReadBool('BasicControls','FillShapes',false);
  ClearKeyColor:=INI.ReadBool('BasicControls','ClearKeyColor',false);
  // Colors selector state
  ColorSelectorMainColor:=INI.ReadUInt32('Colors','Main',$FFFF0000);
  ColorSelectorLeftColor:=INI.ReadUInt32('Colors','Left',$FF000000);
  ColorSelectorRightColor:=INI.ReadUInt32('Colors','Right',$FFFFFFFF);
  fActiveColor:=INI.ReadUInt32('Colors','ActiveColor',$FFFF0000);
  // Inks' settings
  TintStrength:=INI.ReadInteger('Inks','TintStrength',10);
  TintCELAsMask:=INI.ReadBool('Inks','TintCELAsMask',true);
  CGradCenterX:=INI.ReadInteger('Inks','CGradCenterX',0);
  CGradCenterY:=INI.ReadInteger('Inks','CGradCenterY',0);
  CGradRadius:=INI.ReadInteger('Inks','CGradRadius',32);
  RGradCenterX:=INI.ReadInteger('Inks','RGradCenterX',0);
  RGradCenterY:=INI.ReadInteger('Inks','RGradCenterY',0);
  RGradRepetitions:=INI.ReadInteger('Inks','RGradRepetitions',1);
  RGradRotation:=INI.ReadInteger('Inks','RGradRotation',0);
  SoftenCenterWeight:=INI.ReadInteger('Inks','SoftenCenterWeight',1);
  SoftenAlphaToo:=INI.ReadBool('Inks','SoftenAlphaToo',false);
  // Tools' settings
  CircleMode:=INI.ReadInteger('Tools','CircleMode',0);
  SepBoxed:=INI.ReadBool('Tools','SepBoxed',false);
  // Keymap
  LoadKeyMap(INI);
end;

procedure TSettings.fSetWindowWidth(value: integer);
begin
  if (value<>fWindowWidth) and (value>=MINIMUMWINDOWWIDTH) then
    fWindowWidth:=value;
end;

procedure TSettings.fSetWindowHeight(value: integer);
begin
  if (value<>fWindowHeight) and (value>=MINIMUMWINDOWHEIGHT) then
    fWindowHeight:=value;
end;

procedure TSettings.fSetBackupInterval(value:integer);
begin
  if value<30 then value:=30
  else if value>600 then value:=600;
  if fBackupInterval<>value then begin
    fBackupInterval:=value;
    fBackupIntervalTicks:=value*1000;
  end;
end;

procedure TSettings.fSetBackupUnitSize(value:integer);
begin
  if (value in [0..1]) and (value<>fBackupUnitSize) then fBackupUnitSize:=value;
end;

function TSettings.fGetRealBackupFolderMaxSize:integer;
begin
  if LimitBackupFolderMaxSize then begin
    Result:=fBackupFolderMaxSize*1024;
    if BackupUnitSize=1 then Result:=Result*1024;
  end else Result:=0;
end;

function TSettings.fGetRealBackupFolderRetentionTime:integer;
begin
  if LimitBackupFolderRetentionTime then begin
    Result:=BackupFolderRetentionTime;
  end else Result:=0;
end;

function TSettings.fGetRealBackupFolderMaxFileCount:integer;
begin
  if LimitBackupFolderMaxFileCount then begin
    Result:=BackupFolderMaxFileCount;
  end else Result:=0;
end;

procedure TSettings.fSetBackupFolderMaxSize(value:integer);
begin
  if (value<0) then value:=0
  else if value>1024 then value:=1024;
  if value<>fBackupFolderMaxSize then fBackupFolderMaxSize:=value;
end;

procedure TSettings.fSetZoom(value:integer);
begin
  if value in [1..MAXZOOMLEVEL] then fZoom:=value;
end;

function TSettings.fGetSelectedTool(index:integer):string;
begin
  if (index>=0) and (index<8) then
    Result:=fSelectedTools[index]
  else raise Exception.Create(Format('fGetSelectedTool: Index out of range! (%d)',[index]));
end;

procedure TSettings.fSetSelectedTool(index:integer; value:string);
begin
  if (index>=0) and (index<8) then
    fSelectedTools[index]:=value
  else raise Exception.Create(Format('fSetSelectedTool: Index out of range! (%d)',[index]));
end;

function TSettings.fGetSelectedInk(index:integer):string;
begin
  if (index>=0) and (index<8) then
    Result:=fSelectedInks[index]
  else raise Exception.Create(Format('fGetSelectedInk: Index out of range! (%d)',[index]));
end;

procedure TSettings.fSetSelectedInk(index:integer; value:string);
begin
  if (index>=0) and (index<8) then
    fSelectedInks[index]:=value
  else raise Exception.Create(Format('fSetSelectedInk: Index out of range! (%d)',[index]));
end;

procedure TSettings.fSetActiveColor(value:uint32);
begin
  fActiveColor:=value;
  MessageQueue.AddMessage(MSG_ACTIVECOLORCHANGED,0,fActiveColor);
end;

procedure TSettings.fSetTintStrength(value: integer);
begin
  if value<0 then value:=0
  else if value>100 then value:=100;
  fTintStrength:=value;
  fRealTintStrength:=value/100;
end;

end.

