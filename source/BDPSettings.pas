{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPSettings;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type
  TStringArray6=array[0..5] of string;

  { TSettings }

  TSettings=class
    constructor Create;
//    destructor Destroy; override;
    procedure LoadFromFile(pFilename:String);
    procedure SaveToFile(pFilename:String);
  private
    fShowSplash:Boolean;
    fZoom:integer;
    fSelectedTools:TStringArray6;
    fActiveTool:integer;  // within fSelectedTools
    fSelectedInks:TStringArray6;
    fActiveInk:integer;  // within fSelectedInks
    fActiveColor:uint32;
    fDitherStrength:integer;
    fRealDitherStrength:double;
    fTintStrength:integer;
    fRealTintStrength:double;
    procedure fSetZoom(value:integer);
    function fGetSelectedTool(index:integer):string;
    procedure fSetSelectedTool(index:integer;value:string);
    function fGetSelectedInk(index:integer):string;
    procedure fSetSelectedInk(index:integer;value:string);
    procedure fSetActiveColor(value:uint32);
    procedure fSetDitherStrength(value:integer);
    procedure fSetTintStrength(value:integer);
  public
    ColorSelectorMainColor:uint32;
    ColorSelectorLeftColor:uint32;
    ColorSelectorRightColor:uint32;
    FillShapes:boolean;
    ClearKeyColor:boolean;
    DitherGradients:boolean;
    ShowGrid:boolean;
    ZoomLeft,ZoomTop:integer;
    UndoLimit:uint32;
    CGradCenterX,CGradCenterY,CGradRadius:integer;
    RGradCenterX,RGradCenterY,RGradRepetitions,RGradRotation:integer;
    TempRGradCenterX,TempRGradCenterY:integer;
    BackupIntervalTicks:uint64;  // in milliseconds
    BackupFolderMaxSize:integer;  // in bytes, set 0 to disable size check
    BackupFolderRetentionTime:integer; // in seconds, set 0 to disable file age check
    BackupFolderMaxFileCount:integer;  // set 0 to disable file count check
    property Zoom:integer read fZoom write fSetZoom;
    property SelectedTools[index:integer]:string read fGetSelectedTool write fSetSelectedTool;
    property ActiveTool:integer read fActiveTool write fActiveTool;
    property SelectedInks[index:integer]:string read fGetSelectedInk write fSetSelectedInk;
    property ActiveInk:integer read fActiveInk write fActiveInk;
    property ActiveColor:uint32 read fActiveColor write fSetActiveColor;
    property DitherStrength:integer read fDitherStrength write fSetDitherStrength;
    property RealDitherStrength:double read fRealDitherStrength;
    property TintStrength:integer read fTintStrength write fSetTintStrength;
    property RealTintStrength:double read fRealTintStrength;
  end;


implementation

uses MKINIFile, BDPKeyMapping, BDPShared;

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
  fActiveTool:=0;
  fSelectedInks[0]:='OPAQUE';
  fSelectedInks[1]:='H GRAD';
  fSelectedInks[2]:='V GRAD';
  fSelectedInks[3]:='SOFTEN';
  fSelectedInks[4]:='L GRAD';
  fSelectedInks[5]:='C GRAD';
  fActiveInk:=0;
  ColorSelectorMainColor:=$FFFF0000;
  ColorSelectorLeftColor:=$FF000000;
  ColorSelectorRightColor:=$FFFFFFFF;
  fActiveColor:=$FFFF0000;
  UndoLimit:=16;
  DitherStrength:=10;
  TintStrength:=10;
  CGradCenterX:=0;
  CGradCenterY:=0;
  CGradRadius:=32;
  RGradCenterX:=0;
  RGradCenterY:=0;
  RGradRepetitions:=1;
  RGradRotation:=0;
  BackupIntervalTicks:=60*1000;
  BackupFolderMaxSize:=0;
  BackupFolderRetentionTime:=0;
  BackupFolderMaxFileCount:=0;
end;

procedure TSettings.LoadFromFile(pFilename:String);
var INI:TIniFile;
begin
  if not FileExists(pFilename) then exit;
  INI:=TIniFile.Create(pFilename);
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
  fActiveTool:=INI.ReadInteger('BasicControls','ActiveTool',0);
  if (fActiveTool<0) or (fActiveTool>5) then fActiveTool:=0;
  fSelectedInks[0]:=INI.ReadString('BasicControls','Ink0','OPAQUE');
  fSelectedInks[1]:=INI.ReadString('BasicControls','Ink1','H GRAD');
  fSelectedInks[2]:=INI.ReadString('BasicControls','Ink2','V GRAD');
  fSelectedInks[3]:=INI.ReadString('BasicControls','Ink3','SOFTEN');
  fSelectedInks[4]:=INI.ReadString('BasicControls','Ink4','L GRAD');
  fSelectedInks[5]:=INI.ReadString('BasicControls','Ink5','C GRAD');
  fActiveInk:=INI.ReadInteger('BasicControls','ActiveInk',0);
  if (fActiveInk<0) or (fActiveInk>5) then fActiveInk:=0;
  FillShapes:=INI.ReadBool('BasicControls','FillShapes',false);
  ClearKeyColor:=INI.ReadBool('BasicControls','ClearKeyColor',false);
  // System settings
  fShowSplash:=INI.ReadBool('Settings','ShowSplash',false);
  UndoLimit:=INI.ReadInteger('Settings','UndoLimit',16);
  BackupIntervalTicks:=INI.ReadInteger('Settings','BackupInterval',60)*1000;
  BackupFolderMaxSize:=INI.ReadInteger('Settings','BackupFolderMaxSize',16*1024*1024);
  BackupFolderRetentionTime:=INI.ReadInteger('Settings','BackupFolderRetentionTime',0);
  BackupFolderMaxFileCount:=INI.ReadInteger('Settings','BackupFolderMaxFileCount',0);
  // Keymap
  LoadKeyMap(INI);
  // Colors selector state
  ColorSelectorMainColor:=INI.ReadUInt32('Colors','Main',$FFFF0000);
  ColorSelectorLeftColor:=INI.ReadUInt32('Colors','Left',$FF000000);
  ColorSelectorRightColor:=INI.ReadUInt32('Colors','Right',$FFFFFFFF);
  fActiveColor:=INI.ReadUInt32('Colors','ActiveColor',$FFFF0000);
  // Inks' settings
  DitherGradients:=INI.ReadBool('Inks','DitherGradients',false);
  DitherStrength:=INI.ReadInteger('Inks','DitherStrength',10);
  TintStrength:=INI.ReadInteger('Inks','TintStrength',10);
  CGradCenterX:=INI.ReadInteger('Inks','CGradCenterX',0);
  CGradCenterY:=INI.ReadInteger('Inks','CGradCenterY',0);
  CGradRadius:=INI.ReadInteger('Inks','CGradRadius',32);
  RGradCenterX:=INI.ReadInteger('Inks','RGradCenterX',0);
  RGradCenterY:=INI.ReadInteger('Inks','RGradCenterY',0);
  RGradRepetitions:=INI.ReadInteger('Inks','RGradRepetitions',1);
  RGradRotation:=INI.ReadInteger('Inks','RGradRotation',0);
  FreeAndNil(INI);
end;

procedure TSettings.SaveToFile(pFilename:String);
var INI:TIniFile;i:integer;
begin
  INI:=TIniFile.Create(pFilename,false);
  // DrawArea state
  INI.WriteInteger('DrawArea','Zoom',fZoom);
  INI.WriteInteger('DrawArea','ZoomLeft',ZoomLeft);
  INI.WriteInteger('DrawArea','ZoomTop',ZoomTop);
  INI.WriteBool('DrawArea','ShowGrid',ShowGrid);
  // Controls state
  for i:=0 to 5 do
    INI.WriteString('BasicControls','Tool'+inttostr(i),fSelectedTools[i]);
  INI.WriteInteger('BasicControls','ActiveTool',fActiveTool);
  for i:=0 to 5 do
    INI.WriteString('BasicControls','Ink'+inttostr(i),fSelectedInks[i]);
  INI.WriteInteger('BasicControls','ActiveInk',fActiveInk);
  INI.WriteBool('BasicControls','FillShapes',FillShapes);
  INI.WriteBool('BasicControls','ClearKeyColor',ClearKeyColor);
  // System settings
  INI.WriteBool('Settings','ShowSplash',fShowSplash);
  INI.WriteInteger('Settings','UndoLimit',UndoLimit);
  INI.WriteInteger('Settings','BackupInterval',BackupIntervalTicks div 1000);
  INI.WriteInteger('Settings','BackupFolderMaxSize',BackupFolderMaxSize);
  INI.WriteInteger('Settings','BackupFolderRetentionTime',BackupFolderRetentionTime);
  INI.WriteInteger('Settings','BackupFolderMaxFileCount',BackupFolderMaxFileCount);
  // Keymap
  SaveKeyMap(INI);
  // Color selector state
  INI.WriteUInt32('Colors','Main',ColorSelectorMainColor);
  INI.WriteUInt32('Colors','Left',ColorSelectorLeftColor);
  INI.WriteUInt32('Colors','Right',ColorSelectorRightColor);
  INI.WriteUInt32('Colors','ActiveColor',fActiveColor);
  // Inks' settings
  INI.WriteBool('Inks','DitherGradients',DitherGradients);
  INI.WriteInteger('Inks','DitherStrength',fDitherStrength);
  INI.WriteInteger('Inks','TintStrength',fTintStrength);
  INI.WriteInteger('Inks','CGradCenterX',CGradCenterX);
  INI.WriteInteger('Inks','CGradCenterY',CGradCenterY);
  INI.WriteInteger('Inks','CGradRadius',CGradRadius);
  INI.WriteInteger('Inks','RGradCenterX',RGradCenterX);
  INI.WriteInteger('Inks','RGradCenterY',RGradCenterY);
  INI.WriteInteger('Inks','RGradRepetitions',RGradRepetitions);
  INI.WriteInteger('Inks','RGradRotation',RGradRotation);
  FreeAndNil(INI);
end;

function TSettings.fGetSelectedTool(index:integer):string;
begin
  if (index>=0) and (index<6) then
    Result:=fSelectedTools[index]
  else raise Exception.Create(Format('fGetSelectedTool: Index out of range! (%d)',[index]));
end;

procedure TSettings.fSetSelectedTool(index:integer; value:string);
begin
  if (index>=0) and (index<6) then
    fSelectedTools[index]:=value
  else raise Exception.Create(Format('fSetSelectedTool: Index out of range! (%d)',[index]));
end;

function TSettings.fGetSelectedInk(index:integer):string;
begin
  if (index>=0) and (index<6) then
    Result:=fSelectedInks[index]
  else raise Exception.Create(Format('fGetSelectedInk: Index out of range! (%d)',[index]));
end;

procedure TSettings.fSetSelectedInk(index:integer; value:string);
begin
  if (index>=0) and (index<6) then
    fSelectedInks[index]:=value
  else raise Exception.Create(Format('fSetSelectedInk: Index out of range! (%d)',[index]));
end;

procedure TSettings.fSetActiveColor(value:uint32);
begin
  fActiveColor:=value;
  MessageQueue.AddMessage(MSG_ACTIVECOLORCHANGED,0,fActiveColor);
end;

procedure TSettings.fSetDitherStrength(value:integer);
begin
  if value<0 then value:=0
  else if value>255 then value:=255;
  fDitherStrength:=value;
  fRealDitherStrength:=value/255;
end;

procedure TSettings.fSetTintStrength(value: integer);
begin
  if value<0 then value:=0
  else if value>100 then value:=100;
  fTintStrength:=value;
  fRealTintStrength:=value/100;
end;

procedure TSettings.fSetZoom(value:integer);
begin
  if value in [1..MAXZOOMLEVEL] then fZoom:=value;
end;

end.

