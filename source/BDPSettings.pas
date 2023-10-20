{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of BurdockPaint.

  BurdockPaint is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  BurdockPaint is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  BurdockPaint. If not, see <https://www.gnu.org/licenses/>.
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
    fZoom,fZoomLeft,fZoomTop:integer;
    fSelectedTools:TStringArray6;
    fActiveTool:integer;  // within fSelectedTools
    fSelectedInks:TStringArray6;
    fActiveInk:integer;  // within fSelectedInks
    fActiveColor:uint32;
    fUndoLimit:integer;
    fDitherStrength:integer;
    fRealDitherStrength:double;
    fCGradCenterX,fCGradCenterY,fCGradRadius:integer;
    fRGradCenterX,fRGradCenterY,fRGradRepetitions,fRGradRotation:integer;
    fTempRGradCenterX,fTempRGradCenterY:integer;
    fBackupIntervalTicks:uint64;  // in milliseconds
    fBackupFolderMaxSize:integer;  // in bytes, set 0 to disable size check
    fBackupFolderRetentionTime:integer; // in seconds, set 0 to disable file age check
    function fGetSelectedTool(index:integer):string;
    procedure fSetSelectedTool(index:integer;value:string);
    function fGetSelectedInk(index:integer):string;
    procedure fSetSelectedInk(index:integer;value:string);
    procedure fSetActiveColor(value:uint32);
    procedure fSetDitherStrength(value:integer);
  public
    ColorSelectorMainColor:uint32;
    ColorSelectorLeftColor:uint32;
    ColorSelectorRightColor:uint32;
    FillShapes:boolean;
    ClearKeyColor:boolean;
    DitherGradients:boolean;
    ShowGrid:boolean;
    property Zoom:integer read fZoom write fZoom;
    property ZoomLeft:integer read fZoomLeft write fZoomLeft;
    property ZoomTop:integer read fZoomTop write fZoomTop;
    property SelectedTools[index:integer]:string read fGetSelectedTool write fSetSelectedTool;
    property ActiveTool:integer read fActiveTool write fActiveTool;
    property SelectedInks[index:integer]:string read fGetSelectedInk write fSetSelectedInk;
    property ActiveInk:integer read fActiveInk write fActiveInk;
    property ActiveColor:uint32 read fActiveColor write fSetActiveColor;
    property UndoLimit:integer read fUndoLimit write fUndoLimit;
    property DitherStrength:integer read fDitherStrength write fSetDitherStrength;
    property RealDitherStrength:double read fRealDitherStrength;
    property CGradCenterX:integer read fCGradCenterX write fCGradCenterX;
    property CGradCenterY:integer read fCGradCenterY write fCGradCenterY;
    property CGradRadius:integer read fCGradRadius write fCGradRadius;
    property RGradCenterX:integer read fRGradCenterX write fRGradCenterX;
    property RGradCenterY:integer read fRGradCenterY write fRGradCenterY;
    property RGradRepetitions:integer read fRGradRepetitions write fRGradRepetitions;
    property RGradRotation:integer read fRGradRotation write fRGradRotation;
    property TempRGradCenterX:integer read fTempRGradCenterX write fTempRGradCenterX;
    property TempRGradCenterY:integer read fTempRGradCenterY write fTempRGradCenterY;
    property BackupIntervalTicks:uint64 read fBackupIntervalTicks write fBackupIntervalTicks;
    property BackupFolderMaxSize:integer read fBackupFolderMaxSize write fBackupFolderMaxSize;
    property BackupFolderRetentionTime:integer read fBackupFolderRetentionTime write fBackupFolderRetentionTime;
  end;


implementation

uses MKINIFile, BDPKeyMapping, BDPShared;

{ TSettings }

constructor TSettings.Create;
begin
  fZoom:=2;
  fZoomLeft:=0;
  fZoomTop:=0;
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
  fSelectedInks[3]:='ADD';
  fSelectedInks[4]:='L GRAD';
  fSelectedInks[5]:='C GRAD';
  fActiveInk:=0;
  ColorSelectorMainColor:=$FFFF0000;
  ColorSelectorLeftColor:=$FF000000;
  ColorSelectorRightColor:=$FFFFFFFF;
  fActiveColor:=$FFFF0000;
  fUndoLimit:=16;
  fDitherStrength:=10;
  fCGradCenterX:=0;
  fCGradCenterY:=0;
  fCGradRadius:=32;
  fRGradCenterX:=0;
  fRGradCenterY:=0;
  fRGradRepetitions:=1;
  fRGradRotation:=0;
  fBackupIntervalTicks:=60*1000;
  fBackupFolderMaxSize:=0;
  fBackupFolderRetentionTime:=0;
end;

procedure TSettings.LoadFromFile(pFilename:String);
var INI:TIniFile;
begin
  if not FileExists(pFilename) then exit;
  INI:=TIniFile.Create(pFilename);
  // DrawArea state
  fZoom:=INI.ReadInteger('DrawArea','Zoom',2);
  fZoomLeft:=INI.ReadInteger('DrawArea','ZoomLeft',0);
  fZoomTop:=INI.ReadInteger('DrawArea','ZoomTop',0);
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
  fSelectedInks[3]:=INI.ReadString('BasicControls','Ink3','ADD');
  fSelectedInks[4]:=INI.ReadString('BasicControls','Ink4','L GRAD');
  fSelectedInks[5]:=INI.ReadString('BasicControls','Ink5','C GRAD');
  fActiveInk:=INI.ReadInteger('BasicControls','ActiveInk',0);
  if (fActiveInk<0) or (fActiveInk>5) then fActiveInk:=0;
  FillShapes:=INI.ReadBool('BasicControls','FillShapes',false);
  ClearKeyColor:=INI.ReadBool('BasicControls','ClearKeyColor',false);
  // System settings
  fShowSplash:=INI.ReadBool('Settings','ShowSplash',false);
  fUndoLimit:=INI.ReadInteger('Settings','UndoLimit',16);
  fBackupIntervalTicks:=INI.ReadInteger('Settings','BackupInterval',60)*1000;
  fBackupFolderMaxSize:=INI.ReadInteger('Settings','BackupFolderMaxSize',16*1024*1024);
  fBackupFolderRetentionTime:=INI.ReadInteger('Settings','BackupFolderRetentionTime',0);
  // Keymap
  LoadKeyMap(INI);
  // Colors selector state
  ColorSelectorMainColor:=INI.ReadUInt32('Colors','Main',$FFFF0000);
  ColorSelectorLeftColor:=INI.ReadUInt32('Colors','Left',$FF000000);
  ColorSelectorRightColor:=INI.ReadUInt32('Colors','Right',$FFFFFFFF);
  fActiveColor:=INI.ReadUInt32('Colors','ActiveColor',$FFFF0000);
  // Inks' settings
  DitherGradients:=INI.ReadBool('Inks','DitherGradients',false);
  fDitherStrength:=INI.ReadInteger('Inks','DitherStrength',10);
  fCGradCenterX:=INI.ReadInteger('Inks','CGradCenterX',0);
  fCGradCenterY:=INI.ReadInteger('Inks','CGradCenterY',0);
  fCGradRadius:=INI.ReadInteger('Inks','CGradRadius',32);
  fRGradCenterX:=INI.ReadInteger('Inks','RGradCenterX',0);
  fRGradCenterY:=INI.ReadInteger('Inks','RGradCenterY',0);
  fRGradRepetitions:=INI.ReadInteger('Inks','RGradRepetitions',1);
  fRGradRotation:=INI.ReadInteger('Inks','RGradRotation',0);
  FreeAndNil(INI);
end;

procedure TSettings.SaveToFile(pFilename:String);
var INI:TIniFile;i:integer;
begin
  INI:=TIniFile.Create(pFilename,false);
  // DrawArea state
  INI.WriteInteger('DrawArea','Zoom',fZoom);
  INI.WriteInteger('DrawArea','ZoomLeft',fZoomLeft);
  INI.WriteInteger('DrawArea','ZoomTop',fZoomTop);
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
  INI.WriteInteger('Settings','UndoLimit',fUndoLimit);
  INI.WriteInteger('Settings','BackupInterval',fBackupIntervalTicks div 1000);
  INI.WriteInteger('Settings','BackupFolderMaxSize',fBackupFolderMaxSize);
  INI.WriteInteger('Settings','BackupFolderRetentionTime',fBackupFolderRetentionTime);
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
  INI.WriteInteger('Inks','CGradCenterX',fCGradCenterX);
  INI.WriteInteger('Inks','CGradCenterY',fCGradCenterY);
  INI.WriteInteger('Inks','CGradRadius',fCGradRadius);
  INI.WriteInteger('Inks','RGradCenterX',fRGradCenterX);
  INI.WriteInteger('Inks','RGradCenterY',fRGradCenterY);
  INI.WriteInteger('Inks','RGradRepetitions',fRGradRepetitions);
  INI.WriteInteger('Inks','RGradRotation',fRGradRotation);
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

end.

