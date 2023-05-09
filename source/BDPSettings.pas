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
    fFillShapes,
    fClearKeyColor,
    fUseAlpha:boolean;
    fColorSelectorColors:array of integer;
    fActiveColorIndex:integer;
    fUndoLimit:integer;
    fDitherGradients:boolean;
    fDitherStrength:integer;
    fModernGraphics:boolean;
    fRGradCenterX,fRGradCenterY:integer;
    function fGetSelectedColor(index:integer):integer;
    function fGetSelectedTool(index:integer):string;
    procedure fSetSelectedColor(index:integer; AValue:integer);
    procedure fSetSelectedTool(index:integer;value:string);
    function fGetSelectedInk(index:integer):string;
    procedure fSetSelectedInk(index:integer;value:string);
    procedure fSetActiveColorIndex(value:integer);
  public
    property Zoom:integer read fZoom write fZoom;
    property ZoomLeft:integer read fZoomLeft write fZoomLeft;
    property ZoomTop:integer read fZoomTop write fZoomTop;
    property SelectedTools[index:integer]:string read fGetSelectedTool write fSetSelectedTool;
    property ActiveTool:integer read fActiveTool write fActiveTool;
    property SelectedInks[index:integer]:string read fGetSelectedInk write fSetSelectedInk;
    property ActiveInk:integer read fActiveInk write fActiveInk;
    property FillShapes:boolean read fFillShapes write fFillShapes;
    property ClearKeyColor:boolean read fClearKeyColor write fClearKeyColor;
    property UseAlpha:boolean read fUseAlpha write fUseAlpha;
    property ShowSplash:Boolean read fShowSplash write fShowSplash;
    property SelectedColors[index:integer]:integer read fGetSelectedColor write fSetSelectedColor;
    property ActiveColorIndex:integer read fActiveColorIndex write fSetActiveColorIndex;
    property UndoLimit:integer read fUndoLimit write fUndoLimit;
    property ModernGraphics:boolean read fModernGraphics write fModernGraphics;
    property DitherGradients:boolean read fDitherGradients write fDitherGradients;
    property DitherStrength:integer read fDitherStrength write fDitherStrength;
    property RGradCenterX:integer read fRGradCenterX write fRGradCenterX;
    property RGradCenterY:integer read fRGradCenterY write fRGradCenterY;
  end;


implementation

uses MKINIFile, BDPKeyMapping, BDPShared;

{ TSettings }

constructor TSettings.Create;
var i:integer;
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
  fSelectedInks[1]:='OPAQUE';
  fSelectedInks[2]:='L GRAD';
  fSelectedInks[3]:='L GRAD';
  fSelectedInks[4]:='H GRAD';
  fSelectedInks[5]:='H GRAD';
  fActiveInk:=0;
  fFillShapes:=false;
  fClearKeyColor:=false;
  SetLength(fColorSelectorColors,COLORSELECTORCOLORS);
  for i:=0 to COLORSELECTORCOLORS-1 do fColorSelectorColors[i]:=i;
  fActiveColorIndex:=0;
  fUndoLimit:=16;
  fDitherGradients:=false;
  fDitherStrength:=10;
  fRGradCenterX:=0;
  fRGradCenterY:=0;
end;

procedure TSettings.LoadFromFile(pFilename:String);
var INI:TIniFile;i:integer;
begin
  if not FileExists(pFilename) then exit;
  INI:=TIniFile.Create(pFilename);
  fZoom:=INI.ReadInteger('DrawArea','Zoom',2);
  fZoomLeft:=INI.ReadInteger('DrawArea','ZoomLeft',0);
  fZoomTop:=INI.ReadInteger('DrawArea','ZoomTop',0);
  fSelectedTools[0]:=INI.ReadString('BasicControls','Tool0','DRAW');
  fSelectedTools[1]:=INI.ReadString('BasicControls','Tool1','BOX');
  fSelectedTools[2]:=INI.ReadString('BasicControls','Tool2','LINE');
  fSelectedTools[3]:=INI.ReadString('BasicControls','Tool3','CIRCLE');
  fSelectedTools[4]:=INI.ReadString('BasicControls','Tool4','SEP.');
  fSelectedTools[5]:=INI.ReadString('BasicControls','Tool5','FILL');
  fActiveTool:=INI.ReadInteger('BasicControls','ActiveTool',0);
  if (fActiveTool<0) or (fActiveTool>5) then fActiveTool:=0;
  fSelectedInks[0]:=INI.ReadString('BasicControls','Ink0','OPAQUE');
  fSelectedInks[1]:=INI.ReadString('BasicControls','Ink1','OPAQUE');
  fSelectedInks[2]:=INI.ReadString('BasicControls','Ink2','L GRAD');
  fSelectedInks[3]:=INI.ReadString('BasicControls','Ink3','L GRAD');
  fSelectedInks[4]:=INI.ReadString('BasicControls','Ink4','H GRAD');
  fSelectedInks[5]:=INI.ReadString('BasicControls','Ink5','V GRAD');
  fActiveInk:=INI.ReadInteger('BasicControls','ActiveInk',0);
  if (fActiveInk<0) or (fActiveInk>5) then fActiveInk:=0;
  fFillShapes:=INI.ReadBool('BasicControls','FillShapes',false);
  fClearKeyColor:=INI.ReadBool('BasicControls','ClearKeyColor',false);
  fUseAlpha:=INI.ReadBool('BasicControls','UseAlpha',false);
  fShowSplash:=INI.ReadBool('Settings','ShowSplash',false);
  fUndoLimit:=INI.ReadInteger('Settings','UndoLimit',16);
  fModernGraphics:=INI.ReadBool('Settings','ModernGraphics',true);
  LoadKeyMap(INI);
  for i:=0 to COLORSELECTORCOLORS-1 do
    fColorSelectorColors[i]:=INI.ReadInteger('Colors',Format('Selected%d',[i]),i);
  fActiveColorIndex:=INI.ReadInteger('Colors','ActiveColor',0);
  fDitherGradients:=INI.ReadBool('Inks','DitherGradients',false);
  fDitherStrength:=INI.ReadInteger('Inks','DitherStrength',10);
  fRGradCenterX:=INI.ReadInteger('Inks','RGradCenterX',0);
  fRGradCenterY:=INI.ReadInteger('Inks','RGradCenterY',0);
  FreeAndNil(INI);
end;

procedure TSettings.SaveToFile(pFilename:String);
var INI:TIniFile;i:integer;
begin
  INI:=TIniFile.Create(pFilename,false);
  INI.WriteInteger('DrawArea','Zoom',fZoom);
  INI.WriteInteger('DrawArea','ZoomLeft',fZoomLeft);
  INI.WriteInteger('DrawArea','ZoomTop',fZoomTop);
  for i:=0 to 5 do
    INI.WriteString('BasicControls','Tool'+inttostr(i),fSelectedTools[i]);
  INI.WriteInteger('BasicControls','ActiveTool',fActiveTool);
  for i:=0 to 5 do
    INI.WriteString('BasicControls','Ink'+inttostr(i),fSelectedInks[i]);
  INI.WriteInteger('BasicControls','ActiveInk',fActiveInk);
  INI.WriteBool('BasicControls','FillShapes',fFillShapes);
  INI.WriteBool('BasicControls','ClearKeyColor',fClearKeyColor);
  INI.WriteBool('BasicControls','UseAlpha',fUseAlpha);
  INI.WriteBool('Settings','ShowSplash',fShowSplash);
  INI.WriteInteger('Settings','UndoLimit',fUndoLimit);
  INI.WriteBool('Settings','ModernGraphics',fModernGraphics);
  SaveKeyMap(INI);
  for i:=0 to COLORSELECTORCOLORS-1 do
    INI.WriteInteger('Colors',Format('Selected%d',[i]),fColorSelectorColors[i]);
  INI.WriteInteger('Colors','ActiveColor',fActiveColorIndex);
  INI.WriteBool('Inks','DitherGradients',fDitherGradients);
  INI.WriteInteger('Inks','DitherStrength',fDitherStrength);
  INI.WriteInteger('Inks','RGradCenterX',fRGradCenterX);
  INI.WriteInteger('Inks','RGradCenterY',fRGradCenterY);
  FreeAndNil(INI);
end;

function TSettings.fGetSelectedTool(index:integer):string;
begin
  if (index>=0) and (index<6) then
    Result:=fSelectedTools[index]
  else raise Exception.Create(Format('fGetSelectedTool: Index out of range! (%d)',[index]));
end;

function TSettings.fGetSelectedColor(index:integer):integer;
begin
  if (index>=0) and (index<COLORSELECTORCOLORS) then
    Result:=fColorSelectorColors[index]
  else raise Exception.Create(Format('fGetSelectedTool: Index out of range! (%d)',[index]));
end;

procedure TSettings.fSetSelectedColor(index:integer; AValue:integer);
begin
  if (index>=0) and (index<COLORSELECTORCOLORS) then
    fColorSelectorColors[index]:=AValue
  else raise Exception.Create(Format('fSetSelectedTool: Index out of range! (%d)',[index]));
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

procedure TSettings.fSetActiveColorIndex(value:integer);
begin
  if (value>=0) and (value<>fActiveColorIndex) then begin
    fActiveColorIndex:=value;
    MessageQueue.AddMessage(MSG_ACTIVECOLORINDEXCHANGED,fActiveColorIndex);
  end;
end;

end.

