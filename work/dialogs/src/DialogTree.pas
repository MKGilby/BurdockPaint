unit DialogTree;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TDialogItem=class;

  { TDialogItemList }

  TDialogItemList=class(TFPGObjectList<TDialogItem>)
    function GetMaxWidth:integer;
    function GetSumWidth:integer;
    function GetMaxHeight:integer;
    function GetSumHeight:integer;
  end;

  { TDialogItem }

  TDialogItem=class
    procedure CalculatePositions(pTop,pLeft:integer;pParentWidth:integer=-1;pParentHeight:integer=-1); virtual; abstract;
    procedure AddItem(pItem:TDialogItem); virtual;
  strict protected
    fItems:TDialogItemList;
    fTop,fLeft:integer;
    fWidth,fHeight:integer;
    fParentWidth,fParentHeight:integer;
  public
//    property Items:TDialogItemList read fItems;
    property Width:integer read fWidth;
    property Height:integer read fHeight;
  end;

  { TDialog }

  TDialog=class(TDialogItem)
    constructor Create(iName,iCaption,iClassName:string);
    destructor Destroy; override;
    procedure CalculatePositions(pTop,pLeft:integer;pParentWidth:integer=-1;pParentHeight:integer=-1); override;
    procedure AddItem(pItem:TDialogItem); override;
  private
    fName,fCaption,fClassName:string;
  public
    property Name:string read fName;
    property Caption:string read fCaption;
    property Class_Name:string read fClassName;
  end;

  { TLabel }

  TLabel=class(TDialogItem)
    constructor Create(iCaption:string);
    procedure CalculatePositions(pTop,pLeft:integer;pParentWidth:integer=-1;pParentHeight:integer=-1); override;
  private
    fCaption:string;
  end;

  { THorizontalSlider }

  THorizontalSlider=class(TDialogItem)
    constructor Create(iName:string;iMinValue,iMaxValue:integer;iSettingField:string);
    procedure CalculatePositions(pTop,pLeft:integer;pParentWidth:integer=-1;pParentHeight:integer=-1); override;
  private
    fName,fSettingField:string;
    fMinValue,fMaxValue:integer;
  public
    property Name:string read fName;
    property SettingField:string read fSettingField;
    property MinValue:integer read fMinValue;
    property MaxValue:integer read fMaxValue;
  end;

  { TCheckBox }

  TCheckBox=class(TDialogItem)
    constructor Create(iName,iCaption,iSettingField,iHint:string);
    procedure CalculatePositions(pTop,pLeft:integer;pParentWidth:integer=-1;pParentHeight:integer=-1); override;
  private
    fName,fCaption,fSettingField,fHint:string;
  end;

  { THorizontalSplit }

  THorizontalSplit=class(TDialogItem)
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(pItem:TDialogItem); override;
    procedure CalculatePositions(pTop,pLeft:integer;pParentWidth:integer=-1;pParentHeight:integer=-1); override;
  end;

  { TVerticalSplit }

  TVerticalSplit=class(TDialogItem)
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(pItem:TDialogItem); override;
    procedure CalculatePositions(pTop,pLeft:integer;pParentWidth:integer=-1;pParentHeight:integer=-1); override;
  end;

  { TButton }

  TButton=class(TDialogItem)
    constructor Create(iName,iCaption:string;iSave,iClose:boolean;iHint:string);
    procedure CalculatePositions(pTop,pLeft:integer;pParentWidth:integer=-1;pParentHeight:integer=-1); override;
  private
    fName,fCaption,fHint:string;
    fSave,fClose:boolean;
  end;

implementation

uses Lists, Logger, CodeGenerator;

const
  DEFSIZESFILENAME='DefaultControlSizes.txt';

var
  DefaultSizes:TCounterList;
  CodeGenerator:TCodeGenerator;

{ TDialogItemList }

function TDialogItemList.GetMaxWidth: integer;
var i:integer;
begin
  Result:=0;
  for i:=0 to Count-1 do
    if Self[i].Width>Result then Result:=Self[i].Width;
end;

function TDialogItemList.GetSumWidth: integer;
var i:integer;
begin
  Result:=DefaultSizes['MARGINS']*2-DefaultSizes['CONTROLSPACING'];
  for i:=0 to Count-1 do
    Result+=Self[i].Width+DefaultSizes['CONTROLSPACING'];
end;

function TDialogItemList.GetMaxHeight: integer;
var i:integer;
begin
  Result:=0;
  for i:=0 to Count-1 do
    if Self[i].Height>Result then Result:=Self[i].Height;
end;

function TDialogItemList.GetSumHeight: integer;
var i:integer;
begin
  Result:=DefaultSizes['MARGINS']*2-DefaultSizes['CONTROLSPACING'];
  for i:=0 to Count-1 do
    Result+=Self[i].Height+DefaultSizes['CONTROLSPACING'];
end;

procedure TDialogItem.AddItem(pItem:TDialogItem);
begin
  if Assigned(fItems) then fItems.Add(pItem);
end;


{ TDialog }

constructor TDialog.Create(iName,iCaption,iClassName:string);
begin
  fItems:=TDialogItemList.Create;
  fName:=iName;
  fCaption:=uppercase(iCaption);
  fClassName:=iClassName;
  CodeGenerator:=TCodeGenerator.Create(Self);
  fLeft:=-1;
  fTop:=-1;
  fWidth:=-1;
  fHeight:=-1;
end;

destructor TDialog.Destroy;
begin
  if Assigned(CodeGenerator) then begin
    CodeGenerator.Save('..\..\source\units\Dialogs\'+fName+'.pas');
    CodeGenerator.Free;
  end;
  fItems.Free;
  inherited Destroy;
end;

procedure TDialog.CalculatePositions(pTop,pLeft:integer; pParentWidth:integer;
  pParentHeight:integer);
var i:integer;
begin
  fWidth:=fItems.GetMaxWidth+DefaultSizes['MARGINS']*2;
  i:=length(fCaption)*18+3;
  if i>fWidth then fWidth:=i;
  fHeight:=fItems.GetSumHeight+DefaultSizes['DIALOGCAPTIONHEIGHT']+DefaultSizes['MARGINS'];
  CodeGenerator.CreateCode.Add(Format('  inherited Create(%d,%d);',[Width,Height]));
  CodeGenerator.CreateCode.Add(Format('  fName:=''%s'';',[fName]));
  CodeGenerator.CreateCode.Add(Format('  fCaption:=''%s'';',[fCaption]));
  CodeGenerator.CreateCode.Add('  OnShow:=Show;');
  CodeGenerator.CreateCode.Add('  Visible:=false;');
  CodeGenerator.CreateCode.Add('  MouseObjects.Add(Self);');
  CodeGenerator.CreateCode.Add('');
  pTop+=DefaultSizes['DIALOGCAPTIONHEIGHT']+DefaultSizes['MARGINS'];
  for i:=0 to fItems.Count-1 do begin
    fItems[i].CalculatePositions(pTop,pLeft,Width,-1);
    pTop+=fItems[i].Height+DefaultSizes['CONTROLSPACING'];
  end;
end;

procedure TDialog.AddItem(pItem:TDialogItem);
begin
  inherited AddItem(pItem);
  fWidth:=fItems.GetMaxWidth+DefaultSizes['MARGINS']*2;
  fHeight:=fItems.GetSumHeight+DefaultSizes['DIALOGCAPTIONHEIGHT']+DefaultSizes['MARGINS'];
end;


{ TLabel }

constructor TLabel.Create(iCaption: string);
begin
  fCaption:=uppercase(iCaption);
  fWidth:=length(fCaption)*18-3;
  fHeight:=18;
end;

procedure TLabel.CalculatePositions(pTop,pLeft:integer; pParentWidth:integer;
  pParentHeight:integer);
begin
  fTop:=pTop;
  fLeft:=pLeft;
  fParentWidth:=pParentWidth;
  if fParentWidth=-1 then raise Exception.Create('ParentWidth shouldn''t be -1!');
  fParentHeight:=pParentHeight;
  CodeGenerator.RedrawCode.Add(Format('  MM.Fonts[''Black''].OutText(fImage,''%s'',%d,%d,1);',
    [fCaption,fLeft+fParentWidth div 2,fTop]));
end;


{ THorizontalSlider }

constructor THorizontalSlider.Create(iName: string; iMinValue, iMaxValue: integer;
  iSettingField: string);
begin
  fName:=iName;
  fMinValue:=iMinValue;
  fMaxValue:=iMaxValue;
  fSettingField:=iSettingField;
  CodeGenerator.UsesList.Add('BDPSliders');
  CodeGenerator.Privates.Add(fName+':TBDHorizontalSlider;');
  fWidth:=DefaultSizes['SLIDERWIDTH'];
  fHeight:=DefaultSizes['SLIDERHEIGHT'];
end;

procedure THorizontalSlider.CalculatePositions(pTop,pLeft:integer;
  pParentWidth:integer; pParentHeight:integer);
var Left:integer;
begin
  fTop:=pTop;
  fLeft:=pLeft;
  fParentWidth:=pParentWidth;
  if fParentWidth=-1 then raise Exception.Create('ParentWidth shouldn''t be -1!');
  fParentHeight:=pParentHeight;
  Left:=fLeft+(fParentWidth-fWidth) div 2;
  CodeGenerator.CreateCode.Add(Format(
    '  %s:=TBDHorizontalSlider.Create(fLeft+%d,fTop+%d,%d,%d);',[fName,Left,fTop,Width,Height]));

  CodeGenerator.CreateCode.Add(Format('  with %s do begin',[fName]));
  CodeGenerator.CreateCode.Add('    ZIndex:=MODALDIALOG_ZINDEX+1;');
  CodeGenerator.CreateCode.Add(Format('    Name:=''%s'';',[fName]));
  CodeGenerator.CreateCode.Add(Format('    MinValue:=%d;',[fMinValue]));
  CodeGenerator.CreateCode.Add(Format('    MaxValue:=%d;',[fMaxValue]));
  CodeGenerator.CreateCode.Add(Format('    Position:=Settings.%s;',[fSettingField]));
  CodeGenerator.CreateCode.Add('  end;');
  CodeGenerator.CreateCode.Add(Format('  AddChild(%s);',[fName]));
  CodeGenerator.CreateCode.Add('');
  if fSettingField<>'' then begin
    CodeGenerator.SaveCode.Add(Format('  Settings.%s:=%s.Position;',[fSettingField,fName]));
    CodeGenerator.ShowCode.Add(Format('  %s.Position:=Settings.%s;',[fName,fSettingField]));
  end;
end;


{ TCheckBox }

constructor TCheckBox.Create(iName,iCaption,iSettingField,iHint:string);
begin
  fName:=iName;
  fCaption:=uppercase(iCaption);
  fSettingField:=iSettingField;
  fHint:=uppercase(iHint);
  CodeGenerator.UsesList.Add('BDPCheckBox');
  CodeGenerator.Privates.Add(fName+':TBDCheckBox;');
  fWidth:=DefaultSizes['CHECKBOXBOXWIDTH']+DefaultSizes['CONTROLSPACING']+length(fCaption)*18-3;
  fHeight:=DefaultSizes['CHECKBOXHEIGHT'];
end;

procedure TCheckBox.CalculatePositions(pTop,pLeft:integer;
  pParentWidth:integer; pParentHeight:integer);
var Left:integer;
begin
  fTop:=pTop;
  fLeft:=pLeft;
  fParentWidth:=pParentWidth;
  if fParentWidth=-1 then raise Exception.Create('ParentWidth shouldn''t be -1!');
  fParentHeight:=pParentHeight;
  Left:=fLeft+(fParentWidth-fWidth) div 2;
  CodeGenerator.CreateCode.Add(Format('  %s:=TBDCheckBox.Create(fLeft+%d,fTop+%d,%d,%d,''%s'',''%s'');',
    [fName,Left,fTop,fWidth,fHeight,uppercase(fCaption),uppercase(fHint)]));
  CodeGenerator.CreateCode.Add(Format('  with %s do begin',[fName]));
  CodeGenerator.CreateCode.Add('    ZIndex:=MODALDIALOG_ZINDEX+1;');
  CodeGenerator.CreateCode.Add(Format('    Name:=''%s'';',[fName]));
  CodeGenerator.CreateCode.Add('  end;');
  CodeGenerator.CreateCode.Add(Format('  AddChild(%s);',[fName]));
  CodeGenerator.CreateCode.Add('');
  if fSettingField<>'' then begin
    CodeGenerator.SaveCode.Add(Format('  Settings.%s:=%s.Selected;',[fSettingField,fName]));
    CodeGenerator.ShowCode.Add(Format('  %s.Selected:=Settings.%s;',[fName,fSettingField]));
  end;
end;


{ THorizontalSplit }

constructor THorizontalSplit.Create;
begin
  fItems:=TDialogItemList.Create;
end;

destructor THorizontalSplit.Destroy;
begin
  fItems.Free;
  inherited Destroy;
end;

procedure THorizontalSplit.AddItem(pItem:TDialogItem);
begin
  inherited AddItem(pItem);
  fWidth:=fItems.GetSumWidth;
  fHeight:=fItems.GetMaxHeight;
end;

procedure THorizontalSplit.CalculatePositions(pTop,pLeft:integer;
  pParentWidth:integer; pParentHeight:integer);
var i:integer;
begin
  fTop:=pTop;
  fParentWidth:=pParentWidth;
  if fParentWidth=-1 then raise Exception.Create('ParentWidth shouldn''t be -1!');
  fParentHeight:=pParentHeight;
  fWidth:=fItems.GetSumWidth;
  fHeight:=fItems.GetMaxHeight;
  fLeft:=pLeft+(fParentWidth-fWidth) div 2+DefaultSizes['MARGINS'];
  for i:=0 to fItems.Count-1 do begin
    fItems[i].CalculatePositions(pTop,fLeft,fItems[i].Width,Height);
    fLeft+=fItems[i].Width+DefaultSizes['CONTROLSPACING'];
  end;
end;


{ TVerticalSplit }

constructor TVerticalSplit.Create;
begin
  fItems:=TDialogItemList.Create;
end;

destructor TVerticalSplit.Destroy;
begin
  fItems.Free;
  inherited Destroy;
end;

procedure TVerticalSplit.AddItem(pItem:TDialogItem);
begin
  inherited AddItem(pItem);
  fWidth:=fItems.GetMaxWidth;
  fHeight:=fItems.GetSumHeight;
end;

procedure TVerticalSplit.CalculatePositions(pTop,pLeft:integer;
  pParentWidth:integer; pParentHeight:integer);
var i:integer;
begin
  fTop:=pTop;
  fLeft:=pLeft;
  fParentWidth:=pParentWidth;
  if fParentWidth=-1 then raise Exception.Create('ParentWidth shouldn''t be -1!');
  fParentHeight:=pParentHeight;
  fWidth:=fItems.GetMaxWidth;
  fHeight:=fItems.GetSumHeight;
  for i:=0 to fItems.Count-1 do begin
    fItems[i].CalculatePositions(pTop,pLeft,Width,-1);
    pTop+=fItems[i].Height;
  end;
end;


{ TButton }

constructor TButton.Create(iName,iCaption:string; iSave,iClose:boolean;
  iHint:string);
begin
  fName:=iName;
  fCaption:=uppercase(iCaption);
  fSave:=iSave;
  fClose:=iClose;
  fHint:=uppercase(iHint);
  CodeGenerator.UsesList.Add('BDPButton');
  CodeGenerator.OnClicks.Add(fName);
  CodeGenerator.CreateVar.Add('tmp:TBDButton;');
  fWidth:=DefaultSizes['BUTTONWIDTH'];
  fHeight:=DefaultSizes['BUTTONHEIGHT'];
end;

procedure TButton.CalculatePositions(pTop,pLeft:integer; pParentWidth:integer;
  pParentHeight:integer);
var Left:integer;
begin
  fTop:=pTop;
  fLeft:=pLeft;
  fParentWidth:=pParentWidth;
  if fParentWidth=-1 then raise Exception.Create('ParentWidth shouldn''t be -1!');
  fParentHeight:=pParentHeight;
  Left:=fLeft+(fParentWidth-fWidth) div 2;
  CodeGenerator.CreateCode.Add(Format('  tmp:=TBDButton.Create(fLeft+%d,fTop+%d,%d,%d,''%s'',''%s'');',
    [Left,fTop,fWidth,fHeight,fCaption,fHint]));
  CodeGenerator.CreateCode.Add(Format('  tmp.OnClick:=%sClick;',[fName]));
  CodeGenerator.CreateCode.Add('  tmp.ZIndex:=MODALDIALOG_ZINDEX+1;');
  CodeGenerator.CreateCode.Add('  AddChild(tmp);');
  CodeGenerator.CreateCode.Add('');
  if fSave then
    CodeGenerator.OnClickProcs.Add(fName+'=SaveSettings;');
  if fClose then
    CodeGenerator.OnClickProcs.Add(fName+'=Hide;');
end;


initialization
begin
  DefaultSizes:=TCounterList.Create;
  {$ifdef DEBUG}
  if FileExists('..\'+DEFSIZESFILENAME) then DefaultSizes.LoadFromFile('..\'+DEFSIZESFILENAME);
  {$else}
  if FileExists(DEFSIZESFILENAME) then DefaultSizes.LoadFromFile(DEFSIZESFILENAME);
  {$endif}
end;

finalization
begin
  DefaultSizes.Free
end;

end.

