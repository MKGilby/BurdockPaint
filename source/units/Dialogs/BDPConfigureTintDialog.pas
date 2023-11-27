unit BDPConfigureTintDialog;

{$mode Delphi}

interface

uses
  SysUtils, BDPSliders, BDPModalDialog, BDPCheckBox;

type

  { TBDConfigureTintDialog }

  TBDConfigureTintDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure CancelButtonClick(Sender:TObject;x,y,buttons:integer);
  private
    fStrengthSlider:TBDHorizontalSlider;
    fCELAsMaskCheckbox:TBDCheckBox;
    procedure Show(Sender:TObject);
  end;

implementation

uses BDPShared, BDPButton, MKMouse2;

const
  TINTDIALOGWIDTH=480;
  STRENGTHTEXTTOP=MODALDIALOGCAPTIONHEIGHT+9;
  SLIDERTOP=STRENGTHTEXTTOP+15+9;
  CHECKBOXTOP=SLIDERTOP+DIALOGSLIDERHEIGHT+9;
  BUTTONSTOP=CHECKBOXTOP+27+9;
  TINTDIALOGHEIGHT=BUTTONSTOP+NORMALBUTTONHEIGHT+9+3;

{ TBDConfigureTintDialog }

constructor TBDConfigureTintDialog.Create;
var tmp:TBDButton;
begin
  inherited Create(TINTDIALOGWIDTH,TINTDIALOGHEIGHT);
  fName:='ConfigureTintDialog';
  fCaption:='CONFIGURE TINT PARAMETERS';
  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);

  fStrengthSlider:=TBDHorizontalSlider.Create(
    fLeft+(TINTDIALOGWIDTH-DIALOGSLIDERWIDTH) div 2, fTop+SLIDERTOP, DIALOGSLIDERWIDTH, DIALOGSLIDERHEIGHT);
  with fStrengthSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='TintStrSlider';
    MinValue:=0;
    MaxValue:=100;
    Position:=Settings.TintStrength;
  end;
  AddChild(fStrengthSlider);

  fCELAsMaskCheckbox:=TBDCheckBox.Create(
    fLeft+3+9, fTop+CHECKBOXTOP, Width-(2*(3+9)),27,'PUTCEL USE CEL AS MASK','... NOT AS COLOR SOURCE');
  with fCELAsMaskCheckbox do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='TintCheckBox';
  end;
  AddChild(fCELAsMaskCheckbox);

  tmp:=TBDButton.Create(
    fLeft+(TINTDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2),
    fTop+BUTTONSTOP,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'OK','APPLY VALUES');
  tmp.OnClick:=OKButtonClick;
  tmp.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(tmp);

  tmp:=TBDButton.Create(
    fLeft+(TINTDIALOGWIDTH div 3*2-NORMALBUTTONWIDTH div 2),
    fTop+BUTTONSTOP,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'CANCEL','CLOSE DIALOG');
  tmp.OnClick:=CancelButtonClick;
  tmp.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(tmp);

end;

procedure TBDConfigureTintDialog.ReDraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'TINT STRENGTH',Width div 2,STRENGTHTEXTTOP,1);
end;

procedure TBDConfigureTintDialog.OKButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Settings.TintStrength:=fStrengthSlider.Position;
  Settings.TintCELAsMask:=fCELAsMaskCheckbox.Selected;
  Hide;
end;

procedure TBDConfigureTintDialog.CancelButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Hide;
end;

procedure TBDConfigureTintDialog.Show(Sender:TObject);
begin
  fStrengthSlider.Position:=Settings.TintStrength;
  fCELAsMaskCheckbox.Selected:=Settings.TintCELAsMask;
end;

end.

