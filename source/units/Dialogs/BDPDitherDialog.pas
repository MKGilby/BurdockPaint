{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Generated on 2024.06.04

unit BDPDitherDialog;

{$mode delphi}

interface

uses
  SysUtils, BDPModalDialog, BDPButton, BDPSliders;

type

  { TBDDitherDialog }

  TBDDitherDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
  private
    ConfDitherSlider:TBDHorizontalSlider;
    procedure SaveSettings;
    procedure Show(Sender:TObject);
    procedure ConfDitherOKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure ConfDitherCancelButtonClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses BDPShared, MKMouse2;

{ TBDDitherDialog }

constructor TBDDitherDialog.Create;
var tmp:TBDButton;
begin
  inherited Create(489,144);
  fName:='BDPDitherDialog';
  fCaption:='CONFIGURE DITHER PARAMETERS';
  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);

  ConfDitherSlider:=TBDHorizontalSlider.Create(fLeft+84,fTop+57,320,33);
  with ConfDitherSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ConfDitherSlider';
    MinValue:=1;
    MaxValue:=100;
    Position:=Settings.DitherStrength;
  end;
  AddChild(ConfDitherSlider);

  tmp:=TBDButton.Create(fLeft+113,fTop+99,127,27,'OK','APPLY VALUES');
  tmp.OnClick:=ConfDitherOKButtonClick;
  tmp.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(tmp);

  tmp:=TBDButton.Create(fLeft+249,fTop+99,127,27,'CANCEL','CLOSE DIALOG');
  tmp.OnClick:=ConfDitherCancelButtonClick;
  tmp.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(tmp);

end;

procedure TBDDitherDialog.Redraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'DITHER STRENGTH',244,30,1);
end;

procedure TBDDitherDialog.SaveSettings;
begin
  Settings.DitherStrength:=ConfDitherSlider.Position;
end;

procedure TBDDitherDialog.ConfDitherOKButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  SaveSettings;
  Hide;
end;

procedure TBDDitherDialog.ConfDitherCancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Hide;
end;

procedure TBDDitherDialog.Show(Sender:TObject);
begin
  ConfDitherSlider.Position:=Settings.DitherStrength;
end;

end.
