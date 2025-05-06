{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Generated on 2025.05.06

unit BDPImageResizeDialog;

{$mode delphi}

interface

uses
  SysUtils, BDPModalDialog, BDPButton, BDPSliders;

type

  { TBDImageResizeDialog }

  TBDImageResizeDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
  private
    fImageResizeCancelButton:TBDButton;
    fImageResizeOKButton:TBDButton;
    ImageResizeHeightSlider:TBDHorizontalSlider;
    ImageResizeWidthSlider:TBDHorizontalSlider;
    procedure SaveSettings;
    procedure Show(Sender:TObject);
    procedure ImageResizeOKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure ImageResizeCancelButtonClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses BDPShared, MKMouse2;

{ TBDImageResizeDialog }

constructor TBDImageResizeDialog.Create;
begin
  inherited Create(338,231);
  fName:='BDPImageResizeDialog';
  fCaption:='RESIZE IMAGE';
  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);

  ImageResizeWidthSlider:=TBDHorizontalSlider.Create(fLeft+9,fTop+57,320,33);
  with ImageResizeWidthSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ImageResizeWidthSlider';
    MinValue:=32;
    MaxValue:=2048;
    Position:=Settings.TempInt01;
  end;
  AddChild(ImageResizeWidthSlider);

  ImageResizeHeightSlider:=TBDHorizontalSlider.Create(fLeft+9,fTop+126,320,33);
  with ImageResizeHeightSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ImageResizeHeightSlider';
    MinValue:=32;
    MaxValue:=2048;
    Position:=Settings.TempInt02;
  end;
  AddChild(ImageResizeHeightSlider);

  fImageResizeOKButton:=TBDButton.Create(fLeft+37,fTop+186,127,27,'OK','RESIZE IMAGE. PARTS OUTSIDE NEW SIZE WILL BE LOST!');
  fImageResizeOKButton.OnClick:=ImageResizeOKButtonClick;
  fImageResizeOKButton.Name:='ImageResizeOKButton';
  fImageResizeOKButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fImageResizeOKButton);

  fImageResizeCancelButton:=TBDButton.Create(fLeft+173,fTop+186,127,27,'CANCEL','DON''T RESIZE IMAGE');
  fImageResizeCancelButton.OnClick:=ImageResizeCancelButtonClick;
  fImageResizeCancelButton.Name:='ImageResizeCancelButton';
  fImageResizeCancelButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fImageResizeCancelButton);
end;

procedure TBDImageResizeDialog.Redraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'NEW WIDTH',169,30,1);
  MM.Fonts['Black'].OutText(fImage,'NEW HEIGHT',169,99,1);
end;

procedure TBDImageResizeDialog.SaveSettings;
begin
  Settings.TempInt01:=ImageResizeWidthSlider.Position;
  Settings.TempInt02:=ImageResizeHeightSlider.Position;
end;

procedure TBDImageResizeDialog.ImageResizeOKButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  SaveSettings;
  MessageQueue.AddMessage(MSG_RESIZEIMAGECLOSED);
  Hide;
end;

procedure TBDImageResizeDialog.ImageResizeCancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Hide;
end;

procedure TBDImageResizeDialog.Show(Sender:TObject);
begin
  ImageResizeHeightSlider.Position:=Settings.TempInt02;
  ImageResizeWidthSlider.Position:=Settings.TempInt01;
end;

end.
