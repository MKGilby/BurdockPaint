{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPModalDialog;

{$mode Delphi}

interface

uses
  SysUtils, mk_sdl2, vcc2_ContainerStatic, MKMouse2;

type

  { TBDModalOverlay }

  TBDModalOverlay=class(TMouseObject)
    constructor Create;
    procedure Draw; override;
  private
    DarkBar:TTexture;
  end;

  { TBDModalDialog }

  TBDModalDialog=class(TContainerStatic)
    constructor Create(iWidth,iHeight:integer);
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
  protected
    fCaption:string;
    procedure ReDraw; override;
  private
    procedure fSetCaption(value:string);
    function KeyDown(Sender:TObject;key:integer):boolean;
    function KeyUp(Sender:TObject;key:integer):boolean;
  public
    property Caption:string read fCaption write fSetCaption;
  end;

implementation

uses BDPShared, ARGBImageUnit;

{ TBDModalOverlay }

constructor TBDModalOverlay.Create;
begin
  inherited Create;
  SetBounds(0,0,WINDOWWIDTH,WINDOWHEIGHT);
  ZIndex:=MODALDIALOG_ZINDEX-1;
  fName:='ModalOverlay';
  DarkBar:=MM.Textures.ItemByName['DarkBar'];
  Visible:=false;
end;

procedure TBDModalOverlay.Draw;
begin
  bar(0,0,WINDOWWIDTH,WINDOWHEIGHT,DarkBar);
end;

{ TBDModalDialog }

constructor TBDModalDialog.Create(iWidth,iHeight:integer);
begin
  inherited Create;
  fLeft:=(WINDOWWIDTH-iWidth) div 2;
  fTop:=(WINDOWHEIGHT-iHeight) div 2;
  Width:=iWidth;
  Height:=iHeight;
  OnKeyDown:=KeyDown;
  OnKeyUp:=KeyUp;
  ZIndex:=MODALDIALOG_ZINDEX;
  Visible:=false;
end;

destructor TBDModalDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TBDModalDialog.ReDraw;
begin
  // Panel border
  fImage.Bar(0,0,Width,MODALDIALOGCAPTIONHEIGHT,SystemPalette[SYSTEMCOLORDARK]);
  fImage.Bar(0,Height-3,fImage.Width,3,SystemPalette[SYSTEMCOLORDARK]);
  fImage.Bar(0,MODALDIALOGCAPTIONHEIGHT,3,Height-3-MODALDIALOGCAPTIONHEIGHT,SystemPalette[SYSTEMCOLORDARK]);
  fImage.Bar(Width-3,MODALDIALOGCAPTIONHEIGHT,3,fImage.Height-3-MODALDIALOGCAPTIONHEIGHT,SystemPalette[SYSTEMCOLORDARK]);
  // Panel caption
  MM.Fonts['Black'].OutText(fImage,fCaption,Width div 2,3,1);
  // Panel background
  fImage.Bar(3,MODALDIALOGCAPTIONHEIGHT,Width-6,Height-3-MODALDIALOGCAPTIONHEIGHT,SystemPalette[SYSTEMCOLORMID]);
end;

function TBDModalDialog.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

function TBDModalDialog.KeyUp(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

procedure TBDModalDialog.fSetCaption(value:string);
begin
  if fCaption<>value then begin
    fCaption:=value;
    fNeedRedraw:=true;
  end;
end;

procedure TBDModalDialog.Show;
begin
  inherited Show;
  ModalOverlay.Visible:=true;
end;

procedure TBDModalDialog.Hide;
begin
  ModalOverlay.Visible:=false;
  inherited Hide;
end;

end.

