unit BDPControlsUnit;

{$mode Delphi}{$H+}

interface

uses
  vcc_Container2, BDPButtonUnit, mk_sdl2;

type

  { TBDControls }

  TBDControls=class(TContainer)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
//    procedure ActivateToolButton(index:integer);
//    procedure ActivateInkButton(index:integer);
//    procedure ToggleFilledButton;
    function MouseEnter(Sender:TObject;{%H-}x,{%H-}y:integer):boolean;
    function MouseLeave(Sender:TObject;{%H-}x,{%H-}y:integer):boolean;
    function Click(Sender:TObject;{%H-}x, {%H-}y, {%H-}buttons: integer):boolean;
    function FilledButtonClick(Sender:TObject;{%H-}x, {%H-}y, {%H-}buttons: integer):boolean;
//    function ToolEditorActivatorClick(Sender:TObject;{%H-}x, {%H-}y, {%H-}buttons: integer):boolean;
  private
    fTexture:TStreamingTexture;
    fToolButtons:array of TBDButton;
    fInkButtons:array of TBDButton;
    fFilledButton:TBDButton;
//    fToolEditorActivator:TBDInvisibleButton;
  end;

implementation

uses SysUtils, BDPSharedUnit, sdl2;

{ TBDControls }

constructor TBDControls.Create;
begin
  inherited Create;
  fLeft:=0;
  fTop:=WINDOWHEIGHT-CONTROLSHEIGHT;
  fWidth:=WINDOWWIDTH;
  fHeight:=CONTROLSHEIGHT;
  fTexture:=TStreamingTexture.Create(WINDOWWIDTH,CONTROLSHEIGHT);
  fTexture.ARGBImage.Clear;
  fFilledButton:=TBDButton.Create(
    fTexture.ARGBImage,
    fLeft+3,
    fTop+6,
    NORMALBUTTONWIDTH,
    'TEST',
    '',
    'This is a test button');
  fFilledButton.Selected:=false;
  fFilledButton.OnClick:=FilledButtonClick;
  fFilledButton.ParentX:=fLeft;
  fFilledButton.ParentY:=fTop;
  fFilledButton.ZIndex:=15;
  AddChild(fFilledButton);
  fVisible:=true;
  OnMouseEnter:=MouseEnter;
  OnMouseLeave:=MouseLeave;
  OnClick:=Click;
  fName:='Controls';
end;

destructor TBDControls.Destroy;
begin
//  if Assigned(fFilledButton) then FreeAndNil(fFilledButton);
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TBDControls.Draw;
begin
  if fVisible then begin
    fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,SystemColors[1].c32);
    fTexture.ARGBImage.Bar(0,3,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height-3,SystemColors[2].c32);
    inherited Draw;
    fTexture.Update;
    PutTexture(fLeft,fTop,fTexture);
  end;
end;

function TBDControls.MouseEnter(Sender:TObject; x,y:integer):boolean;
begin
  if fVisible then SDL_ShowCursor(SDL_ENABLE);
  Result:=false;
end;

function TBDControls.MouseLeave(Sender:TObject; x,y:integer):boolean;
begin
  if fVisible then SDL_ShowCursor(SDL_DISABLE);
  Result:=false;
end;

function TBDControls.Click(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TBDControls.FilledButtonClick(Sender:TObject; x,y,buttons:integer
  ):boolean;
begin
  if Sender is TBDButton then with Sender as TBDButton do
    fSelected:=not fSelected;
  Result:=true;
end;

end.

