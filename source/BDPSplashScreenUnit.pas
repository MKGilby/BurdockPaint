unit BDPSplashScreenUnit;

{$mode Delphi}

interface

uses
  vcc2_Container, mk_sdl2, MKMouse2;

type

  { TBDSplashScreen }

  TBDSplashScreen=class(TContainer)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    function KeyDown(Sender:TObject;key:integer):boolean;
    function KeyUp(Sender:TObject;key:integer):boolean;
  private
    fTexture:TStreamingTexture;
    fWindowLeft,fWindowTop:integer;
    fStart:integer;
  end;

implementation

uses
  SysUtils, SDL2, BDPSharedUnit, Font2Unit;

const
  SPLASHSCREENWIDTH=480;
  SPLASHSCREENHEIGHT=128;

{ TBDSplashScreen }

constructor TBDSplashScreen.Create;
begin
  inherited Create;
  SetBoundsWH(0,0,WINDOWWIDTH,WINDOWHEIGHT);
  fName:='SplashScreen';
  fWindowLeft:=(WINDOWWIDTH-SPLASHSCREENWIDTH) div 2;
  fWindowTop:=(WINDOWHEIGHT-SPLASHSCREENHEIGHT) div 2;
  fTexture:=TStreamingTexture.Create(SPLASHSCREENWIDTH,SPLASHSCREENHEIGHT);
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,OverlayImage.Palette[3]);
  MM.Fonts['LogoFont'].OutText(fTexture.ARGBImage,'BURDoCK PAINT',80,24,0);
  MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,'CODE: GILBY/MKSZTSZ',80,56,0);
  MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,'HUNGARY - 2023',80,88,0);
  MM.Images.ItemByName['Burdock'].CopyTo(0,0,46,52,16,(SPLASHSCREENHEIGHT-52) div 2,fTexture.ARGBImage,true);
  OnKeyDown:=KeyDown;
  OnKeyUp:=KeyUp;
  Visible:=true;
  fStart:=SDL_GetTicks;
end;

destructor TBDSplashScreen.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TBDSplashScreen.Draw;
begin
  fTexture.Update;
  PutTexture(fWindowLeft,fWindowTop,fTexture);
  if SDL_GetTicks-fStart>5000 then fVisible:=false;
end;

function TBDSplashScreen.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

function TBDSplashScreen.KeyUp(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

end.

