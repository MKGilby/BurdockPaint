{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPToolConfigureRGrad;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolConfigureRGrad }

  TBDToolConfigureRGrad=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  end;

implementation

uses sdl2, BDPShared;

{ TBDToolConfigureRGrad }

constructor TBDToolConfigureRGrad.Create;
begin
  inherited Create;
  fName:='CONFRG';
  fHint:=uppercase('Select center.');
end;

function TBDToolConfigureRGrad.Click(x,y,button:integer):boolean;
begin
  if button=SDL_BUTTON_LEFT then begin
    InfoBar.ShowText('');
    Settings.TempRGradCenterX:=x;
    Settings.TempRGradCenterY:=y;
    MessageQueue.AddMessage(MSG_CONFIGRGRADCENTERFINISHED);
    Result:=true;
  end
  else if Button=SDL_BUTTON_RIGHT then begin  // Right button
    InfoBar.ShowText('');
    Settings.TempRGradCenterX:=Settings.RGradCenterX;
    Settings.TempRGradCenterY:=Settings.RGradCenterY;
    MessageQueue.AddMessage(MSG_CONFIGRGRADCENTERFINISHED);
    Result:=true;
  end else Result:=false;
end;

procedure TBDToolConfigureRGrad.Draw;
begin
  OverlayImage.VLine(fX,0,OverlayImage.Height,VibroColors.GetColor);
  OverlayImage.HLine(0,fY,OverlayImage.Width,VibroColors.GetColor);
  InfoBar.ShowText(Format('%d,%d - CONFIGURE R GRAD - '#132' SELECT CENTER '#133' CANCEL',[fX,fY]));
end;

procedure TBDToolConfigureRGrad.Clear;
begin
  OverlayImage.VLine(fX,0,OverlayImage.Height,0);
  OverlayImage.HLine(0,fY,OverlayImage.Width,0);
end;

end.

