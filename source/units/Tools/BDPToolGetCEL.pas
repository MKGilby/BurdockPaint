{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPToolGetCEL;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolGetCEL }

  TBDToolGetCEL=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
  end;

implementation

uses sdl2, BDPShared, BDPRegion;

{ TBDToolGetCEL }

constructor TBDToolGetCEL.Create;
begin
  inherited Create;
  fName:='GETCEL';
  fHint:=uppercase('Get a part of the image into a temporary image.');
end;

function TBDToolGetCEL.Click(x,y,button:integer):boolean;
var i:integer;
begin
  if button=SDL_BUTTON_LEFT then begin
    case fState of
      0:begin
          fSX:=x;
          fSY:=y;
          Result:=true;
          fState:=1;
//          Log.Trace(Format('GETCEL1 fSX=%d, fSY=%d',[fSX,fSY]));
        end;
      1:begin
//          Log.Trace(Format('GETCEL2 fSX=%d, fSY=%d, x=%d, y=%d',[fSX,fSY,x,y]));
          if x<fSX then begin i:=x;x:=fSX;fSX:=i;end;
          if y<fSY then begin i:=y;y:=fSY;fSY:=i;end;
          if Assigned(Project.CELImage) then Project.CELImage.Free;
          Project.CELImage:=TBDRegion.Create(x-fSX+1,y-fSY+1);
          Project.CELImage.PutImagePart(0,0,fSX,fSY,x-fSX+1,y-fSY+1,Project.CurrentRegion);
          Project.CELImage.Left:=fSX;
          Project.CELImage.Top:=fSY;
          fState:=-1;
          MessageQueue.AddMessage(MSG_GETCELFINISHED);
          Result:=true;
        end;
    end;
  end
  else if Button=SDL_BUTTON_RIGHT then begin
    fState:=0;
    MessageQueue.AddMessage(MSG_RESTORECONTROLS);
    Result:=true;
  end
  else Result:=false;
end;

function TBDToolGetCEL.MouseUp(x,y,button:integer):boolean;
begin
  Result:=true;
end;

procedure TBDToolGetCEL.Draw;
begin
  case fState of
    -1:fState:=0;  // One frame where we don't draw anything before resetting fState.
                   // This is needed to remove flickering.
    0:begin
        OverlayImage.VLine(fX,0,OverlayImage.Height,VibroColors.GetColor);
        OverlayImage.HLine(0,fY,OverlayImage.Width,VibroColors.GetColor);
        InfoBar.ShowText(inttostr(fX)+','+inttostr(fY));
      end;
    1:begin
        OverlayImage.RectangleXY(fSX,fSY,fX,fY,VibroColors.GetColor);

        InfoBar.ShowText('('+inttostr(fSX)+','+inttostr(fSY)+') '+
          'WI='+inttostr(abs(fSX-fX)+1)+' HE='+inttostr(abs(fSY-fY)+1)+' '+
          '('+inttostr(fX)+','+inttostr(fY)+') ');
      end;
  end;
end;

procedure TBDToolGetCEL.Clear;
begin
  case fState of
    0:begin
        OverlayImage.VLine(fX,0,OverlayImage.Height,0);
        OverlayImage.HLine(0,fY,OverlayImage.Width,0);
      end;
    1:begin
        OverlayImage.RectangleXY(fSX,fSY,fX,fY,0);
      end;
  end;
end;

end.

