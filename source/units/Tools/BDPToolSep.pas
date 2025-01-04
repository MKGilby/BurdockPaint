{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPToolSep;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolSep }

  TBDToolSep=class(TBDTool)
    constructor Create; override;
    procedure Draw; override;
    procedure Clear; override;
    function Click(x,y,button:integer):boolean; override;
    function ClickNotBoxed(x,y,button:integer):boolean;
    function ClickBoxed(x,y,button:integer):boolean;
    procedure Configure; override;
  private
    fX1,fY1,fX2,fY2:integer;
  end;

implementation

uses sdl2, BDPShared, BDPRegion;

{ TBDToolSep }

constructor TBDToolSep.Create;
begin
  inherited ;
  fName:='SEP.';
  fHint:=uppercase('Change the clicked color to ink color. '#132'SELECT '#133'CONFIGURE');
  fPinnable:=true;
end;

procedure TBDToolSep.Draw;
begin
  case fState of
    0:;
    1:begin
        OverlayImage.RectangleXY(fX1,fY1,fX,fY,VibroColors.GetColor);

        InfoBar.ShowText('SEP.: ('+inttostr(fX1)+','+inttostr(fY2)+') '+
          'WI='+inttostr(abs(fX1-fX)+1)+' HE='+inttostr(abs(fY1-fY)+1)+' '+
          '('+inttostr(fX)+','+inttostr(fY)+') - '#132'FINISH BOUNDING BOX '#133'CANCEL');
      end;
    2:begin
        OverlayImage.RectangleXY(fX1,fY1,fX2,fY2,VibroColors.GetColor);

        InfoBar.ShowText('SEP.: ('+inttostr(fX1)+','+inttostr(fY1)+') '+
          'WI='+inttostr(abs(fX2-fX1)+1)+' HE='+inttostr(abs(fY2-fY1)+1)+' '+
          '('+inttostr(fX2)+','+inttostr(fY2)+') - '#132'PICK COLOR TO CHANGE '#133'CANCEL');
      end;
  end;
end;

procedure TBDToolSep.Clear;
begin
  case fState of
    0:;
    1:OverlayImage.RectangleXY(fX1,fY1,fX,fY,0);
    2:OverlayImage.RectangleXY(fX1,fY1,fX2,fY2,0);
  end;
end;

function TBDToolSep.Click(x,y,button:integer):boolean;
begin
  if Settings.SepBoxed then
    Result:=ClickBoxed(x,y,button)
  else
    Result:=ClickNotBoxed(x,y,button);
end;

function TBDToolSep.ClickNotBoxed(x, y, button: integer): boolean;
var i,j:integer;sc:uint32;
  fLeft,fRight,fTop,fBottom:integer;
  fTempImage:TBDRegion;
begin
  if button=SDL_BUTTON_LEFT then begin
    fTempImage:=TBDRegion.Create(Project.CurrentRegion.Width,Project.CurrentRegion.Height);
    fTempImage.PutImage(0,0,Project.CurrentRegion);
    fLeft:=Project.CurrentRegion.Width;
    fRight:=-1;
    fTop:=Project.CurrentRegion.Height;
    fBottom:=-1;
    sc:=Project.CurrentRegion.GetPixel(x,y);
    for j:=0 to Project.CurrentRegion.Height-1 do
      for i:=0 to Project.CurrentRegion.Width-1 do
        if Project.CurrentRegion.GetPixel(i,j)=sc then begin
          Project.CurrentRegion.Putpixel(i,j,POSTPROCESSCOLOR);
          if i>fRight then fRight:=i;
          if i<fLeft then fLeft:=i;
          if j>fBottom then fBottom:=j;
          if j<fTop then fTop:=j;
        end;
    Project.CurrentImage.RegionUndo.AddImageUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1,fTempImage);
    FreeAndNil(fTempImage);
    ActiveInk.InitializeArea(fLeft,fTop,fRight,fBottom);
    ActiveInk.PostProcess;
    Project.CurrentImage.RegionUndo.AddImageRedoToLastUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1);
    Result:=true;
  end else Result:=false;
end;

function TBDToolSep.ClickBoxed(x, y, button: integer): boolean;
var i,j:integer;sc:uint32;
begin
  if button=SDL_BUTTON_LEFT then begin
    case fState of
      0:begin
          fX1:=x;
          fY1:=y;
          Result:=true;
          fState:=1;
        end;
      1:begin
          fX2:=x;
          fY2:=y;
          Result:=true;
          fState:=2;
        end;
      2:begin
          if fX1>fX2 then begin i:=fX2;fX2:=fX1;fX1:=i;end;
          if fY1>fY2 then begin i:=fY2;fY2:=fY1;fY1:=i;end;
          sc:=Project.CurrentRegion.GetPixel(x,y);
          Project.CurrentImage.RegionUndo.AddImageUndo(fX1,fY1,fX2-fX1+1,fY2-fY1+1);
          ActiveInk.InitializeArea(fX1,fY1,fX2,fY2);

          for j:=fY1 to fY2 do
            for i:=fX1 to fX2 do
              if Project.CurrentRegion.GetPixel(i,j)=sc then
                Project.CurrentRegion.Putpixel(i,j,POSTPROCESSCOLOR);

          ActiveInk.PostProcess;
          Project.CurrentImage.RegionUndo.AddImageRedoToLastUndo(fX1,fY1,fX2-fX1+1,fY2-fY1+1);
          Result:=true;
          InfoBar.ShowText('');
          fState:=0;
          Result:=true;
        end;
    end;
  end
  else if Button=SDL_BUTTON_RIGHT then
    if fState>0 then begin
      fState:=0;
      InfoBar.ShowText('');
      Result:=true;
    end else Result:=false
  else Result:=false;
end;

procedure TBDToolSep.Configure;
begin
  MessageQueue.AddMessage(MSG_OPENCONFIGURESEPDIALOG);
end;

end.

