{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of BurdockPaint.

  BurdockPaint is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  BurdockPaint is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  BurdockPaint. If not, see <https://www.gnu.org/licenses/>.
}

unit BDPToolPutCEL;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolPutCEL }

  TBDToolPutCEL=class(TBDTool)
    constructor Create; override;
    procedure Initialize; override;
    function Click(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
  end;

implementation

uses sdl2, BDPShared;

{ TBDToolPutCEL }

constructor TBDToolPutCEL.Create;
begin
  inherited Create;
  fName:='PUTCEL';
  fHint:=uppercase('Paste the temporary image to the image.');
end;

procedure TBDToolPutCEL.Initialize;
begin
  CELHelperImage.Bar(0,0,CELHelperImage.Width,CELHelperImage.Height,0);
end;

function TBDToolPutCEL.Click(x,y,button:integer):boolean;
begin
  if button=SDL_BUTTON_LEFT then begin
    case fState of
      0:begin
          fSX:=x;
          fSY:=y;
          inc(fState);
        end;
      1:begin
          fState:=-1;
          Project.CelImage.Left:=Project.CelImage.Left+fX-fSX;
          Project.CelImage.Top:=Project.CelImage.Top+fY-fSY;
          Project.CurrentImage.RegionUndo.AddImageUndo(
            Project.CelImage.Left,Project.Celimage.Top,Project.CelImage.Width,Project.CELImage.Height);
          ActiveInk.ProcessWithCEL(Project.CelImage.Left,Project.CelImage.Top);
          Project.CurrentImage.RegionUndo.AddImageRedoToLastUndo(Project.CelImage.Left,Project.Celimage.Top,Project.CelImage.Width,Project.CELImage.Height);
          MessageQueue.AddMessage(MSG_RESTORECONTROLS);
        end;
    end;
  end else
  if button=SDL_BUTTON_RIGHT then begin
    fState:=-1;
    MessageQueue.AddMessage(MSG_RESTORECONTROLS);
  end;
  Result:=true;
end;

function TBDToolPutCEL.MouseUp(x,y,button:integer):boolean;
begin
  Result:=true;
end;

procedure TBDToolPutCEL.Draw;
begin
  case fState of
    -1:fState:=0;  // One frame where we don't draw anything before resetting fState.
                   // This is needed to remove flickering.
    0:begin
        Project.OverlayImage.Rectangle(Project.CelImage.Left,Project.CelImage.Top,Project.CELImage.Width,Project.CELImage.Height,VibroColors.GetColor);
        if Settings.ClearKeyColor then
          CELHelperImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage,$FF000000)
        else
          CELHelperImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage);
        InfoBar.ShowText(inttostr(fX)+','+inttostr(fY));
      end;
    1:begin
        if Settings.ClearKeyColor then
          CELHelperImage.PutImage(Project.CELImage.Left+fX-fSX,Project.CELImage.Top+fY-fSY,Project.CELImage,$FF000000)
        else
          CELHelperImage.PutImage(Project.CELImage.Left+fX-fSX,Project.CELImage.Top+fY-fSY,Project.CELImage);
        InfoBar.ShowText(Format('%d,%d (%d,%d)',[Project.CELImage.Left+fX-fSX,Project.CELImage.Top+fY-fSY,fX-fSX,fY-fSY]));
      end;
  end;
end;

procedure TBDToolPutCEL.Clear;
begin
  case fState of
    0:begin
        Project.OverlayImage.Rectangle(Project.CelImage.Left,Project.CelImage.Top,Project.CELImage.Width,Project.CELImage.Height,0);
        CELHelperImage.Bar(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage.Width,Project.CELImage.Height,0);
      end;
    1:begin
        CELHelperImage.Bar(Project.CELImage.Left+fX-fSX,Project.CELImage.Top+fY-fSY,Project.CELImage.Width,Project.CELImage.Height,0);
      end;

  end;
end;

end.

