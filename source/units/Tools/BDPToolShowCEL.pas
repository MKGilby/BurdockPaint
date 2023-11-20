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

unit BDPToolShowCEL;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolShowCEL }

  TBDToolShowCEL=class(TBDTool)
    constructor Create; override;
    procedure Initialize; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fStartTime:UInt64;
  end;

implementation

uses BDPShared;

{ TBDToolShowCEL }

constructor TBDToolShowCEL.Create;
begin
  inherited Create;
  fName:='SHOWCEL';
  fHint:=uppercase('Shows CEL for 1 sec.');
end;

procedure TBDToolShowCEL.Initialize;
begin
  CELHelperImage.Bar(0,0,CELHelperImage.Width,CELHelperImage.Height,0);
  fStartTime:=GetTickCount64;
end;

procedure TBDToolShowCEL.Draw;
begin
  OverlayImage.Rectangle(Project.CelImage.Left,Project.CelImage.Top,Project.CELImage.Width,Project.CELImage.Height,VibroColors.GetColor);
  if Settings.ClearKeyColor then
    CELHelperImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage,$FF000000)
  else
    CELHelperImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage);
  if GetTickCount64-fStartTime>1000 then begin
    MessageQueue.AddMessage(MSG_RESTORECONTROLS);
  end;
end;

procedure TBDToolShowCEL.Clear;
begin
  OverlayImage.Rectangle(Project.CelImage.Left,Project.CelImage.Top,Project.CELImage.Width,Project.CELImage.Height,0);
  CELHelperImage.Bar(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage.Width,Project.CELImage.Height,0);
end;

end.

