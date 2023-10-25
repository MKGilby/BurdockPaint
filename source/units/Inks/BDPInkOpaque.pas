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

unit BDPInkOpaque;

{$mode Delphi}

interface

uses
  BDPInkBase;

type

  { TBDInkOpaque }

  TBDInkOpaque=class(TBDInk)
    constructor Create; override;
    function GetColorAt(pX,pY:integer):uint32; override;
    procedure ProcessWithCEL(pX,pY:integer); override;
  end;

implementation

uses BDPShared;

{ TBDInkOpaque }

constructor TBDInkOpaque.Create;
begin
  inherited ;
  fName:='OPAQUE';
  fHint:='DRAWS WITH THE SELECTED COLOR.';
  fSupportsOnTheFly:=true;
end;

function TBDInkOpaque.GetColorAt(pX,pY:integer):uint32;
begin
  Result:=Settings.ActiveColor;
end;

procedure TBDInkOpaque.ProcessWithCEL(pX,pY:integer);
begin
  if Settings.ClearKeyColor then
    Project.CurrentRegion.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage,$FF000000)
  else
    Project.CurrentRegion.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage);
end;

end.

