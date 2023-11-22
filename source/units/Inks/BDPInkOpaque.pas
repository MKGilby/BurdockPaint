{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
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

