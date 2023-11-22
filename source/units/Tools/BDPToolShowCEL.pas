{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
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
  fStartTime:=GetTickCount64;
end;

procedure TBDToolShowCEL.Draw;
begin
  if Settings.ClearKeyColor then
    OverlayImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage,$FF000000)
  else
    OverlayImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage);
  OverlayImage.Rectangle(Project.CelImage.Left,Project.CelImage.Top,Project.CELImage.Width,Project.CELImage.Height,VibroColors.GetColor);
  if GetTickCount64-fStartTime>1000 then begin
    MessageQueue.AddMessage(MSG_RESTORECONTROLS);
  end;
end;

procedure TBDToolShowCEL.Clear;
begin
  OverlayImage.Bar(Project.CelImage.Left,Project.CelImage.Top,Project.CELImage.Width,Project.CELImage.Height,0);
end;

end.

