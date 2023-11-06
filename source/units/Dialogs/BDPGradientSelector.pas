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

unit BDPGradientSelector;

{$mode Delphi}

interface

uses
  SysUtils, BDPModalDialog, BDPButton, BDPSimpleGradient, BDPSliders;

type

  { TBDGradientSelector }

  TBDGradientSelector=class(TBDModalDialog)
    constructor Create;
    destructor Destroy; override;
  private
    fUndoButton,
    fRedoButton,
    fDeleteButton:TBDButton;
    fSelectedGradientIndex:integer;
    fGradients:array[0..7] of TBDSimpleGradient;
    fScrollBar:TBDVerticalSlider;
    procedure GradientClick(Sender:TObject;x,y,button:integer);
    procedure SelectClick(Sender:TObject;x,y,button:integer);
    procedure CancelClick(Sender:TObject;x,y,button:integer);
  end;

implementation

uses BDPShared, MKMouse2, BDPGradient, Math;

const
  GRADIENTSELECTORWIDTH=640;
//  GRADIENTSELECTORHEIGHT=6*(NORMALBUTTONHEIGHT+3)-3+2*9+3+MODALDIALOGCAPTIONHEIGHT+15;
  BUTTONSLEFT=GRADIENTSELECTORWIDTH-NORMALBUTTONWIDTH-9-3;
  BUTTONSTOP=MODALDIALOGCAPTIONHEIGHT+9;
  SCROLLBARWIDTH=33;
  GRADIENTSCROLLBARGAP=3;
  GRADIENTWIDTH=GRADIENTSELECTORWIDTH-3-9-GRADIENTSCROLLBARGAP-SCROLLBARWIDTH-9-NORMALBUTTONWIDTH-9-3;
  GRADIENTHEIGHT=36;
  GRADIENTSTOP=BUTTONSTOP;
  GRADIENTSELECTORHEIGHT=8*(GRADIENTHEIGHT+3)-3+2*9+3+MODALDIALOGCAPTIONHEIGHT;
  SCROLLBARLEFT=3+9+GRADIENTWIDTH+GRADIENTSCROLLBARGAP;
  SCROLLBARTOP=GRADIENTSTOP;
  SCROLLBARHEIGHT=8*(GRADIENTHEIGHT+3)-3;

{ TBDGradientSelector }

constructor TBDGradientSelector.Create;
var i:integer;

  procedure CreateButton(pLeft,pTop:integer;pCaption,pHint,pName:string;pOnClick:TMouseButtonEvent=nil);
  var tmpB:TBDButton;
  begin
    tmpB:=TBDButton.Create(pLeft,pTop,NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
      pCaption,pHint);
    with tmpB do begin
      Name:=pName;
      ZIndex:=MODALDIALOG_ZINDEX+1;
      OnClick:=pOnClick;
    end;
    AddChild(tmpB);
  end;

  procedure CreateGradient(pIndex,pLeft,pTop:integer;pGradient:TGradient=nil);
  begin
    fGradients[pIndex]:=TBDSimpleGradient.Create(pLeft,pTop,GRADIENTWIDTH,GRADIENTHEIGHT,pGradient);
    with fGradients[pIndex] do begin
      ZIndex:=MODALDIALOG_ZINDEX+1;
      Name:='GS Gradient '+inttostr(pIndex);
      Tag:=pIndex;
      OnClick:=GradientClick;
    end;
    AddChild(fGradients[pIndex]);
  end;

begin
  inherited Create(GRADIENTSELECTORWIDTH,GRADIENTSELECTORHEIGHT);
  Caption:='GRADIENT SELECTOR';
  fName:='GradientSelector';
  MouseObjects.Add(Self);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP,
    'SELECT','SELECT THE GRADIENT.','GS Select',SelectClick);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+NORMALBUTTONHEIGHT+3,
    'CANCEL','CLOSE DIALOG.','GS Close',CancelClick);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+(NORMALBUTTONHEIGHT+3)*2,
    'ADD','ADD NEW GRADIENT.','GS Add');

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+(NORMALBUTTONHEIGHT+3)*3,
    'DELETE','DELETE SELECTED GRADIENT.','GS Delete');
  fDeleteButton:=TBDButton(fChildren[fChildren.Count-1]);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+(NORMALBUTTONHEIGHT+3)*4,
    'UNDO','UNDO LAST GRADIENT OPERATION.','GS Undo');
  fUndoButton:=TBDButton(fChildren[fChildren.Count-1]);

  CreateButton(fLeft+BUTTONSLEFT,fTop+BUTTONSTOP+(NORMALBUTTONHEIGHT+3)*5,
    'REDO','REDO LAST GRADIENT OPERATION.','GS Redo');
  fRedoButton:=TBDButton(fChildren[fChildren.Count-1]);

  for i:=0 to min(Project.CurrentGradientList.Count-1,7) do
    CreateGradient(i,fLeft+3+9,fTop+GRADIENTSTOP+(GRADIENTHEIGHT+3)*i,Project.CurrentGradientList[i]);

  for i:=Project.CurrentGradientList.Count to 7 do
    CreateGradient(i,fLeft+3+9,fTop+GRADIENTSTOP+(GRADIENTHEIGHT+3)*i);

  fScrollBar:=TBDVerticalSlider.Create(fLeft+SCROLLBARLEFT,fTop+SCROLLBARTOP,SCROLLBARWIDTH,SCROLLBARHEIGHT);
  fScrollBar.ZIndex:=MODALDIALOG_ZINDEX+1;
  fScrollBar.Name:='GS ScrollBar';
  fScrollBar.MinValue:=0;
  fScrollBar.MaxValue:=max(Project.CurrentGradientList.Count-8,0);
  fScrollBar.Position:=0;
  fScrollBar.ShowNumber:=false;
  AddChild(fScrollBar);

  fSelectedGradientIndex:=Project.CurrentGradientList.ActiveIndex-fScrollBar.Position;
  fGradients[fSelectedGradientIndex].Selected:=true;
end;

destructor TBDGradientSelector.Destroy;
begin
  inherited Destroy;
end;

procedure TBDGradientSelector.GradientClick(Sender:TObject; x,y,button:integer);
var i:integer;
begin
  if Sender is TBDSimpleGradient then with Sender as TBDSimpleGradient do begin
    if Tag<Project.CurrentGradientList.Count then begin
      fGradients[fSelectedGradientIndex].Selected:=false;
      fSelectedGradientIndex:=Tag;
      fGradients[fSelectedGradientIndex].Selected:=true;
    end;
  end;
end;

procedure TBDGradientSelector.SelectClick(Sender:TObject; x,y,button:integer);
begin
  Project.CurrentGradientList.ActiveIndex:=fScrollBar.Position+fSelectedGradientIndex;
  Self.Hide;
end;

procedure TBDGradientSelector.CancelClick(Sender:TObject; x,y,button:integer);
begin
  Self.Hide;
end;

end.

