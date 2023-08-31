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

unit BDPToolBase;

{$mode Delphi}

interface

uses
  SysUtils, Lists;

type

  { TBDTool }

  TBDTool=class
    constructor Create; virtual;
    // If the tool needs some initialization before every use, do that here.
    procedure Initialize; virtual;
    // Draw helping lines, refresh infobar
    procedure Draw; virtual;
    // Clear helping lines if the tool has them.
    procedure Clear; virtual;
    // The mouse positioned over the draw area (x,y).
    procedure Move(x,y:integer); virtual;
    // Mouse down occured over the draw area.
    function MouseDown(x,y,buttons:integer):boolean; virtual;
    // Mouse moved over the draw area.
    function MouseMove(x,y,buttons:integer):boolean; virtual;
    // Mouse up occured over the draw area.
    function MouseUp(x,y,buttons:integer):boolean; virtual;
    // Mouse click occured over the draw area.
    function Click(x,y,buttons:integer):boolean; virtual;
  protected
    fState:integer;  // 0 - Waiting for first click, 1 - waiting for second click, etc.
    fX,fY:integer;  // Current position from Move
    fName,fHint:string;
    fPinnable:boolean;
  public
    property Name:string read fName;
    property Hint:string read fHint;
    property Pinnable:boolean read fPinnable;
  end;

  { TBDTools }

  TBDTools=class(TNamedList<TBDTool>)
    constructor Create;
  end;

implementation

uses
  BDPShared, BDPToolBox, BDPToolCircle, BDPToolDraw, BDPToolFill, BDPToolFillTo,
  BDPToolLine, BDPToolSep, BDPToolEdge, BDPToolGetCEL, BDPToolPutCEL,
  BDPToolShowCEL, BDPToolConfigureCGrad, BDPToolConfigureRGrad,
  BDPToolSelectColor;

{ TBDTool }

constructor TBDTool.Create;
begin
  fState:=0;
  fPinnable:=false;
end;

procedure TBDTool.Initialize;
begin
  // Do nothing. Override if the Tool needs initializing before every use.
end;

procedure TBDTool.Draw;
begin
  // Do nothing. Tools with only 1 click operations (Draw, Fill) does not need
  // additional drawing. They don't need to override this.
end;

procedure TBDTool.Clear;
begin
  // Do nothing. Tools with only 1 click operations (Draw, Fill) does not need
  // additional clearing. They don't need to override this.
end;

procedure TBDTool.Move(x,y:integer);
begin
  fX:=x;
  fY:=y;
end;

function TBDTool.MouseDown(x,y,buttons:integer):boolean;
begin
  Result:=false;
end;

function TBDTool.MouseUp(x,y,buttons:integer):boolean;
begin
  Result:=false;
end;

function TBDTool.MouseMove(x,y,buttons:integer):boolean;
begin
  Result:=false;
end;

function TBDTool.Click(x,y,buttons:integer):boolean;
begin
  Result:=false;
end;

{ TBDTools }

constructor TBDTools.Create;
begin
  inherited Create;
  AddObject('BOX',TBDToolBox.Create);
  AddObject('CIRCLE',TBDToolCircle.Create);
  AddObject('DRAW',TBDToolDraw.Create);
  AddObject('FILL',TBDToolFill.Create);
  AddObject('FILLTO',TBDToolFillTo.Create);
  AddObject('LINE',TBDToolLine.Create);
  AddObject('SEP.',TBDToolSep.Create);
  AddObject('EDGE',TBDToolEdge.Create);
  AddObject('GETCEL',TBDToolGetCel.Create);
  AddObject('PUTCEL',TBDToolPutCel.Create);
  AddObject('SELCOL',TBDToolSelectColor.Create);
  AddObject('SHOWCEL',TBDToolShowCEL.Create);
  AddObject('CONFCG',TBDToolConfigureCGrad.Create);
  AddObject('CONFRG',TBDToolConfigureRGrad.Create);
end;

end.

