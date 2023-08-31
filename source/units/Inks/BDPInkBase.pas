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

unit BDPInkBase;

{$mode Delphi}

interface

uses
  SysUtils, BDPModalDialog, Lists;

type

  { TBDInk }

  TBDInk=class
    constructor Create; virtual;
    procedure InitializeAreaWH(pLeft,pTop,pWidth,pHeight:integer); virtual;
    procedure InitializeArea(pX1,pY1,pX2,pY2:integer); virtual;
    function GetColorAt(pX,pY:integer):uint32; virtual;
    procedure PostProcess; virtual;
    procedure Configure; virtual;
    procedure ProcessWithCEL(pX,pY:integer); virtual;
  protected
    fLeft,fTop,fWidth,fHeight:integer;
    fSupportsOnTheFly:boolean;
    fName,fHint:string;
    fConfigureDialog:TBDModalDialog;
  public
    property Name:string read fName;
    property Hint:string read fHint;
    property SupportsOnTheFly:boolean read fSupportsOnTheFly;
    property ConfigureDialog:TBDModalDialog read fConfigureDialog write fConfigureDialog;
  end;

  { TBDInks }

  TBDInks=class(TNamedList<TBDInk>)
    constructor Create;
  end;

implementation

uses
  BDPShared, BDPInkOpaque, BDPInkHGrad, BDPInkLGrad, BDPInkVGrad, BDPInkCGrad,
  BDPInkRandom, BDPInkSoften, BDPInkRGrad;

// ------------------------------------------------------------- [ TBDInk ] ---

constructor TBDInk.Create;
begin
  fLeft:=0;
  fTop:=0;
  fWidth:=WINDOWWIDTH;
  fHeight:=WINDOWHEIGHT;
  fConfigureDialog:=nil;
end;

procedure TBDInk.InitializeAreaWH(pLeft,pTop,pWidth,pHeight:integer);
begin
  if pWidth<0 then begin
    fLeft:=pLeft+pWidth;
    fWidth:=-pWidth;
  end else begin
    fLeft:=pLeft;
    fWidth:=pWidth;
  end;
  if pHeight<0 then begin
    fTop:=pTop+pHeight;
    fHeight:=-pHeight;
  end else begin
    fTop:=pTop;
    fHeight:=pHeight;
  end;
end;

procedure TBDInk.InitializeArea(pX1,pY1,pX2,pY2:integer);
begin
  if pX1>pX2 then begin
    fLeft:=pX2;
    fWidth:=pX1-pX2+1;
  end else begin
    fLeft:=pX1;
    fWidth:=pX2-pX1+1;
  end;
  if pY1>pY2 then begin
    fTop:=pY2;
    fHeight:=pY1-pY2+1;
  end else begin
    fTop:=pY1;
    fHeight:=pY2-pY1+1;
  end;
end;

function TBDInk.GetColorAt(pX,pY:integer):uint32;
begin
  Result:=0;
  if fSupportsOnTheFly then
    raise Exception.Create('This ink ('+fName+') needs to override the GetColorIndexAt method!')
  else
    raise Exception.Create('This ink does not support OnTheFly, GetColorIndexAt method shouldn''t be called! ('+fName+')');
end;

procedure TBDInk.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do
    for i:=fLeft to fLeft+fWidth-1 do
      if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        Project.CurrentImage.PutPixel(i,j,GetColorAt(i,j));
end;

procedure TBDInk.Configure;
begin
  if Assigned(fConfigureDialog) then fConfigureDialog.Show;
end;

procedure TBDInk.ProcessWithCEL(pX,pY:integer);
var i,j:integer;
begin
  InitializeAreaWH(pX,pY,Project.CELImage.Width,Project.CELImage.Height);
  if Assigned(Project.CELImage) then begin
    for j:=0 to Project.CELImage.Height-1 do
      for i:=0 to Project.CELImage.Width-1 do
        if not Settings.ClearKeyColor or
           (Settings.ClearKeyColor and (Project.CELImage.GetPixel(i,j)<>$FF000000)) then
        Project.CurrentImage.PutPixel(px+i,py+j,POSTPROCESSCOLOR);
    PostProcess;
  end;
end;

// ------------------------------------------------------------ [ TBDInks ] ---

constructor TBDInks.Create;
begin
  AddObject('OPAQUE',TBDInkOpaque.Create);
  AddObject('H GRAD',TBDInkHGrad.Create);
  AddObject('L GRAD',TBDInkLGrad.Create);
  AddObject('V GRAD',TBDInkVGrad.Create);
  AddObject('C GRAD',TBDInkCGrad.Create);
  AddObject('RANDOM',TBDInkRandom.Create);
  AddObject('SOFTEN',TBDInkSoften.Create);
  AddObject('R GRAD',TBDInkRGrad.Create);
end;

end.

