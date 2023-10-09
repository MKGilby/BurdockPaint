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

unit BDPSelectColorClusterDialog;

{$mode Delphi}

interface

uses
  SysUtils, BDPModalDialog;

type

  { TBDSelectColorClusterDialog }

  TBDSelectColorClusterDialog=class(TBDModalDialog)
    constructor Create;
    procedure Refresh; override;
    procedure Click(Sender:TObject;x,y,buttons:integer);
    procedure AddButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure ColorClusterClick(Sender:TObject;x,y,buttons:integer);
    procedure SetPositionAndWidth(pLeft,pBottom,pWidth:integer);
  private
    fBottom: integer;
    fWindowWidth:integer;
    procedure fSetWindowLeft(value:integer);
    procedure fSetWindowWidth(value:integer);
    procedure fSetBottom(value:integer);
  public
    property WindowLeft:integer write fSetWindowLeft;
    property WindowWidth:integer read fWindowWidth write fSetWindowWidth;
    property Bottom:integer write fSetBottom;
  end;

implementation

uses BDPShared, MKMouse2, BDPButton, mk_sdl2, BDPColorCluster, BDPSimpleColorCluster;

const
  COLORCLUSTERDIALOGWIDTH=COLORCLUSTERWIDTH;

{ TBDSelectColorClusterDialog }

constructor TBDSelectColorClusterDialog.Create;
begin
  inherited Create(COLORCLUSTERDIALOGWIDTH,36*(Project.CurrentColorClusters.Count+1)+3);
  fWindowWidth:=COLORCLUSTERDIALOGWIDTH;
  fName:='SelectColorClusterDialog';
  Visible:=false;
  MouseObjects.Add(Self);
  Refresh;
  OnClick:=CLick;
end;

procedure TBDSelectColorClusterDialog.Refresh;
var y,i:integer;atmC:TBDSimpleColorCluster;atmB:TBDButton;fNeedAddButton:boolean;
begin
  ClearChildren;
  fTexture.Free;
  fNeedAddButton:=Project.CurrentColorClusters.Count<16;
  if fNeedAddButton then begin
    fTexture:=TStreamingTexture.Create(fWindowWidth,36*(Project.CurrentColorClusters.Count+1)+3);
    y:=36;
  end else begin
    fTexture:=TStreamingTexture.Create(fWindowWidth,36*(Project.CurrentColorClusters.Count)+9);
    y:=6;
  end;
  fTop:=fBottom-fTexture.Height;
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height,SystemPalette[SYSTEMCOLORDARK]);
  fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,SystemPalette[SYSTEMCOLORMID]);
  fTexture.Update;
  if fNeedAddButton then begin
    atmB:=TBDButton.Create(fLeft+(fTexture.Width-NORMALBUTTONWIDTH) div 2,fTop+6,
      NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,'ADD','');
    atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
    atmB.Name:='ADD_CC';
    atmB.OnClick:=AddButtonClick;
    AddChild(atmB);
  end;
  for i:=Project.CurrentColorClusters.Count-1 downto 0 do begin
    atmC:=TBDSimpleColorCluster.Create(fLeft+6,fTop+y+(Project.CurrentColorClusters.Count-1-i)*36,COLORCLUSTERWIDTH,COLORCLUSTERHEIGHT,Project.CurrentColorClusters[i]);
    atmC.Width:=fTexture.Width-12;
    atmC.Height:=COLORCLUSTERHEIGHT-3;
    atmC.ZIndex:=MODALDIALOG_ZINDEX+1;
    atmC.Tag:=i;
    atmC.OnClick:=ColorClusterClick;
    atmC.Selected:=(i=Project.CurrentColorClusters.ActiveIndex);
    atmC.Name:=Format('SimpleColorCluster %d',[i]);
    AddChild(atmC);
  end;
end;

procedure TBDSelectColorClusterDialog.Click(Sender:TObject; x,y,buttons:integer);
begin
  if (x<fLeft) or (x>=fLeft+fTexture.Width) or (y<fTop) or (y>fTop+fTexture.Height) then
    MessageQueue.AddMessage(MSG_COLORCLUSTERDIALOGRESP);
end;

procedure TBDSelectColorClusterDialog.AddButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Project.CurrentColorClusters.Add(TColorCluster.Create(0,15));
  fTop-=COLORCLUSTERHEIGHT;
  Refresh;
end;

procedure TBDSelectColorClusterDialog.ColorClusterClick(Sender:TObject; x,y,buttons:integer);
var cc:TBDSimpleColorCluster;
begin
  cc:=Sender as TBDSimpleColorCluster;
  x-=cc.Left;
  if x<cc.ColorsWidth then
    MessageQueue.AddMessage(MSG_COLORCLUSTERDIALOGRESP,cc.Tag)
  else begin
    if not cc.Selected then begin
      Project.CurrentColorClusters.Delete(cc.Tag);
      if Project.CurrentColorClusters.ActiveIndex>cc.Tag then
        Project.CurrentColorClusters.ActiveIndex:=Project.CurrentColorClusters.ActiveIndex-1;
      fTop+=COLORCLUSTERHEIGHT;
      Refresh;
    end;
  end;
end;

procedure TBDSelectColorClusterDialog.SetPositionAndWidth(pLeft,pBottom,pWidth:integer);
begin
  Left:=pLeft;
  Width:=pWidth;
  fBottom:=pBottom;
  Refresh;
end;

procedure TBDSelectColorClusterDialog.fSetWindowLeft(value:integer);
begin
  Left:=value;
  Refresh;
end;

procedure TBDSelectColorClusterDialog.fSetWindowWidth(value:integer);
begin
  Width:=value;
  Refresh;
end;

procedure TBDSelectColorClusterDialog.fSetBottom(value: integer);
begin
  fBottom:=value;
  Refresh;
end;

end.

