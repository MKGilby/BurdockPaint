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

unit BDPMenu;

{$mode Delphi}

interface

uses Classes, SysUtils, fgl, mk_sdl2, MKMouse2, BDPMessage, vcc2_VisibleControl;

type

  { TSubMenuItem }

  TSubMenuItem=record
    _name:string;
    _hint:string;
    _message:TMessage;
    _enabled:boolean;
    constructor Init(iName,iHint:string;iMessage:TMessage);
  end;

  { TSubMenu }

  TSubMenu=class(TVisibleControl)
    constructor Create(iLeft:integer);
    procedure MouseMove(Sender:TObject;x,y:integer);
    procedure MouseDown(Sender:TObject;x,y,buttons:integer);
    procedure MouseLeave(Sender:TObject);
    procedure AddItem(item,hint:string;msg:TMessage;enabled:boolean=true);
    procedure DisableItem(item:string);
    procedure EnableItem(item:string);
    procedure DisableAllItems;
    procedure EnableAllItems;
  protected
    procedure ReDraw; override;
  private
    fItems:array of TSubMenuItem;
    fSelected:integer;
    procedure fSetName(value:string);
    procedure Resize;
    procedure SetItemEnabled(item:string;value:boolean);
  public
    property Name:string read fName write fSetName;
  end;

  { TSubMenuList }

  TSubMenuList=class(TFPGObjectList<TSubMenu>)
    function ItemByName(pName:string):TSubMenu;
  end;

  { TMainMenuItem }

  TMainMenuItem=record
    _name:string;
    _enabled:boolean;
    constructor Init(iName:string);
  end;

  { TMainMenu }

  TMainMenu=class(TVisibleControl)
    constructor Create(iBINstring:string);
    destructor Destroy; override;
    procedure EnableCELSubMenusWithActiveCEL;
    procedure DisableCELSubMenusWithActiveCEL;
    procedure SetToolsMenuStates;
    procedure SetInksMenuStates;
    procedure MouseMove(Sender:TObject;x,y:integer);
    procedure MouseLeave(Sender:TObject);
    procedure MouseEnter(Sender:TObject);
    function ProcessMessage(msg:TMessage):boolean;
    procedure DisableItem(item:string);
    procedure EnableItem(item:string);
  protected
    procedure ReDraw; override;
  private
    fSubMenus:TSubMenuList;
    fItems:array of TMainMenuItem;
    fSelected:integer;
  end;

implementation

uses BDPShared, MKStream;

{ TSubMenuList }

function TSubMenuList.ItemByName(pName:string):TSubMenu;
var i:integer;
begin
  for i:=0 to Self.Count-1 do
    if Self[i].Name=pName then begin
      Result:=Self[i];
      exit;
    end;
  Result:=nil;
end;

{ TMainMenuItem }

constructor TMainMenuItem.Init(iName:string);
begin
  _name:=iName;
  _enabled:=true;
end;

{ TSubMenuItem }

constructor TSubMenuItem.Init(iName,iHint:string; iMessage:TMessage);
begin
  _name:=iName;
  _hint:=uppercase(iHint);
  _message:=iMessage;
  _enabled:=true;
end;

{ TSubMenu }

constructor TSubMenu.Create(iLeft:integer);
begin
  inherited Create;
  Left:=iLeft;
  Top:=0;
  Width:=180;
  Height:=TOPMENUHEIGHT;
  SetLength(fItems,0);
  fSelected:=-1;
  ZIndex:=LEVEL1CONTROLS_ZINDEX-1;
  Enabled:=true;
  OnMouseDown:=MouseDown;
  OnMouseMove:=MouseMove;
  OnMouseLeave:=MouseLeave;
end;

procedure TSubMenu.MouseMove(Sender:TObject; x,y:integer);
var pre:integer;
begin
  pre:=fSelected;
  fSelected:=(y-TOPMENUHEIGHT) div SUBMENULINEHEIGHT;
  if pre<>fSelected then begin
    if (fSelected>=0) and (fSelected<Length(fItems)) then
      InfoBar.ShowText(fItems[fSelected]._hint);
    fNeedRedraw:=true;
  end;
end;

procedure TSubMenu.MouseDown(Sender:TObject; x,y,buttons:integer);
begin
  if (fSelected>=0) and (fSelected<Length(fItems)) and (fItems[fSelected]._enabled) then begin
    MessageQueue.AddMessage(fItems[fSelected]._message);
    fSelected:=-1;
    Visible:=false;
  end;
end;

procedure TSubMenu.MouseLeave(Sender:TObject);
begin
  fSelected:=-1;
  fNeedRedraw:=true;
  InfoBar.ShowText('');
  Visible:=false;
end;

procedure TSubMenu.AddItem(item,hint:string; msg:TMessage; enabled:boolean);
begin
  if item<>'' then begin
    SetLength(fItems,Length(fItems)+1);
    fItems[Length(fItems)-1]:=TSubMenuItem.Init(item,hint,msg);
    fItems[Length(fItems)-1]._enabled:=enabled;
    Resize;
  end;
end;

procedure TSubMenu.DisableItem(item:string);
begin
  SetItemEnabled(item,false);
end;

procedure TSubMenu.EnableItem(item:string);
begin
  SetItemEnabled(item,true);
end;

procedure TSubMenu.DisableAllItems;
var i:integer;
begin
  for i:=0 to length(fItems)-1 do fItems[i]._enabled:=false;
  fNeedRedraw:=true;
end;

procedure TSubMenu.EnableAllItems;
var i:integer;
begin
  for i:=0 to length(fItems)-1 do fItems[i]._enabled:=true;
  fNeedRedraw:=true;
end;

procedure TSubMenu.fSetName(value:string);
begin
  fName:=value;
  Resize;
end;

procedure TSubMenu.Resize;
var i,w,h:integer;
begin
  w:=0;
  if (length(fName)+2)*18>w then w:=(length(fName)+2)*18;
  for i:=0 to Length(fItems)-1 do
    if (length(fItems[i]._name)+1)*18>w then w:=(length(fItems[i]._name)+1)*18;
  h:=TOPMENUHEIGHT+Length(fItems)*SUBMENULINEHEIGHT+3;
  if (w<>fWidth) or (h<>fHeight) then begin
    Width:=w;
    Height:=h;
  end;
end;

procedure TSubMenu.SetItemEnabled(item:string; value:boolean);
var i:integer;
begin
  for i:=0 to length(fItems)-1 do
    if fItems[i]._name=item then begin
      value:=value and (fItems[i]._message.TypeID<>MSG_NONE);
      fItems[i]._enabled:=value;
      fNeedRedraw:=true;
      break;
    end;
end;

procedure TSubMenu.ReDraw;
var i:integer;
begin
  if Assigned(fTexture) then begin
    fTexture.ARGBImage.Bar(0,0,(length(fName)+2)*18,TOPMENUHEIGHT-3,SystemPalette[SYSTEMCOLORLIGHT]);
    fTexture.ARGBImage.Bar((length(fName)+2)*18,0,Width-(length(fName)+2)*18,TOPMENUHEIGHT-3,SystemPalette[SYSTEMCOLORTRANSPARENT]);
    MM.Fonts['Red'].OutText(fTexture.ARGBImage,fName,18,3,0);
    fTexture.ARGBImage.Bar(0,TOPMENUHEIGHT-3,fWidth,3,SystemPalette[SYSTEMCOLORDARK]);
    fTexture.ARGBImage.Bar(0,TOPMENUHEIGHT-3,3,fHeight-TOPMENUHEIGHT,SystemPalette[SYSTEMCOLORDARK]);
    fTexture.ARGBImage.Bar(0,fHeight-3,fWidth,3,SystemPalette[SYSTEMCOLORDARK]);
    fTexture.ARGBImage.Bar(fWidth-3,TOPMENUHEIGHT-3,3,fHeight-TOPMENUHEIGHT,SystemPalette[SYSTEMCOLORDARK]);
    fTexture.ARGBImage.Bar(3,TOPMENUHEIGHT,fWidth-6,fHeight-TOPMENUHEIGHT-3,SystemPalette[SYSTEMCOLORMID]);
    if fSelected>-1 then
      fTexture.ARGBImage.Bar(3,TOPMENUHEIGHT+fSelected*SUBMENULINEHEIGHT,fWidth-6,SUBMENULINEHEIGHT,SystemPalette[SYSTEMCOLORLIGHT]);
    if Assigned(fItems) then begin
      for i:=0 to Length(fItems)-1 do begin
        if fItems[i]._enabled then begin
          if i<>fSelected then
            MM.Fonts['Black'].OutText(fTexture.ARGBImage,fItems[i]._name,9,TOPMENUHEIGHT+i*SUBMENULINEHEIGHT+(SUBMENULINEHEIGHT-15) div 2,0)
          else
            MM.Fonts['Red'].OutText(fTexture.ARGBImage,fItems[i]._name,9,TOPMENUHEIGHT+i*SUBMENULINEHEIGHT+(SUBMENULINEHEIGHT-15) div 2,0);
        end else
          MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,fItems[i]._name,9,TOPMENUHEIGHT+i*SUBMENULINEHEIGHT+(SUBMENULINEHEIGHT-15) div 2,0);
      end;
    end;
    fTexture.Update;
  end;
end;

{ TMainMenu }

constructor TMainMenu.Create(iBINstring:string);
var atm:TSubMenu;x,menucount,submenucount:integer;msg:TMessage;Xs:TStream;
  s,hint:string;

  function ReadString(pStream:TStream):string;
  var i:integer;
  begin
    i:=0;
    pStream.Read(i,1);
    Result:='';
    SetLength(Result,i);
    pStream.Read(Result[1],i);
  end;

  function ReadInteger(pStream:TStream):integer;
  begin
    Result:=0;
    pStream.Read(Result,2);
  end;

begin
  Left:=0;
  Top:=0;
  Width:=WINDOWWIDTH;
  Height:=TOPMENUHEIGHT;
  Name:='MainMenu';
  SetLength(fItems,0);
  fSubMenus:=TSubMenuList.Create;
  ZIndex:=LEVEL1CONTROLS_ZINDEX;
  fVisible:=true;
  fSelected:=-1;
  Enabled:=true;
  OnMouseMove:=MouseMove;
  OnMouseLeave:=MouseLeave;
  OnMouseEnter:=MouseEnter;
  MouseObjects.Add(Self);

  // Add menu items
  Xs:=TStringStream.Create(iBINstring);
  try
    x:=0;
    menucount:=0;
    Xs.Read(menucount,1);
    while menucount>0 do begin
      s:=ReadString(Xs);
      SetLength(fItems,length(fItems)+1);
      fItems[length(fItems)-1]:=TMainMenuItem.Init(s);
      atm:=TSubMenu.Create(x);
      atm.name:=s;
      atm.Visible:=false;
      submenucount:=0;
      Xs.Read(submenucount,1);
      while submenucount>0 do begin
        s:=ReadString(Xs);
        msg.TypeID:=ReadInteger(Xs);
        msg.DataInt:=ReadInteger(Xs);
        hint:=ReadString(Xs);
        atm.AddItem(s,hint,msg,msg.TypeID<>MSG_NONE);
        dec(submenucount);
      end;
      fSubMenus.Add(atm);
      x+=(length(fItems[length(fItems)-1]._name)+2)*18;
      MouseObjects.Add(atm);
      dec(menucount);
    end;
  finally
    Xs.Free;
  end;
  SetToolsMenuStates;
  SetInksMenuStates;
end;

destructor TMainMenu.Destroy;
begin
  if Assigned(fSubMenus) then FreeAndNil(fSubMenus);
//  if Assigned(fItems) then FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TMainMenu.EnableCELSubMenusWithActiveCEL;
var submenu:TSubMenu;
begin
  submenu:=fSubMenus.ItemByName('CEL');
  if Assigned(submenu) then begin
    submenu.EnableItem('PUT');
    submenu.EnableItem('RELEASE');
    submenu.EnableItem('ROTATE');
    submenu.EnableItem('FLIP V');
    submenu.EnableItem('FLIP H');
    submenu.EnableItem('MAGNIFY');
    submenu.EnableItem('SAVE');
    submenu.EnableItem('EXPORT');
  end;
end;

procedure TMainMenu.DisableCELSubMenusWithActiveCEL;
var submenu:TSubMenu;
begin
  submenu:=fSubMenus.ItemByName('CEL');
  if Assigned(submenu) then begin
    submenu.DisableItem('PUT');
    submenu.DisableItem('RELEASE');
    submenu.DisableItem('ROTATE');
    submenu.DisableItem('FLIP V');
    submenu.DisableItem('FLIP H');
    submenu.DisableItem('MAGNIFY');
    submenu.DisableItem('SAVE');
    submenu.DisableItem('EXPORT');
  end;
end;

procedure TMainMenu.SetToolsMenuStates;
var submenu:TSubMenu;i:integer;
begin
  submenu:=fSubMenus.ItemByName('TOOLS');
  if Assigned(submenu) then begin
    submenu.EnableAllItems;
    for i:=0 to 5 do submenu.DisableItem(Settings.SelectedTools[i]);
  end;
end;

procedure TMainMenu.SetInksMenuStates;
var submenu:TSubMenu;i:integer;
begin
  submenu:=fSubMenus.ItemByName('INKS');
  if Assigned(submenu) then begin
    submenu.EnableAllItems;
    for i:=0 to 5 do submenu.DisableItem(Settings.SelectedInks[i]);
  end;
end;

procedure TMainMenu.MouseMove(Sender:TObject; x,y:integer);
var pre,mx,i:integer;
begin
  x-=Left;
  mx:=0;
  pre:=fSelected;
  fSelected:=-1;
  for i:=0 to length(fItems)-1 do begin
    if (x>=mx) and (x<mx+(length(fItems[i]._name)+2)*18) then begin
      fSelected:=i;
    end;
    mx+=(length(fItems[i]._name)+2)*18;
  end;
  if fSelected<>pre then begin
    if pre<>-1 then fSubMenus[pre].Hide;
    if (fSelected<>-1) and fItems[fSelected]._enabled then fSubMenus[fSelected].Show;
    fNeedRedraw:=true;
  end;
  if (fSelected<>-1) and not fSubMenus[fSelected].Visible and fItems[fSelected]._enabled then fSubMenus[fSelected].Show;
end;

procedure TMainMenu.MouseLeave(Sender:TObject);
begin
  fSelected:=-1;
  fNeedRedraw:=true;
end;

procedure TMainMenu.MouseEnter(Sender:TObject);
begin
  if (fSelected<>-1) and not fSubMenus[fSelected].Visible and fItems[fSelected]._enabled then
    fSubMenus[fSelected].Show;
end;

function TMainMenu.ProcessMessage(msg:TMessage):boolean;
var submenu:TSubMenu;
begin
  Result:=false;
  case msg.TypeID of
    MSG_ACTIVEIMAGECHANGED:begin
      submenu:=fSubMenus.ItemByName('IMAGE');
      if Assigned(submenu) then begin
        if msg.DataInt=1 then begin
          submenu.DisableItem('REMOVE');
        end else begin
          submenu.EnableItem('REMOVE');
        end;
      end;
      Result:=false;  // Not true, let the others also know about the count change!
    end;
  end;
end;

procedure TMainMenu.DisableItem(item:string);
var i:integer;
begin
  for i:=0 to length(fItems)-1 do
    if fItems[i]._name=item then begin
      fItems[i]._enabled:=false;
      fNeedRedraw:=true;
      exit;
    end;
end;

procedure TMainMenu.EnableItem(item:string);
var i:integer;
begin
  for i:=0 to length(fItems)-1 do
    if fItems[i]._name=item then begin
      fItems[i]._enabled:=true;
      fNeedRedraw:=true;
      exit;
    end;
end;

procedure TMainMenu.ReDraw;
var x,i:integer;
begin
  if Assigned(fTexture) then begin
    fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height-3,SystemPalette[SYSTEMCOLORMID]);
    fTexture.ARGBImage.Bar(0,TOPMENUHEIGHT-3,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height-3,SystemPalette[SYSTEMCOLORDARK]);
    x:=0;
    for i:=0 to length(fItems)-1 do begin
      if fSelected=i then
        fTexture.ARGBImage.Bar(x,0,(length(fItems[i]._name)+2)*18,TOPMENUHEIGHT-3,SystemPalette[SYSTEMCOLORLIGHT]);
      if fItems[i]._enabled then begin
        if fSelected<>i then
          MM.Fonts['Black'].OutText(fTexture.ARGBImage,fItems[i]._name,x+18,3,0)
        else
          MM.Fonts['Red'].OutText(fTexture.ARGBImage,fItems[i]._name,x+18,3,0);
      end else
        MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,fItems[i]._name,x+18,3,0);
      x+=(length(fItems[i]._name)+2)*18;
    end;
    fTexture.Update;
  end;
end;

end.

