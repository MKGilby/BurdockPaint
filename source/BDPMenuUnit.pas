unit BDPMenuUnit;

{$mode Delphi}

interface

uses Classes, mk_sdl2, MKMouse2, fgl, BDPMessageUnit, vcc2_VisibleControl;

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
    procedure Draw; override;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    procedure MouseLeave(Sender:TObject);
    procedure AddItem(item,hint:string;msg:TMessage;enabled:boolean=true);
    procedure DisableItem(item:string);
    procedure EnableItem(item:string);
  private
    fItems:array of TSubMenuItem;
    fSelected:integer;
    procedure fSetName(value:string);
    procedure Resize;
    procedure SetItemEnabled(item:string;value:boolean);
  public
    property Name:string read fName write fSetName;
  end;

  TSubMenuList=TFPGObjectList<TSubMenu>;

  { TMainMenu }

  TMainMenu=class(TVisibleControl)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure EnableCELSubMenusWithActiveCEL;
    procedure DisableCELSubMenusWithActiveCEL;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    procedure MouseLeave(Sender:TObject);
  private
    fSubMenus:TSubMenuList;
    fItems:TStringList;
    fSelected:integer;
  end;

implementation

uses SysUtils, BDPSharedUnit;

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

procedure TSubMenu.Draw;
var i:integer;
begin
  fTexture.ARGBImage.Bar(0,0,(length(fName)+2)*18,TOPMENUHEIGHT-3,OverlayImage.Palette[4]);
  MM.Fonts['Red'].OutText(fTexture.ARGBImage,fName,18,3,0);
  fTexture.ARGBImage.Bar(0,TOPMENUHEIGHT-3,fWidth,3,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(0,TOPMENUHEIGHT-3,3,fHeight-TOPMENUHEIGHT,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(0,fHeight-3,fWidth,3,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(fWidth-3,TOPMENUHEIGHT-3,3,fHeight-TOPMENUHEIGHT,OverlayImage.Palette[2]);
  fTexture.ARGBImage.Bar(3,TOPMENUHEIGHT,fWidth-6,fHeight-TOPMENUHEIGHT-3,OverlayImage.Palette[3]);
  if fSelected>-1 then
    fTexture.ARGBImage.Bar(3,TOPMENUHEIGHT+fSelected*SUBMENULINEHEIGHT,fWidth-6,SUBMENULINEHEIGHT,OverlayImage.Palette[4]);
  for i:=0 to Length(fItems)-1 do begin
    if fItems[i]._enabled then begin
      if i<>fSelected then
        MM.Fonts['Black'].OutText(fTexture.ARGBImage,fItems[i]._name,9,TOPMENUHEIGHT+i*SUBMENULINEHEIGHT+(SUBMENULINEHEIGHT-15) div 2,0)
      else
        MM.Fonts['Red'].OutText(fTexture.ARGBImage,fItems[i]._name,9,TOPMENUHEIGHT+i*SUBMENULINEHEIGHT+(SUBMENULINEHEIGHT-15) div 2,0);
    end else
      MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,fItems[i]._name,9,TOPMENUHEIGHT+i*SUBMENULINEHEIGHT+(SUBMENULINEHEIGHT-15) div 2,0);
  end;
  fTexture.Update;
  PutTexture(fLeft,fTop,fTexture);
end;

function TSubMenu.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  fSelected:=(y-TOPMENUHEIGHT) div SUBMENULINEHEIGHT;
  if (fSelected>=0) and (fSelected<Length(fItems)) then
    InfoBar.ShowText(fItems[fSelected]._hint);
  Result:=true;
end;

function TSubMenu.MouseDown(Sender:TObject; x,y,buttons:integer):boolean;
begin
  fSelected:=(y-TOPMENUHEIGHT) div SUBMENULINEHEIGHT;
  if (fSelected>=0) and (fSelected<Length(fItems)) and (fItems[fSelected]._enabled) then begin
    MessageQueue.AddMessage(fItems[fSelected]._message);
    fSelected:=-1;
    Visible:=false;
  end;
  Result:=true;
end;

procedure TSubMenu.MouseLeave(Sender:TObject);
begin
  fSelected:=-1;
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
    fWidth:=w;
    fHeight:=h;
    RecreateTexture;
  end;
end;

procedure TSubMenu.SetItemEnabled(item:string; value:boolean);
var i:integer;
begin
  for i:=0 to length(fItems)-1 do
    if fItems[i]._name=item then begin
      fItems[i]._enabled:=value;
      break;
    end;
end;

{ TMainMenu }

constructor TMainMenu.Create;
var atm:TSubMenu;x:integer;msg:TMessage;
begin
  Left:=0;
  Top:=0;
  Width:=WINDOWWIDTH;
  Height:=TOPMENUHEIGHT;
  Name:='MainMenu';
  fItems:=TStringList.Create;
  fItems.Add('FILE');
  fItems.Add('IMAGE');
  fItems.Add('CEL');
  fSubMenus:=TSubMenuList.Create;
  fSubMenus.FreeObjects:=true;
  ZIndex:=LEVEL1CONTROLS_ZINDEX;
  fVisible:=true;

  x:=0;
  atm:=TSubMenu.Create(x);
  atm.Name:=fItems[0];
  msg.TypeID:=MSG_NONE;
  atm.AddItem('NEW','',msg,false);
  atm.AddItem('OPEN','',msg,false);
  atm.AddItem('SAVE','',msg,false);
  atm.AddItem('SETTINGS','',msg,false);
  atm.AddItem('QUIT','Save work state and quit program.',TMessage.Init(MSG_QUIT,1));
  atm.Visible:=false;
  fSubMenus.Add(atm);
  x+=(length(fItems[0])+2)*18;
  MouseObjects.Add(atm);

  atm:=TSubMenu.Create(x);
  atm.Name:=fItems[1];
  atm.AddItem('CLEAR','Clear image to key color.',TMessage.Init(MSG_CLEARPICTURE,0));
  atm.AddItem('RESIZE','',msg,false);
  atm.AddItem('EXPORT','',msg,false);
  atm.Visible:=false;
  fSubMenus.Add(atm);
  x+=(length(fItems[1])+2)*18;
  MouseObjects.Add(atm);

  atm:=TSubMenu.Create(x);
  atm.Name:=fItems[2];
  atm.AddItem('GET','Get a part of the image into a temporary image (CEL).',TMessage.Init(MSG_GETCEL,0));
  atm.AddItem('PUT','Put CEL to the image.',TMessage.Init(MSG_PUTCEL,0));
  atm.AddItem('RELEASE','Forget current CEL.',TMessage.Init(MSG_RELEASECEL,0));
  atm.AddItem('ROTATE','Rotate CEL by 90°, 180° or 270°.',TMessage.Init(MSG_OPENROTATECELDIALOG,0));
  atm.AddItem('FLIP V','Flip CEL vertically.',TMessage.Init(MSG_FLIPCEL,0));
  atm.AddItem('FLIP H','Flip CEL horizontally.',TMessage.Init(MSG_FLIPCEL,1));
  atm.AddItem('MAGNIFY','Magnify CEL to 2x, 3x or 5x.',TMessage.Init(MSG_OPENMAGNIFYCELDIALOG,0));
  atm.AddItem('LOAD','Load CEL from file. (BDC or Legacy CEL)',TMessage.Init(MSG_LOADCEL,0));
  atm.AddItem('SAVE','Save CEL to file. (BDC)',TMessage.Init(MSG_SAVECEL,0));
  atm.AddItem('EXPORT','Export CEL to file. (PNG)',msg,false);
  atm.Visible:=false;
  fSubMenus.Add(atm);
  x+=(length(fItems[2])+2)*18;
  MouseObjects.Add(atm);

  fSelected:=-1;
  Enabled:=true;
  OnMouseDown:=MouseDown;
  OnMouseMove:=MouseMove;
  OnMouseLeave:=MouseLeave;
  MouseObjects.Add(Self);
end;

destructor TMainMenu.Destroy;
begin
  if Assigned(fSubMenus) then FreeAndNil(fSubMenus);
  if Assigned(fItems) then FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TMainMenu.Draw;
var x,i:integer;
begin
  fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height-3,OverlayImage.Palette[3]);
  fTexture.ARGBImage.Bar(0,TOPMENUHEIGHT-3,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height-3,OverlayImage.Palette[2]);
  x:=0;
  for i:=0 to fItems.Count-1 do begin
    if fSelected<>i then
      MM.Fonts['Black'].OutText(fTexture.ARGBImage,fItems[i],x+18,3,0)
    else begin
      fTexture.ARGBImage.Bar(x,0,(length(fItems[i])+2)*18,TOPMENUHEIGHT-3,OverlayImage.Palette[4]);
      MM.Fonts['Red'].OutText(fTexture.ARGBImage,fItems[i],x+18,3,0);
    end;
    x+=(length(fItems[i])+2)*18;
  end;
  fTexture.Update;
  PutTexture(fLeft,fTop,fTexture);
end;

procedure TMainMenu.EnableCELSubMenusWithActiveCEL;
var submenu:TSubMenu;
begin
  submenu:=fSubMenus[fItems.IndexOf('CEL')];
  if Assigned(submenu) then begin
    submenu.EnableItem('PUT');
    submenu.EnableItem('RELEASE');
    submenu.EnableItem('ROTATE');
    submenu.EnableItem('FLIP V');
    submenu.EnableItem('FLIP H');
    submenu.EnableItem('MAGNIFY');
    submenu.EnableItem('SAVE');
//    submenu.EnableItem('EXPORT');
  end;
end;

procedure TMainMenu.DisableCELSubMenusWithActiveCEL;
var submenu:TSubMenu;
begin
  submenu:=fSubMenus[fItems.IndexOf('CEL')];
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

function TMainMenu.MouseMove(Sender:TObject; x,y:integer):boolean;
var pre,mx,i:integer;
begin
  x-=Left;
  mx:=0;
  pre:=fSelected;
  fSelected:=-1;
  for i:=0 to fItems.Count-1 do begin
    if (x>=mx) and (x<mx+(length(fItems[i])+2)*18) then begin
      fSelected:=i;
    end;
    mx+=(length(fItems[i])+2)*18;
  end;
  if fSelected<>pre then begin
    if pre<>-1 then fSubMenus[pre].Visible:=false;
    if fSelected<>-1 then fSubMenus[fSelected].Visible:=true;
  end;
  if (fSelected<>-1) and not fSubMenus[fSelected].Visible then fSubMenus[fSelected].Visible:=true;
  Result:=true;
end;

function TMainMenu.MouseDown(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

procedure TMainMenu.MouseLeave(Sender:TObject);
begin
//  if fVisible then SDL_ShowCursor(SDL_DISABLE);
//  if fSelected<>-1 then fSubMenus[fSelected].Visible:=false;
  fSelected:=-1;
end;

end.

