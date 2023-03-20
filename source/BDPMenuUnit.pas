unit BDPMenuUnit;

{$mode Delphi}

interface

uses Classes, mk_sdl2, MKMouse2, fgl, BDPMessageUnit, vcc2_VisibleControl;

type

  { TSubMenuItem }

  TSubMenuItem=record
    _name:string;
    _message:TMessage;
    _enabled:boolean;
    constructor Init(iName:string;iMessage:TMessage);
  end;

  { TSubMenu }

  TSubMenu=class(TVisibleControl)
    constructor Create(iLeft:integer);
    procedure Draw; override;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    procedure MouseLeave(Sender:TObject);
    procedure AddItem(item:string;msg:TMessage;enabled:boolean=true);
  private
    fItems:array of TSubMenuItem;
    fSelected:integer;
    procedure fSetName(value:string);
    procedure Resize;
  public
    property Name:string read fName write fSetName;
  end;

  TSubMenuList=TFPGObjectList<TSubMenu>;

  { TMainMenu }

  TMainMenu=class(TVisibleControl)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
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

constructor TSubMenuItem.Init(iName:string; iMessage:TMessage);
begin
  _name:=iName;
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
//  OnMouseEnter:=MouseEnter;
end;

procedure TSubMenu.Draw;
var i:integer;
begin
  if fVisible then begin
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
end;

function TSubMenu.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  fSelected:=(y-TOPMENUHEIGHT) div SUBMENULINEHEIGHT;
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
  Visible:=false;
end;

procedure TSubMenu.AddItem(item:string; msg:TMessage; enabled:boolean);
begin
  if item<>'' then begin
    SetLength(fItems,Length(fItems)+1);
    fItems[Length(fItems)-1]:=TSubMenuItem.Init(item,msg);
    fItems[Length(fItems)-1]._enabled:=enabled;
    Resize;
  end;
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
{    if Assigned(fTexture) then FreeAndNil(fTexture);
    fTexture:=TStreamingTexture.Create(fWidth,fHeight);
    fTexture.ARGBImage.Clear;
    fTexture.Update;}
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
  fItems.Add('PICTURE');
  fItems.Add('CEL');
  fSubMenus:=TSubMenuList.Create;
  fSubMenus.FreeObjects:=true;
  ZIndex:=LEVEL1CONTROLS_ZINDEX;
  x:=0;

  atm:=TSubMenu.Create(x);
  atm.Name:=fItems[0];
  msg.TypeID:=MSG_NONE;
  atm.AddItem('NEW',msg,false);
  atm.AddItem('OPEN',msg,false);
  atm.AddItem('SAVE',msg,false);
  atm.AddItem('SETTINGS',msg,false);
  atm.AddItem('QUIT',TMessage.Init(MSG_QUIT,1));
  atm.Visible:=false;
  fSubMenus.Add(atm);
  x+=(length(fItems[0])+2)*18;
  MouseObjects.Add(atm);

  atm:=TSubMenu.Create(x);
  atm.Name:=fItems[1];
  atm.AddItem('CLEAR',TMessage.Init(MSG_CLEARPICTURE,0));
  atm.AddItem('RESIZE',msg,false);
  atm.AddItem('EXPORT',msg,false);
  atm.Visible:=false;
  fSubMenus.Add(atm);
  x+=(length(fItems[1])+2)*18;
  MouseObjects.Add(atm);

  atm:=TSubMenu.Create(x);
  atm.Name:=fItems[2];
  atm.AddItem('GET',TMessage.Init(MSG_GETCEL,0));
  atm.AddItem('PUT',msg,false);
  atm.AddItem('RELEASE',TMessage.Init(MSG_RELEASECEL,0));
  atm.AddItem('ROTATE',msg,false);
  atm.AddItem('FLIP V',msg,false);
  atm.AddItem('FLIP H',msg,false);
  atm.AddItem('LOAD',TMessage.Init(MSG_LOADCEL,0));
  atm.AddItem('SAVE',msg,false);
  atm.AddItem('EXPORT',msg,false);
  atm.Visible:=false;
  fSubMenus.Add(atm);
  x+=(length(fItems[2])+2)*18;
  MouseObjects.Add(atm);

  fSelected:=-1;
  Enabled:=true;
  OnMouseDown:=MouseDown;
  OnMouseMove:=MouseMove;
  OnMouseLeave:=MouseLeave;
//  OnMouseEnter:=MouseEnter;
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
  if fVisible then begin
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

