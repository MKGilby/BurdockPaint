unit BDPMenuUnit;

{$mode Delphi}

interface

uses Classes, mk_sdl2, MKMouse2, fgl, BDPMessageUnit;

type

  { TSubMenu }

  TSubMenu=class(TMouseObject)
    constructor Create(iLeft:integer);
    destructor Destroy; override;
    procedure Draw; override;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
    procedure AddItem(item:string;msg:TMessage);
  private
    fTexture:TStreamingTexture;
    fItems:TStringList;
    fMessages:array of TMessage;
    fSelected:integer;
    procedure fSetName(value:string);
    procedure RecreateTexture;
  public
    property Name:string read fName write fSetName;
  end;

  TSubMenuList=TFPGObjectList<TSubMenu>;

  { TMainMenu }

  TMainMenu=class(TMouseObject)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
  private
    fTexture:TStreamingTexture;
    fSubMenus:TSubMenuList;
    fItems:TStringList;
    fSelected:integer;
  end;

implementation

uses SysUtils, BDPSharedUnit, SDL2;

{ TSubMenu }

constructor TSubMenu.Create(iLeft:integer);
begin
  Left:=iLeft;
  Top:=0;
  Width:=180;
  Height:=TOPMENUHEIGHT;
  fTexture:=TStreamingTexture.Create(fWidth,fHeight);
  fTexture.ARGBImage.Clear;
  fTexture.Update;
  fItems:=TStringList.Create;
  SetLength(fMessages,0);
  fSelected:=-1;
  ZIndex:=9;
  Enabled:=true;
  OnMouseDown:=MouseDown;
  OnMouseMove:=MouseMove;
  OnMouseLeave:=MouseLeave;
  OnMouseEnter:=MouseEnter;
end;

destructor TSubMenu.Destroy;
begin
  if Assigned(fItems) then FreeAndNil(fItems);
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
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
    for i:=0 to fItems.Count-1 do
      if i<>fSelected then
        MM.Fonts['Black'].OutText(fTexture.ARGBImage,fItems[i],9,TOPMENUHEIGHT+i*SUBMENULINEHEIGHT+(SUBMENULINEHEIGHT-15) div 2,0)
      else
        MM.Fonts['Red'].OutText(fTexture.ARGBImage,fItems[i],9,TOPMENUHEIGHT+i*SUBMENULINEHEIGHT+(SUBMENULINEHEIGHT-15) div 2,0);
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
  if (fSelected>=0) and (fSelected<fItems.Count) then begin
    MessageQueue.AddMessage(fMessages[fSelected]);
    fSelected:=-1;
    Visible:=false;
  end;
  Result:=true;
end;

procedure TSubMenu.MouseEnter(Sender:TObject);
begin
  if fVisible then SDL_ShowCursor(SDL_ENABLE);
end;

procedure TSubMenu.MouseLeave(Sender:TObject);
begin
  fSelected:=-1;
  Visible:=false;
end;

procedure TSubMenu.AddItem(item:string; msg:TMessage);
begin
  if item<>'' then begin
    fItems.Add(item);
    SetLength(fMessages,Length(fMessages)+1);
    fMessages[length(fMessages)-1]:=msg;
    RecreateTexture;
  end;
end;

procedure TSubMenu.fSetName(value:string);
begin
  fName:=value;
  RecreateTexture;
end;

procedure TSubMenu.RecreateTexture;
var i,w,h:integer;
begin
  w:=0;
  if (length(fName)+2)*18>w then w:=(length(fName)+2)*18;
  for i:=0 to fItems.Count-1 do
    if (length(fItems[i])+1)*18>w then w:=(length(fItems[i])+1)*18;
  h:=TOPMENUHEIGHT+fItems.Count*SUBMENULINEHEIGHT+3;
  if (w<>fWidth) or (h<>fHeight) then begin
    fWidth:=w;
    fHeight:=h;
    if Assigned(fTexture) then FreeAndNil(fTexture);
    fTexture:=TStreamingTexture.Create(fWidth,fHeight);
    fTexture.ARGBImage.Clear;
    fTexture.Update;
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
  fTexture:=TStreamingTexture.Create(fWidth,fHeight);
  fTexture.ARGBImage.Clear;
  fTexture.Update;
  fItems:=TStringList.Create;
  fItems.Add('FILE');
  fItems.Add('PICTURE');
  fItems.Add('CEL');
  fSubMenus:=TSubMenuList.Create;
  fSubMenus.FreeObjects:=true;
  x:=0;

  atm:=TSubMenu.Create(x);
  atm.Name:=fItems[0];
  msg.TypeID:=MSG_NONE;
  atm.AddItem('NEW',msg);
  atm.AddItem('OPEN',msg);
  atm.AddItem('SAVE',msg);
  atm.AddItem('SETTINGS',msg);
  atm.AddItem('QUIT',msg);
  atm.Visible:=false;
  fSubMenus.Add(atm);
  x+=(length(fItems[0])+2)*18;
  MouseObjects.Add(atm);

  atm:=TSubMenu.Create(x);
  atm.Name:=fItems[1];
  atm.AddItem('CLEAR',msg);
  atm.AddItem('RESIZE',msg);
  atm.AddItem('EXPORT',msg);
  atm.Visible:=false;
  fSubMenus.Add(atm);
  x+=(length(fItems[1])+2)*18;
  MouseObjects.Add(atm);

  atm:=TSubMenu.Create(x);
  atm.Name:=fItems[2];
  atm.AddItem('GET',msg);
  atm.AddItem('PUT',msg);
  atm.AddItem('RELEASE',msg);
  atm.AddItem('ROTATE',msg);
  atm.AddItem('FLIP V',msg);
  atm.AddItem('FLIP H',msg);
  atm.AddItem('LOAD',TMessage.Init(MSG_OPENCEL,0));
  atm.AddItem('SAVE',msg);
  atm.AddItem('EXPORT',msg);
  atm.Visible:=false;
  fSubMenus.Add(atm);
  x+=(length(fItems[2])+2)*18;
  MouseObjects.Add(atm);

  fSelected:=-1;
  Enabled:=true;
  OnMouseDown:=MouseDown;
  OnMouseMove:=MouseMove;
  OnMouseLeave:=MouseLeave;
  OnMouseEnter:=MouseEnter;
end;

destructor TMainMenu.Destroy;
begin
  if Assigned(fSubMenus) then FreeAndNil(fSubMenus);
  if Assigned(fItems) then FreeAndNil(fItems);
  if Assigned(fTexture) then FreeAndNil(fTexture);
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

procedure TMainMenu.MouseEnter(Sender:TObject);
begin
  if fVisible then SDL_ShowCursor(SDL_ENABLE);
end;

procedure TMainMenu.MouseLeave(Sender:TObject);
begin
//  if fVisible then SDL_ShowCursor(SDL_DISABLE);
//  if fSelected<>-1 then fSubMenus[fSelected].Visible:=false;
  fSelected:=-1;
end;

end.

