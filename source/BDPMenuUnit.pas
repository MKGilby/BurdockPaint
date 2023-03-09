unit BDPMenuUnit;

{$mode Delphi}

interface

uses Classes, mk_sdl2, MKMouse2, fgl;

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
    procedure AddItem(item:string);
  private
    fTexture:TStreamingTexture;
    fItems:TStringList;
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
  Height:=MENUHEIGHT;
  fTexture:=TStreamingTexture.Create(fWidth,fHeight);
  fTexture.ARGBImage.Clear;
  fTexture.Update;
  fItems:=TStringList.Create;
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
    fTexture.ARGBImage.Bar(0,0,(length(fName)+2)*18,MENUHEIGHT-3,OverlayImage.Palette[4]);
    MM.Fonts['Red'].OutText(fTexture.ARGBImage,fName,18,3,0);
    fTexture.ARGBImage.Bar(0,MENUHEIGHT-3,fWidth,3,OverlayImage.Palette[2]);
    fTexture.ARGBImage.Bar(0,MENUHEIGHT-3,3,fHeight-MENUHEIGHT,OverlayImage.Palette[2]);
    fTexture.ARGBImage.Bar(0,fHeight-3,fWidth,3,OverlayImage.Palette[2]);
    fTexture.ARGBImage.Bar(fWidth-3,MENUHEIGHT-3,3,fHeight-MENUHEIGHT,OverlayImage.Palette[2]);
    fTexture.ARGBImage.Bar(3,MENUHEIGHT,fWidth-6,fHeight-MENUHEIGHT-3,OverlayImage.Palette[3]);
    for i:=0 to fItems.Count-1 do
      MM.Fonts['Black'].OutText(fTexture.ARGBImage,fItems[i],9,3+i*21+MENUHEIGHT,0);
    fTexture.Update;
    PutTexture(fLeft,fTop,fTexture);
  end;
end;

function TSubMenu.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  Result:=true;
end;

function TSubMenu.MouseDown(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

procedure TSubMenu.MouseEnter(Sender:TObject);
begin
  if fVisible then SDL_ShowCursor(SDL_ENABLE);
end;

procedure TSubMenu.MouseLeave(Sender:TObject);
begin
  Visible:=false;
end;

procedure TSubMenu.AddItem(item:string);
begin
  if item<>'' then begin
    fItems.Add(item);
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
  h:=MENUHEIGHT+fItems.Count*21+3;
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
var atm:TSubMenu;x:integer;
begin
  Left:=0;
  Top:=0;
  Width:=WINDOWWIDTH;
  Height:=MENUHEIGHT;
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
  atm.AddItem('NEW');
  atm.AddItem('OPEN');
  atm.AddItem('SAVE');
  atm.AddItem('SETTINGS');
  atm.AddItem('QUIT');
  atm.Visible:=false;
  fSubMenus.Add(atm);
  x+=(length(fItems[0])+2)*18;
  MouseObjects.Add(atm);

  atm:=TSubMenu.Create(x);
  atm.Name:=fItems[1];
  atm.AddItem('CLEAR');
  atm.AddItem('RESIZE');
  atm.AddItem('EXPORT');
  atm.Visible:=false;
  fSubMenus.Add(atm);
  x+=(length(fItems[1])+2)*18;
  MouseObjects.Add(atm);

  atm:=TSubMenu.Create(x);
  atm.Name:=fItems[2];
  atm.AddItem('GET');
  atm.AddItem('PUT');
  atm.AddItem('RELEASE');
  atm.AddItem('ROTATE');
  atm.AddItem('FLIP V');
  atm.AddItem('FLIP H');
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
    fTexture.ARGBImage.Bar(0,MENUHEIGHT-3,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height-3,OverlayImage.Palette[2]);
    x:=0;
    for i:=0 to fItems.Count-1 do begin
      if fSelected<>i then
        MM.Fonts['Black'].OutText(fTexture.ARGBImage,fItems[i],x+18,3,0)
      else begin
        fTexture.ARGBImage.Bar(x,0,(length(fItems[i])+2)*18,MENUHEIGHT-3,OverlayImage.Palette[4]);
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

