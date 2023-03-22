unit BDPModalDialogUnit;

{$mode Delphi}

interface

uses vcc2_Container, mk_sdl2;

type

  { TModalDialog }

  TModalDialog=class(TContainer)
    constructor Create(iWidth,iHeight:integer);
    destructor Destroy; override;
    procedure Draw; override;
    function Click(Sender:TObject;x,y,buttons: integer):boolean;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseUp(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer):boolean;
    function KeyDown(Sender:TObject;key:integer):boolean;
    function KeyUp(Sender:TObject;key:integer):boolean;
  protected
    fTexture:TStreamingTexture;
    fWindowLeft,fWindowTop:integer;
  end;

implementation

uses SysUtils, BDPSharedUnit;

{ TModalDialog }

constructor TModalDialog.Create(iWidth,iHeight:integer);
begin
  inherited Create;
  SetBoundsWH(0,0,WINDOWWIDTH,WINDOWHEIGHT);
  fWindowLeft:=(WINDOWWIDTH-iWidth) div 2;
  fWindowTop:=(WINDOWHEIGHT-iHeight) div 2;
  fTexture:=TStreamingTexture.Create(iWidth,iHeight);
  OnMouseMove:=MouseMove;
  OnMouseDown:=MouseDown;
  OnMouseUp:=MouseUp;
  OnMouseWheel:=MouseWheel;
  OnClick:=Click;
  OnKeyDown:=KeyDown;
  OnKeyUp:=KeyUp;
  ZIndex:=MODALDIALOG_ZINDEX;
end;

destructor TModalDialog.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TModalDialog.Draw;
begin
  if fVisible then
    PutTexture(fWindowLeft,fWindowTop,fTexture);
end;

function TModalDialog.Click(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TModalDialog.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  Result:=true;
end;

function TModalDialog.MouseDown(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TModalDialog.MouseUp(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TModalDialog.MouseWheel(Sender:TObject; x,y,wheelx,wheely:integer):boolean;
begin
  Result:=true;
end;

function TModalDialog.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

function TModalDialog.KeyUp(Sender:TObject; key:integer):boolean;
begin
  Result:=true;
end;

end.

