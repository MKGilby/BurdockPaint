{ -[Name]-------------------------------------------

   MKSZTSZ Visual Component Collection for SDL2

                               Visible Control Base

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

   Written by Gilby/MKSZTSZ               Freeware!
   Hungary, 2023

  --------------------------------------------------

  -[Description]------------------------------------

   It is a simple control that has appearance.
   It creates and recreates texture as size changes.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2023.03.14
//    * Initial creation
//  V1.01: Gilby - 2023.04.07
//    * Added ReDraw. You have to redraw and update fTexture in this method.

{$mode delphi}
{$smartlink on}

unit vcc2_VisibleControl;

interface

uses Classes, MKMouse2, mk_sdl2;

type

  { TVisibleControl }

  TVisibleControl=class(TMouseObject)
    constructor Create; overload;
    destructor Destroy; override;
    // Draws the Control to PrimaryWindow
    procedure Draw; override;
  protected
    fTexture:TStreamingTexture;
    procedure fSetHeight(value:integer); override;
    procedure fSetWidth(value:integer); override;
    // This one is called when one of the visible properties is changed.
    // (Size, Enabled, Selected)
    // You have to redraw and update fTexture in this method.
    procedure ReDraw; virtual;
  private
    // Recreates texture bacuse of size change.
    procedure RecreateTexture;
    procedure fSetSelected(value:boolean);
    procedure fSetEnabled(value:boolean);
  public
    property Width:integer read fWidth write fSetWidth;
    property Height:integer read fHeight write fSetHeight;
    property Enabled:boolean read fEnabled write fSetEnabled;
    property Selected:boolean read fSelected write fSetSelected;
  end;
     
implementation

uses SysUtils, MKToolBox, Logger;
     
const
  Fstr={$I %FILE%}+', ';
  Version='1.01';


{ TVisibleControl}

constructor TVisibleControl.Create;
begin
  inherited Create;
  fWidth:=64;
  fHeight:=24;
  RecreateTexture;
end;

destructor TVisibleControl.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TVisibleControl.Draw;
begin
  if Assigned(fTexture) then PutTexture(Left,Top,fTexture);
end;

procedure TVisibleControl.fSetHeight(value:integer);
begin
  inherited fSetHeight(value);
  RecreateTexture;
  ReDraw;
end;

procedure TVisibleControl.fSetWidth(value:integer);
begin
  inherited fSetWidth(value);
  RecreateTexture;
  ReDraw;
end;

procedure TVisibleControl.ReDraw;
begin
  // You have to redraw and update fTexture in this method.
end;

procedure TVisibleControl.RecreateTexture;
begin
  if not Assigned(fTexture) or (fWidth<>fTexture.Width) or (fHeight<>fTexture.Height) then begin
    if (fWidth>0) and (fHeight>0) then begin
      if Assigned(fTexture) then FreeAndNil(fTexture);
      fTexture:=TStreamingTexture.Create(fWidth,fHeight);
    end;
  end;
end;

procedure TVisibleControl.fSetSelected(value:boolean);
begin
  if fSelected<>value then begin
    fSelected:=value;
    ReDraw;
  end;
end;

procedure TVisibleControl.fSetEnabled(value:boolean);
begin
  if fEnabled<>value then begin
    fSelected:=value;
    ReDraw;
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
