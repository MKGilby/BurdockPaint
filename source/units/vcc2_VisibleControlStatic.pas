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
//  V1.00: Gilby - 2023.11.17
//    * Initial creation from vcc2_VisibleControl

{$mode delphi}
{$smartlink on}

unit vcc2_VisibleControlStatic;

interface

uses Classes, MKMouse2, mk_sdl2, ARGBImageUnit;

type

  { TVisibleControlStatic }

  TVisibleControlStatic=class(TMouseObject)
    constructor Create; overload;
    destructor Destroy; override;
    // Draws the Control to PrimaryWindow
    procedure Draw; override;
    // Redraws the control to its internal texture
    procedure Refresh; virtual;
  protected
    fNeedRedraw:boolean;
    fTexture:TStaticTexture;
    fImage:TARGBImage;
    procedure fSetHeight(value:integer); override;
    procedure fSetWidth(value:integer); override;
    // This one is called when one of the visible properties is changed.
    // (Size, Enabled, Selected)
    // You have to redraw and update fTexture in this method.
    procedure ReDraw; virtual;
  private
    procedure fSetSelected(value:boolean);
    procedure fSetEnabled(value:boolean);
  public
    OnRecreateTexture:TSimpleEvent;
    property Width:integer read fWidth write fSetWidth;
    property Height:integer read fHeight write fSetHeight;
    property Enabled:boolean read fEnabled write fSetEnabled;
    property Selected:boolean read fSelected write fSetSelected;
  end;
     
implementation

uses SysUtils, MKToolBox, Logger, SDL2;
     
const
  Fstr={$I %FILE%}+', ';
  Version='1.04';


{ TVisibleControlStatic}

constructor TVisibleControlStatic.Create;
begin
  inherited Create;
  fNeedRedraw:=true;
  fWidth:=64;
  fHeight:=24;
  OnRecreateTexture:=nil;
end;

destructor TVisibleControlStatic.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TVisibleControlStatic.Draw;
begin
  if fNeedRedraw then begin
    if Assigned(fTexture) then FreeAndNil(fTexture);
    fImage:=TARGBImage.Create(fWidth,fHeight);
    try
      ReDraw;
      fTexture:=TStaticTexture.Create(fImage);
      SDL_SetTextureBlendMode(fTexture.Texture,SDL_BLENDMODE_BLEND);
    finally
      fImage.Free;
    end;
    fNeedRedraw:=false;
  end;
  if Assigned(fTexture) then PutTexture(Left,Top,fTexture);
end;

procedure TVisibleControlStatic.Refresh;
begin
  fNeedRedraw:=true;
end;

procedure TVisibleControlStatic.fSetHeight(value:integer);
begin
  inherited fSetHeight(value);
  fNeedRedraw:=true;
end;

procedure TVisibleControlStatic.fSetWidth(value:integer);
begin
  inherited fSetWidth(value);
  fNeedRedraw:=true;
end;

procedure TVisibleControlStatic.ReDraw;
begin
  // You have to recreate fTexture in this method.
end;

procedure TVisibleControlStatic.fSetSelected(value:boolean);
begin
  if fSelected<>value then begin
    fSelected:=value;
    fNeedRedraw:=true;
  end;
end;

procedure TVisibleControlStatic.fSetEnabled(value:boolean);
begin
  if fEnabled<>value then begin
    fEnabled:=value;
    fNeedRedraw:=true;
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
