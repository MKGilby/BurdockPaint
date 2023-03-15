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

{$mode delphi}
{$smartlink on}

unit vcc2_VisibleControl;

interface

uses Classes, MKMouse2, mk_sdl2;

type

  { TButton }

  TVisibleControl=class(TMouseObject)
    constructor Create; overload;
    destructor Destroy; override;
  protected
    fTexture:TStreamingTexture;
    procedure fSetHeight(value:integer); override;
    procedure fSetWidth(value:integer); override;
    procedure RecreateTexture;
  public
    property Width:integer read fWidth write fSetWidth;
    property Height:integer read fHeight write fSetHeight;
  end;
     
implementation

uses SysUtils, MKToolBox, Logger;
     
const
  Fstr={$I %FILE%}+', ';
  Version='1.00';



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

procedure TVisibleControl.fSetHeight(value:integer);
begin
  inherited fSetHeight(value);
  RecreateTexture;
end;

procedure TVisibleControl.fSetWidth(value:integer);
begin
  inherited fSetWidth(value);
  RecreateTexture;
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

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
