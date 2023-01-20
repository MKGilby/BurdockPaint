{ -[Name]-------------------------------------------

                  TFontData class

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2020

  --------------------------------------------------

  -[Description]------------------------------------

   Base for TFont class, but without images

  -------------------------------------------------- }

// Version info:
//   V1.00: Gilby - 2020.03.16
//      - Initial creation from Font2Unit

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit FontDataUnit;

interface

uses SDL2;

type
  TFontData=class
    constructor Create;
    procedure SetCharBox(c,x,y,w,h:integer);
  protected
    // Character box for each char. Set width to 0 if the char is not present in font.
    fCharBoxes:array [0..255] of TSDL_Rect;
    function fGetCharBox(index:integer):TSDL_Rect;
  public
    property CharBoxes[index:integer]:TSDL_Rect read fGetCharBox;
  end;

implementation

uses sysutils, Logger;

const Fstr='FontDataUnit.pas, ';
      Version='1.00';

// ----------------------------------------------------------- [ TFontData ]---

constructor TFontData.Create;
var i:integer;
begin
  for i:=0 to 255 do
    fCharBoxes[i].w:=0;
end;

procedure TFontData.SetCharBox(c,x,y,w,h:integer);
begin
  if (c>=0) and (c<256) then begin
    fCharBoxes[c].x:=x;
    fCharBoxes[c].y:=y;
    fCharBoxes[c].w:=w;
    fCharBoxes[c].h:=h;
  end else raise Exception.Create(Format('Invalid character index in TFontData.SetCharBox! (%d)',[c]));
end;

function TFontData.fGetCharBox(index:integer):TSDL_Rect;
begin
  if (index>=0) and (index<256) then
    Result:=fCharBoxes[index]
  else
    Result:=fCharBoxes[0];
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
