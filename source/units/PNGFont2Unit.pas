{ -[Name]-------------------------------------------

               TPNGFont class for SDL2

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 3
             (or newer).

  Written by Gilby/MKSZTSZ
  Hungary, 2023

  --------------------------------------------------

  -[Description]------------------------------------

   Loads a PNGFont file (special PNG, containing font data).
   
   To create PNGFont files use MKConv (our image converter)
           
  -------------------------------------------------- }

// Version info:
//   V1.00: Gilby - 2020.04.01
//      - Initial creation from TGAFont2Unit
//   V1.01: Gilby - 2020.06.26
//      * Dummy image creation is changed to Exception
//   V1.02: Gilby - 2021.08.12
//      * Following changes in Font2Unit
//   V1.03: Gilby - 2023.11.16
//      * Adding flags to Create.

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit PNGFont2Unit;

interface

uses Classes, Font2Unit;

type

  { TPNGFont }

  TPNGFont=class(TFont)
    constructor Create(iSource:TStream;iCreateFlags:integer=FONT_CREATE_BOTH); overload;
    constructor Create(iFileName:string;iCreateFlags:integer=FONT_CREATE_BOTH); overload;
  end;

implementation

uses Logger, SysUtils, ARGBImageUnit, ARGBImagePNGReaderUnit, MKStream;

const
  Fstr={$I %FILE%}+', ';
  Version='1.03';

// ------------------------------------------------------------ [ TPNGFont ]---

constructor TPNGFont.Create(iSource:TStream; iCreateFlags:integer);
var tmp:TARGBImage;
begin
  tmp:=TARGBImage.Create;
  tmp.ReadFile(iSource,'PNG');
  Create(tmp,iCreateFlags);
  tmp.Free;
end;

constructor TPNGFont.Create(iFileName:string; iCreateFlags:integer);
var Xs:TStream;
begin
  inherited Create;
  iFileName:=ChangeFileExt(iFileName,'.png');
  fName:=iFileName;
  Xs:=MKStreamOpener.OpenStream(iFileName);
  Create(Xs,iCreateFlags);
  Xs.Free;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
