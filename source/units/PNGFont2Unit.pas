{ -[Name]-------------------------------------------

               TPNGFont class for SDL2

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2020

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

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit PNGFont2Unit;

interface

uses Classes, Font2Unit;

type
  TPNGFont=class(TFont)
    constructor Create(iSource:TStream); overload;
    constructor Create(iFileName:string); overload;
  end;

implementation

uses Logger, SysUtils, ARGBImageUnit, ARGBImagePNGReaderUnit, MKStream;

const
  Fstr={$I %FILE%}+', ';
  Version='1.02';

// ------------------------------------------------------------ [ TPNGFont ]---

constructor TPNGFont.Create(iSource:TStream);
var tmp:TARGBImage;
begin
  tmp:=TARGBImage.Create;
  tmp.ReadFile(iSource,'PNG');
  Create(tmp);
  FreeAndNil(tmp);
end;

constructor TPNGFont.Create(iFilename:string);
var Xs:TStream;
begin
  inherited Create;
  iFileName:=ChangeFileExt(iFileName,'.png');
  fName:=iFileName;
  Xs:=MKStreamOpener.OpenStream(iFileName);
  Create(Xs);
  FreeAndNil(Xs);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
