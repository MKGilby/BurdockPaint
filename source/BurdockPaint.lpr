program BurdockPaint;

{$ifndef DEBUG}{$apptype gui}{$endif}

uses
  Interfaces,
  BDPMain,
  ARGBImageBMPReaderUnit,
  ARGBImageCELReaderUnit,
  ARGBImagePNGReaderUnit,
  ARGBImagePNGWriterUnit,
  ARGBImageTGAReaderUnit,
  ARGBImageTGAWriterUnit;

const
  VERSION='0.9';
  BDATE={$i %DATE%};

{$R *.res}

begin
  with TMain.Create(VERSION,BDATE) do
    try
      Run;
    finally
      Free;
    end;
end.


