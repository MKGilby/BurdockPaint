program BurdockPaint;

uses
  BDPMainUnit,
  ARGBImageBMPReaderUnit,
  ARGBImageCELReaderUnit,
  ARGBImagePNGReaderUnit,
  ARGBImagePNGWriterUnit,
  ARGBImageTGAReaderUnit,
  ARGBImageTGAWriterUnit, BDPSliderUnit;

const
  VERSION='0.9';
  BDATE={$i %DATE%};

var
  Main:TMain;

{$R *.res}

begin
  Main:=TMain.Create(VERSION,BDATE);
  try
    Main.Run;
  finally
    Main.Free;
  end;
end.


