{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

program BurdockPaint;

{$ifndef DEBUG}{$apptype gui}{$endif}

uses
  Interfaces,
  SysUtils,
  BDPMain,
  Logger,
  ARGBImageBMPReaderUnit,
  ARGBImageBMPWriterUnit,
  ARGBImageCELReaderUnit,
  ARGBImagePNGReaderUnit,
  ARGBImagePNGWriterUnit,
  ARGBImageTGAReaderUnit,
  ARGBImageTGAWriterUnit,
  ARGBImageGIFReaderUnit,
  FileInfo,
  winpeimagereader;

const
  BDATE={$i %DATE%};

{$R *.res}

function GetVersionString:string;
var
  PV:TProgramVersion;
begin
  GetProgramVersion(PV);
  if PV.Revision=0 then
    Result:=Format('%d.%d build %d',[PV.Major,PV.Minor,PV.Build])
  else
    Result:=Format('%d.%d.%d build %d',[PV.Major,PV.Minor,PV.Revision,PV.Build]);
end;

begin
  try
    with TMain.Create(GetVersionString,BDATE) do try
      Run;
    finally
      Free;
    end;
  except
    on e:exception do
      Log.LogError(e.Message);
  end;
end.


