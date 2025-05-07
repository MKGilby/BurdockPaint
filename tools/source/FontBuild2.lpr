{
  V1.0.0
    - Initial creation
  V1.0.1
    - Fixed width font won't save the whole image
    - Added BDC reader
    - Added GIF reader
  V1.0.2
    - Updated BDC reader
  V1.0.3
    - Removed BDC reader (using PNG instead)
    - Using top-left pixel as empty pixel when scanning
  V1.0.4
    - Added -help
}

program FontBuild2;

{$mode delphi}{$H+}

uses
  SysUtils,
  ParametersUnit,
  FontBuilderUnit,
  ARGBImageUnit,
//  ARGBImageBMPReaderUnit,
  ARGBImageCELReaderUnit,
//  ARGBImageTGAReaderUnit,
  ARGBImagePNGReaderUnit,
//  ARGBImageGIFReaderUnit,
  Logger,
  MKToolbox,
  MKStream,
  FileInfo,
  winpeimagereader;


const
  BDATE={$i %DATE%};

type

  { TMain }

  TMain=class
    constructor Create;
    procedure Run;
  private
    InputFilename,OutputFilename:string;
    CharSet:string;
    IsFixedWidth,
    IsColorKey,
    IsSort:boolean;
    CharSetIndex:integer;
    FontBuilder:TFontBuilder;
    function DecodeCharset(input:string):string;
    function GetParameters:boolean;
    procedure ProcessRow(pImage:TARGBImage;pTop,pMarker:integer;pEmptyColor:uint32);
    procedure LoadImages;
  end;

function GetVersionString:string;
var
  PV:TProgramVersion;
begin
  GetProgramVersion(PV);
  if PV.Revision=0 then
    if PV.Build=0 then
      Result:=Format('%d.%d',[PV.Major,PV.Minor])
    else
      Result:=Format('%d.%d build %d',[PV.Major,PV.Minor,PV.Build])
  else
    if PV.Build=0 then
      Result:=Format('%d.%d.%d',[PV.Major,PV.Minor,PV.Revision])
    else
      Result:=Format('%d.%d.%d build %d',[PV.Major,PV.Minor,PV.Revision,PV.Build]);
end;

constructor TMain.Create;
begin
  IsSort:=false;
  IsFixedWidth:=false;
  IsColorKey:=false;
  OutputFilename:='';
  MKStreamOpener.AddDirectory('.',0);
end;

procedure TMain.Run;
begin
  if GetParameters then begin
    FontBuilder:=TFontBuilder.Create(IsSort,IsFixedWidth,IsColorKey,true);
    try
    LoadImages;
    writeln('Constructing font...');
    FontBuilder.BuildFont(ChangeFileExt(OutputFilename,'.png'));
    finally
      FontBuilder.Free;
    end;
    writeln('Ready.');
  end;
end;

// Changes all #nn into chr(hextoint(nn))
function TMain.DecodeCharset(input:string):string;
const HexChars='0123456789ABCDEF';
begin
  Result:='';
  while length(input)>0 do begin
    if input[1]='#' then begin
      delete(input,1,1);
      if length(input)>1 then begin
        if (pos(upcase(input[1]),HexChars)>0) and (pos(upcase(input[2]),HexChars)>0) then begin
          Result+=chr(hextoint(UpperCase(copy(input,1,2))));
          delete(input,1,2);
        end else begin
          Result+='#';  // Keep it if there's no chars to decode
        end;
      end else begin
        Result+='#';  // Keep it if there's no chars to decode
      end;
    end else begin
      Result+=input[1];
      delete(input,1,1);
    end;
  end;
end;

function TMain.GetParameters:boolean;
begin
  Result:=false;
  writeln(Format('MKSZTSZ Font Builder2 V%s - (C) 2021-%s Gilby/MKSZTSZ',[GetVersionString,copy(BDATE,1,4)]));

  if Parameters.IndexOfSwitch('help')>-1 then begin
    writeln('This font builder reads an image file and creates a PNGFont from it, ');
    writeln('using the following algorythm to cut letter images:');
    writeln;
    writeln('The image consist at least 3 horizontal zones:');
    writeln;
    writeln('  <FontFace>');
    writeln('    Containing the real images of the letters. Any height.');
    writeln('  <Divider>');
    writeln('    One pixel height empty (completely black) line.');
    writeln('  <CharMarker>');
    writeln('    One pixel height line, marking characters. One consecutive non-black');
    writeln('    pixel row is one character.');
    writeln('  <Divider>');
    writeln('    One pixel height empty (completely black) line, before the next <FontFace>.');
    writeln('    Don''t needed if there are no more zones.');
    writeln;
    writeln('First column must be total empty, except non-black pixels showing the');
    writeln('<CharMarker> lines. Second column is total empty, won''t be processed.');
    writeln;
    writeln('The top-leftmost pixel will be used as empty color for scanning.');
    writeln;
    writeln('Usage:');
    writeln;
    writeln('  FontBuild2 inputfilename outputfilename -charset "charset" ');
    writeln('             [-fixed|-fixedwidth] [-colorkey] [-sort]');
    writeln;
    writeln('    -fixed or -fixedwith: All letters will share the widest letter''s ');
    writeln('     width, smaller letters will be centered within this space.');
    writeln('    -colorkey: Sets the black color transparent.');
    writeln('    -sort: Makes the font be constructed in ascii order');
    writeln('           instead of the order in Chars.');
    writeln;
    writeln('Example:');
    writeln;
    writeln('  Font1.png image containing ''A'',''B'' and ''!'' characters:');
    writeln;
    writeln('  ...xxx..xxxx..x <FontFace>');
    writeln('  ..x...x.x...x.x');
    writeln('  ..xxxxx.xxxx..x');
    writeln('  ..x...x.x...x..');
    writeln('  ..x...x.xxxx..x');
    writeln('  ............... <Divider>');
    writeln('  x.xxxxx.xxxxx.x <CharMarker>');
    writeln;
    writeln('  To create Font2.png containing font data, run:');
    writeln;
    writeln('    FontBuild2 image -chars "AB!"');
  end else begin
    try
      IsFixedWidth:=Parameters.IndexOfSwitch('fixed',true)>-1;
      IsFixedWidth:=IsFixedWidth or (Parameters.IndexOfSwitch('fixedwidth',true)>-1);
      IsColorKey:=Parameters.IndexOfSwitch('colorkey',true)>-1;
      IsSort:=Parameters.IndexOfSwitch('sort',true)>-1;
      Charset:=Parameters.GetNextOfSwitch('charset',true);
      if Charset='' then raise Exception.Create('Missing charset parameter!');
      Charset:=DecodeCharset(Charset);
      if Parameters.Count>1 then begin
        InputFilename:=Parameters[1];
        if Parameters.Count>2 then begin
          OutputFilename:=Parameters[2];
        end else raise Exception.Create('Missing outputfile parameter!');
      end else raise Exception.Create('Missing infile parameter!');
      Result:=true;
    except
      on e:Exception do begin
        writeln;
        writeln('Error: '+e.Message);
        writeln;
        writeln('Use -help for help.');
      end;
    end;
  end;
end;

procedure TMain.ProcessRow(pImage:TARGBImage;pTop,pMarker:integer;pEmptyColor:uint32);
var fMode:(mScanForPixel,mScanForEmpty);
  i,pre:integer;
  tmp:TARGBImage;
begin
  pre:=-1;
  fMode:=mScanForPixel;
  tmp:=nil;
  for i:=2 to pImage.Width-1 do begin
    if (pImage.GetPixel(i,pMarker) and $ffffff<>pEmptyColor) then begin
      if fMode=mScanForPixel then begin
        fMode:=mScanForEmpty;
        pre:=i;
      end else begin
        // Do nothing
      end;
    end else begin  // empty
      if fMode=mScanForEmpty then begin
        Log.LogDebug(Format('pre=%d, i=%d, i-pre=%d',[pre,i,i-pre]));
        if pMarker-pTop>0 then begin
          tmp:=TARGBImage.Create(i-pre,pMarker-pTop-1);
          try
            pImage.Copy(pre,pTop,i-pre,pMarker-pTop-1,tmp);
            if CharSetIndex>length(CharSet) then
              raise Exception.Create(Format('Not enough characters in charset! (%d)',[CharSetIndex]));
            FontBuilder.AddChar(tmp,CharSet[CharSetIndex]);
          finally
            tmp.Free;
          end;
          inc(CharSetIndex);
        end;
        fMode:=mScanForPixel;
      end else begin
        // Do nothing
      end;
    end;
  end;
//  pImage.writefile('temp.png','PNG');
  if fMode=mScanForEmpty then begin
    Log.LogDebug(Format('pre=%d, imageWidth=%d',[pre,pImage.Width]));
    if pMarker-pTop>0 then begin
      tmp:=TARGBImage.Create(pImage.Width-pre,pMarker-pTop-1);
      try
        pImage.Copy(pre,pTop,pImage.Width-pre,pMarker-pTop-1,tmp);
    //    tmp.WriteFile(inttostr(ord(Charset[CharsetIndex]))+'.png','PNG');
        if CharSetIndex>length(CharSet) then
          raise Exception.Create(Format('Not enough characters in charset! (%d)',[CharSetIndex]));
        FontBuilder.AddChar(tmp,CharSet[CharSetIndex]);
      finally
        tmp.Free;
      end;
    end;
    inc(CharSetIndex);
  end;
end;

procedure TMain.LoadImages;
var atm:TARGBImage;i,prevcharmarker:integer;emptycolor:uint32;
begin
  writeln('Loading and feeding image to FontBuilder...');
  atm:=TARGBImage.Create(InputFilename);
  try
    prevcharmarker:=-2;
    CharSetIndex:=1;
    emptycolor:=atm.GetPixel(0,0) and $ffffff;
    for i:=0 to atm.Height-1 do begin
      if atm.GetPixel(0,i) and $ffffff<>emptycolor then begin
        ProcessRow(atm,prevcharmarker+2,i,emptycolor);
        prevcharmarker:=i;
      end;
    end;
  finally
    atm.Free;
  end;
end;

{$R *.res}

begin
  with TMain.Create do try Run finally Free end;
end.

