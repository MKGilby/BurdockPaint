{$mode delphi}

uses Classes, SysUtils, ARGBImageUnit, ARGBImageTGAReaderUnit, MKStream;

const Chars='ABCDEFGHIJKLMNOPQRSTUVWXYZ013456789,.-?:_;>*<}{@&#][|\'#39'"+!%/=()$2'#128#129#130#131' ';

var c2:string;

procedure PutCharData(Target:TStream;c:integer;rp:TARGBImage);
var p:pointer;y,x:integer;b:byte;
begin
  p:=rp.RawData+c*6*4;
  for y:=0 to 5 do begin
    b:=0;
    for x:=0 to 4 do begin
      b:=b<<1;
      if byte(p^)<>0 then b+=1;
      inc(p,4);
    end;
    Target.Write(b,1);
    p:=p+rp.Width*4-5*4;
  end;
end;

procedure Run;
var
  rp:TARGBImage;
  chrs:array[0..31] of byte;
  data,trg:TStream;
  i:integer;
  s:string;
begin
  rp:=TARGBImage.Create;
  rp.ReadFile('systemfnt.tga');
  fillchar(chrs[0],32,0);
  data:=TMemoryStream.Create;
  c2:='';
  for i:=0 to 255 do if pos(chr(i),Chars)>0 then begin
    c2+=chr(i);
    chrs[i div 8]:=chrs[i div 8] or (128>>(i mod 8));
    PutCharData(data,pos(chr(i),Chars)-1,rp);
  end;
  writeln(c2);
  trg:=TFileStream.Create('system.mkr',fmCreate);
  s:='MKAR'#2#5#6;
  trg.Write(s[1],length(s));
  trg.Write(chrs[0],32);
  data.Seek(0,soFromBeginning);
  trg.CopyFrom(data,data.size);
  FreeAndNil(trg);
  FreeAndNil(data);
  FreeAndNil(rp);
end;

begin
  MKStreamOpener.AddDirectory('.',0);
  Run;
end.
