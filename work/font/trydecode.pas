{$mode delphi}

uses Classes, SysUtils;

var Xs,Ys:TStream;
    a:array [0..5] of byte;
    b:array [0..7] of byte;
    j:byte;

begin
  Xs:=TFileStream.Create('system.fnt',fmOpenRead);
  Xs.Seek($25A,soFromBeginning);
  Ys:=TFileStream.Create('system2.fnt',fmCreate);
  
  while Xs.Size-Xs.Position>6 do begin
    Xs.read(a[0],6);
    j:=a[0];a[0]:=a[1];a[1]:=j;
    j:=a[2];a[2]:=a[3];a[3]:=j;
    j:=a[4];a[4]:=a[5];a[5]:=j;
    b[0]:=a[0]>>2;
    b[1]:=(a[0] and $03)<<4+a[1]>>4;
    b[2]:=(a[1] and $0f)<<2+a[2]>>6;
    b[3]:=a[2] and $3f;
    b[4]:=a[3]>>2;
    b[5]:=(a[3] and $03)<<4+a[4]>>4;
    b[6]:=(a[4] and $0f)<<2+a[5]>>6;
    b[7]:=a[5] and $3f;
    Ys.Write(b[0],8);
  end;
  
  Ys.Free;
  Xs.Free;
end.
