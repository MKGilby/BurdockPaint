{ -[Name]-------------------------------------------

              Coding and decoding routines

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2004-2009

  --------------------------------------------------

  -[Description]------------------------------------

   Data encoding methods to make them more compressable.
   
   Interleave: changes byte order
   
    Interleave by 2: (useful on 16 bit .wav files)
      Original:    0 1 2 3 4 5 6 7 8 9
      Interleaved: 0 2 4 6 8 1 3 5 7 9
     
    Interleave by 3: (useful on 24 bit images)
      Original:    0 1 2 3 4 5 6 7 8 9
      Interleaved: 0 3 6 9 1 4 7 2 5 8

   Delta: calculates the difference beetween bytes
      Original:    0 2 4 5 5 6 7 9 8 7
      Delta:       0 2 2 1 0 1 1 2 255 255
      
   MTF (Move to front): the bytes are changed according to
                        how long ago it was last used in the sequence.
                        (There's a list with all the bytes. The current byte
                         moves to the front, shifting the others one value up.)
      Original:    5 2 4 7 0 0 7 1 7 7
      MTF:         5 3 5 7 4 0 1 5 1 0

  --------------------------------------------------

}

// Version info:
//
//  V1.00: Gilby
//    * Initial creation
//    + Interleave and deinterleave added
//  V1.01: Gilby
//    + Delta and undelta added
//  V1.02: Gilby - 2009.09.13
//    + MoveToFront encoding and decoding added
//  V1.03: Gilby - 2009.12.30
//    + Stream seek fix in MTF routines
//  V1.04: Gilby - 2010.03.23
//    - Removed Result:=Result from Interleave
//  V1.05: Gilby - 2010.08.18
//    * Delta performance is significantly speed up. (20x)
//    * Optimized code for interleave2. (9x)
//    * Optimized code for interleave3. (7x)
//  V1.06: Gilby - 2010.10.28
//    * RealDelta and RealUndelta made delphi compatible
//  V1.07: Gilby - 2010.10.29
//    * BUGFIX: RealDelta and RealUndelta: TDeltaBuffer had to be var'ed.
//  V1.07a: Gilby - 2015.12.09
//    * Fixes to suppress hints in Lazarus
//  V1.08: Gilby - 2019.01.10
//    * Range check error fix (when in debug mode)

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit CodingUnit;

interface

uses Classes;

  function Interleave(iSource:TStream;iValue:integer):TStream;
  function DeInterleave(iSource:TStream;iValue:integer):TStream;
  function Delta(iSource:TStream):TStream;
  function UnDelta(iSource:TStream):TStream;
  function MTFEncode(iSource:TStream):TStream;
  function MTFDecode(iSource:TStream):TStream;

implementation

uses SysUtils, Logger;

const Fstr='CodingUnit.pas, ';
      Version='1.08';

type TDeltaBuffer=array[0..4095] of byte;

function Interleave2B(iSource:TStream):TStream;
var buf:array[0..15] of byte;b:byte;len,tpos1,tpos2:integer;
begin
  Result:=TMemoryStream.Create;
  (Result as TMemoryStream).SetSize(iSource.Size);
  tpos1:=0;
  tpos2:=(iSource.Size div 2)+iSource.Size mod 2;
  buf[0]:=0;
  while iSource.Position<iSource.Size do begin
    len:=iSource.Size-iSource.Position;
    if len>16 then len:=16;
    iSource.Read(buf,len);
    b:=buf[1];buf[1]:=buf[2];buf[2]:=buf[4];buf[4]:=buf[8];buf[8]:=b;
    b:=buf[3];buf[3]:=buf[6];buf[6]:=buf[12];buf[12]:=buf[9];buf[9]:=b;
    b:=buf[5];buf[5]:=buf[10];buf[10]:=b;
    b:=buf[7];buf[7]:=buf[14];buf[14]:=buf[13];buf[13]:=buf[11];buf[11]:=b;
    if len=16 then begin
      Result.Seek(tpos1,soFromBeginning);
      Result.Write(buf[0],8);
      inc(tpos1,8);
      Result.Seek(tpos2,soFromBeginning);
      Result.Write(buf[8],8);
      inc(tpos2,8);
    end else begin
      Result.Seek(tpos1,soFromBeginning);
      if len mod 2=1 then 
        Result.Write(buf[0],len div 2+1)
      else 
        Result.Write(buf[0],len div 2);
      Result.Seek(tpos2,soFromBeginning);
      Result.Write(buf[8],len div 2);
    end;
  end;
end;

function Interleave3(iSource:TStream):TStream;
var buf:array[0..11] of byte;b:byte;len,tpos1,tpos2,tpos3:integer;
begin
  Result:=TMemoryStream.Create;
  (Result as TMemoryStream).SetSize(iSource.Size);
  tpos1:=0;
  tpos2:=(iSource.Size div 3);
  tpos3:=(iSource.Size div 3)*2;
  if iSource.Size mod 3>0 then begin inc(tpos2);inc(tpos3);end;
  if iSource.Size mod 3>1 then inc(tpos3);
  buf[0]:=0;
  while iSource.Position<iSource.Size do begin
    len:=iSource.Size-iSource.Position;
    if len>12 then len:=12;
    iSource.Read(buf,len);
    b:=buf[1];buf[1]:=buf[3];buf[3]:=buf[9];buf[9]:=buf[5];buf[5]:=buf[4];buf[4]:=b;
    b:=buf[2];buf[2]:=buf[6];buf[6]:=buf[7];buf[7]:=buf[10];buf[10]:=buf[8];buf[8]:=b;
    if len=12 then begin
      Result.Seek(tpos1,soFromBeginning);
      Result.Write(buf[0],4);
      inc(tpos1,4);
      Result.Seek(tpos2,soFromBeginning);
      Result.Write(buf[4],4);
      inc(tpos2,4);
      Result.Seek(tpos3,soFromBeginning);
      Result.Write(buf[8],4);
      inc(tpos3,4);
    end else begin
      Result.Seek(tpos1,soFromBeginning);
      if len mod 3>0 then
        Result.Write(buf[0],len div 3+1)
      else
        Result.Write(buf[0],len div 3);

      Result.Seek(tpos2,soFromBeginning);
      if len mod 3>1 then
        Result.Write(buf[4],len div 3+1)
      else
        Result.Write(buf[4],len div 3);
        
      Result.Seek(tpos3,soFromBeginning);
      Result.Write(buf[8],len div 3);
    end;
  end;
end;

function Interleave(iSource:TStream;iValue:integer):TStream;
var b:byte;
    CurrentShift:integer;
    CurrentPosition:integer;
    i:integer;
begin
  case iValue of
    2:Result:=Interleave2B(iSource);
    3:Result:=Interleave3(iSource);
  else begin
      b:=0;
      CurrentShift:=0;
      CurrentPosition:=0;
      Result:=TMemoryStream.Create;
      for i:=0 to iSource.Size-1 do begin
        iSource.Seek(CurrentPosition,soFromBeginning);
        iSource.Read(b,1);
        Result.Write(b,1);
        CurrentPosition:=CurrentPosition+iValue;
        if CurrentPosition>=iSource.Size then begin
          inc(CurrentShift);
          CurrentPosition:=CurrentShift;
        end;
      end;
    end;
  end;
  Result.Seek(0,soFromBeginning);
end;

function DeInterleave(iSource:TStream;iValue:integer):TStream;
var b:Byte;
    CurrentShift:integer;
    CurrentPosition:integer;
    i:integer;
begin
  CurrentShift:=0;
  CurrentPosition:=0;
  Result:=TMemoryStream.Create;
  TMemoryStream(Result).SetSize(iSource.Size);
  b:=0;
  for i:=0 to iSource.Size-1 do begin
    iSource.Read(b,1);
    Result.Seek(CurrentPosition,soFromBeginning);
    Result.Write(b,1);
    CurrentPosition:=CurrentPosition+iValue;
    if CurrentPosition>=Result.Size then begin
      inc(CurrentShift);
      CurrentPosition:=CurrentShift;
    end;
  end;
  Result.Seek(0,soFromBeginning);
end;

procedure RealDelta(var buf:TDeltaBuffer;size:integer;var pre:byte);
var b:byte;i:integer;
begin
  for i:=0 to size-1 do begin
    b:=buf[i]-pre;
    pre:=buf[i];
    buf[i]:=b;
  end;
end;

procedure RealUnDelta(var buf:TDeltaBuffer;size:integer;var pre:byte);
var i:integer;
begin
  for i:=0 to size-1 do begin
    pre:=(pre+buf[i]) and $ff;
    buf[i]:=pre;
  end;
end;

function DeltaEx(iSource:TStream;un:boolean=false):TStream;
var pre,b:byte;len:integer;
    buffer:TDeltaBuffer;
begin
  Result:=TMemoryStream.Create;
  iSource.Seek(0,soFromBeginning);
  b:=0;
  iSource.Read(b,1);
  Result.Write(b,1);
  pre:=b;
  buffer[0]:=0;
  while iSource.Position<iSource.Size do begin
    if iSource.Position>=iSource.Size-4095 then
      len:=iSource.Size-iSource.Position
    else
      len:=4096;
    iSource.Read(Buffer[0],len);
    if un then RealUnDelta(Buffer,len,pre)
          else RealDelta(Buffer,len,pre);
    Result.Write(Buffer[0],len);
  end;
  Result.Seek(0,soFromBeginning);
end;

function Delta(iSource:TStream):TStream;
begin
  Result:=DeltaEx(iSource);
end;

function UnDelta(iSource:TStream):TStream;
begin
  Result:=DeltaEx(iSource,true);
end;

function MTFEncode(iSource:TStream):TStream;
var i,j:longint;b:byte;
var values:array[0..255] of byte;
begin
  iSource.Seek(0,soFromBeginning);
  Result:=TMemoryStream.Create;
  for i:=0 to 255 do Values[i]:=i;
  b:=0;
  for i:=0 to iSource.Size-1 do begin
    iSource.Read(b,1);
    for j:=0 to 255 do
      if values[j]=b then break;
    Result.Write(j,1);
    if j>0 then begin
      move(values[0],values[1],j);
      values[0]:=b;
    end;
  end;
  Result.Seek(0,soFromBeginning);
end;

function MTFDecode(iSource:TStream):TStream;
var i,j:longint;b:byte;
var values:array[0..255] of byte;
begin
  iSource.Seek(0,soFromBeginning);
  Result:=TMemoryStream.Create;
  for i:=0 to 255 do Values[i]:=i;
  b:=0;
  for i:=0 to iSource.Size-1 do begin
    iSource.Read(b,1);
    j:=values[b];
    Result.Write(j,1);
    if b>0 then begin
      move(values[0],values[1],b);
      values[0]:=j;
    end;
  end;
  Result.Seek(0,soFromBeginning);
end;

initialization
begin
  Log.LogStatus(Fstr+'version '+Version,'uses');
end;

end.
