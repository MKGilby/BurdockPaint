{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of BurdockPaint.

  BurdockPaint is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  BurdockPaint is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  BurdockPaint. If not, see <https://www.gnu.org/licenses/>.
}

program BDPMap;

uses Classes, SysUtils, MyZStreamUnit;

type

  { TMain }

  TMain=class
    constructor Create(iFilename:string);
    destructor Destroy; override;
    procedure Run;
  private
    fStream:TStream;
    fIndent:integer;
    procedure ProcessBlock;
    procedure ProcessP;
    procedure ProcessI;
    procedure ProcessE;
    procedure ProcessC;
    procedure ProcessR;
    procedure ProcessU;
    procedure ProcessV;
    procedure ProcessL;
    procedure ProcessO;
    procedure ProcessQ;
    procedure ProcessS;
    procedure ProcessT;
    procedure W(s:string);
  end;

var
  Main:TMain;

{ TMain }

constructor TMain.Create(iFilename:string);
begin
  if FileExists(iFilename) then
    fStream:=TFileStream.Create(iFilename,fmOpenRead or fmShareDenyNone)
  else
    fStream:=nil;
  fIndent:=0;
end;

destructor TMain.Destroy;
begin
  if Assigned(fStream) then FreeAndNil(fStream);
  inherited Destroy;
end;

procedure TMain.Run;
begin
  ProcessBlock;
end;

procedure TMain.ProcessBlock;
var c:char;
begin
  c:=#0;
  fStream.Read(c,1);
  case c of
    'P':ProcessP;
    'I':ProcessI;
    'E':ProcessE;
    'C':ProcessC;
    'R':ProcessR;
    'U':ProcessU;
    'V':ProcessV;
    'L':ProcessL;
    'O':ProcessO;
    'Q':ProcessQ;
    'S':ProcessS;
    'T':ProcessT;
  end;
end;

procedure TMain.ProcessP;
var len:uint32;ver,flags:Byte;imagecount,activeimageindex:integer;
begin
  len:=0;ver:=0;flags:=0;
  fStream.Read(len,4);
  fStream.Read(ver,1);

  W(Format('P version %d (Start: %d, Len: %d)',[ver,fStream.Position-6,len]));
  fStream.Read(flags,1);
  W(Format('  Flags: %d',[flags]));
  if flags and 1<>0 then writeln('    Contains CEL image.') else writeln('    Does not contains CEL image.');
  imagecount:=0;
  fStream.Read(imagecount,2);
  activeimageindex:=0;
  fStream.Read(activeimageindex,2);
  W(Format('  ImageCount: %d (Current: %d)',[imagecount,activeimageindex]));
  inc(fIndent,2);
  while imagecount>0 do begin
    ProcessBlock;
    dec(imagecount);
  end;
  if flags and 1<>0 then ProcessBlock;
  dec(fIndent,2);
end;

procedure TMain.ProcessI;
var len:uint32;ver:byte;
begin
  len:=0;ver:=0;
  fStream.Read(len,4);
  fStream.Read(ver,1);
  W(Format('I version %d (Start: %d, Len: %d)',[ver,fStream.Position-5,len]));
  inc(fIndent,2);
  if ver=1 then begin
    ProcessBlock;
    ProcessBlock;
  end
  else if ver=2 then begin
    ProcessBlock;
  end;
  dec(fIndent,2);
end;

procedure TMain.ProcessE;
var len:uint32;ver,flags:Byte;s:string;
begin
  len:=0;ver:=0;flags:=0;
  fStream.Read(len,4);
  fStream.Read(ver,1);
  W(Format('E version %d (Start: %d, Len: %d)',[ver,fStream.Position-6,len]));
  fStream.Read(flags,1);
  W(Format('  Flags: %d',[flags]));
  s:='    [';
  if flags and 2<>0 then s+='X' else s+=' ';
  s+='] ImageUndoData, [';
  if flags and 4<>0 then s+='X' else s+=' ';
  s+='] PaletteUndoData, [';
  if flags and 8<>0 then s+='X' else s+=' ';
  s+='] ColorClusters';
  W(s);
  inc(fIndent,2);
  ProcessBlock;
  ProcessBlock;
  if flags and 2<>0 then ProcessBlock;
  if flags and 4<>0 then ProcessBlock;
  if flags and 8<>0 then ProcessBlock;
  dec(fIndent,2);
end;

procedure TMain.ProcessC;
var pos,len:uint32;ver:byte;Xs:TStream;cnt:integer;
begin
  len:=0;
  fStream.Read(len,4);
  pos:=fStream.Position;
  Xs:=TMemoryStream.Create;
  UnCompressStream(fStream,Xs);
  Xs.Position:=0;
  ver:=0;cnt:=0;
  Xs.Read(ver,1);
  Xs.Read(cnt,2);
  W(Format('C version %d (Start: %d, Len: %d)',[ver,pos-5,len]));
  W(Format('  Color count: %d',[cnt]));
  Xs.Free;
  fStream.Position:=pos+len;
end;

procedure TMain.ProcessR;
var pos,len:uint32;ver:byte;left,top,width,height:integer;Xs:TStream;
begin
  len:=0;
  fStream.Read(len,4);
  pos:=fStream.Position;
  Xs:=TMemoryStream.Create;
  UnCompressStream(fStream,Xs);
  Xs.Position:=0;
  ver:=0;left:=0;top:=0;width:=0;height:=0;
  Xs.Read(ver,1);
  Xs.Read(left,2);
  Xs.Read(top,2);
  Xs.Read(width,2);
  Xs.Read(height,2);
  W(Format('R version %d (Start: %d, Len: %d)',[ver,pos-5,len]));
  W(Format('  Left: %d, Top: %d, Width: %d, Height: %d',[left,top,width,height]));
  Xs.Free;
  fStream.Position:=pos+len;
end;

procedure TMain.ProcessU;
var len:uint32;ver:Byte;opcount,current:integer;
begin
  len:=0;ver:=0;
  fStream.Read(len,4);
  fStream.Read(ver,1);
  W(Format('U version %d (Start: %d, Len: %d)',[ver,fStream.Position-5,len]));
  opcount:=0;current:=0;
  fStream.Read(opcount,2);
  fStream.Read(current,2);
  W(Format('  ItemCount: %d (Current: %d)',[opcount,current]));
  inc(fIndent,2);
  while opcount>0 do begin
    ProcessBlock;
    dec(opcount);
  end;
  dec(fIndent,2);
end;

procedure TMain.ProcessV;
var len:uint32;ver:Byte;opcount,current:integer;
begin
  len:=0;ver:=0;
  fStream.Read(len,4);
  fStream.Read(ver,1);
  W(Format('V version %d (Start: %d, Len: %d)',[ver,fStream.Position-5,len]));
  opcount:=0;current:=0;
  fStream.Read(opcount,2);
  fStream.Read(current,2);
  W(Format('  ItemCount: %d (Current: %d)',[opcount,current]));
  inc(fIndent,2);
  while opcount>0 do begin
    ProcessBlock;
    dec(opcount);
  end;
  dec(fIndent,2);
end;

procedure TMain.ProcessL;
var len:uint32;ver:Byte;count,active:integer;
begin
  len:=0;ver:=0;
  fStream.Read(len,4);
  fStream.Read(ver,1);
  W(Format('L version %d (Start: %d, Len: %d)',[ver,fStream.Position-6,len]));
  count:=0;
  fStream.Read(count,1);
  W(Format('  ItemCount: %d',[count]));
  if ver=2 then begin
    active:=0;
    fStream.Read(active,1);
    W(Format('  Active item index: %d',[active]));
  end;
  inc(fIndent,2);
  while count>0 do begin
    ProcessBlock;
    dec(count);
  end;
  dec(fIndent,2);
end;

procedure TMain.ProcessO;
var len:uint32;ver:byte;
begin
  len:=0;ver:=0;
  fStream.Read(len,4);
  fStream.Read(ver,1);
  W(Format('O version %d (Start: %d, Len: %d)',[ver,fStream.Position-5,len]));
  inc(fIndent,2);
  ProcessBlock;
  ProcessBlock;
  dec(fIndent,2);
end;

procedure TMain.ProcessQ;
var len:uint32;ver:byte;firstcolor:integer;
begin
  len:=0;ver:=0;
  fStream.Read(len,4);
  fStream.Read(ver,1);
  W(Format('Q version %d (Start: %d, Len: %d)',[ver,fStream.Position-5,len]));
  firstcolor:=0;
  fStream.Read(firstcolor,2);
  W(Format('  First affected color: %d',[firstcolor]));
  inc(fIndent,2);
  ProcessBlock;
  ProcessBlock;
  dec(fIndent,2);
end;

procedure TMain.ProcessS;
var len:uint32;ver:byte;color:integer;
begin
  len:=0;ver:=0;
  fStream.Read(len,4);
  fStream.Read(ver,1);
  W(Format('S version %d (Start: %d, Len: %d)',[ver,fStream.Position-5,len]));
  color:=0;
  fStream.Read(color,2);
  W(Format('  Affected color: %d',[color]));
  inc(fIndent,2);
  fStream.Read(len,4);
  W('  Value before: '+hexstr(len,8));
  fStream.Read(len,4);
  W('  Value after : '+hexstr(len,8));
  dec(fIndent,2);
end;

procedure TMain.ProcessT;
var len:uint32;ver,flags:Byte;startc,endc:integer;s:string;
  c1,c2:uint32;
begin
  len:=0;ver:=0;
  fStream.Read(len,4);
  fStream.Read(ver,1);
  W(Format('T version %d (Start: %d, Len: %d)',[ver,fStream.Position-6,len]));
  flags:=0;
  if ver=1 then begin
    startc:=0;endc:=0;
    fStream.Read(startc,2);
    fStream.Read(endc,2);
    fStream.Read(flags,1);
    W(Format('  Start: %d, End: %d',[startc,endc]));
  end
  else if ver=2 then begin
    c1:=0;c2:=0;
    fStream.Read(c1,4);
    fStream.Read(c2,4);
    fStream.Read(flags,1);
    W(Format('  Color1: %s, Color2: %s',[hexstr(c1,8),hexstr(c2,8)]));
  end;
  W(Format('  Flags: %d',[flags]));
  s:='    [';
  if flags and 1<>0 then s+='X' else s+=' ';
  s+='] Reversed, [';
  if flags and 2<>0 then s+='X' else s+=' ';
  s+='] Pingpong';
  W(s);
end;

procedure TMain.W(s:string);
const spc='                                             ';
begin
  if fIndent>0 then
    write(copy(spc,1,findent));
  writeln(s);
end;

begin
  Main:=TMain.Create(paramstr(1));
  try
    Main.Run;
  finally
    Main.Free;
  end;
end.

