program MenuCompiler;

{$mode delphi}

type

  { TSubMenu }

  TSubMenu=record
    _name:string;
    _messageid,_messageparam:integer;
    _hint:string;
    constructor Init(iName:String);
  end;

var
  t,incl:TextFile;
  s:string;
  bin:file;
  level:integer;
  submenupos:integer;
  menucount,submenucount:integer;
  submenu:TSubMenu;

  msgs:array of string;

const
  NextMessageID:integer=200;

function CountIndent(s:string):integer;
begin
  Result:=0;
  while (length(s)>Result) and (s[Result+1]=' ') do inc(Result);
end;

function AllTrim(s:string):string;
begin
  while (length(s)>0) and (s[1]=' ') do delete(s,1,1);
  while (length(s)>0) and (s[length(s)]=' ') do s:=copy(s,1,length(s)-1);
  Result:=s;
end;

procedure WriteString(s:string);
var b:byte;
begin
  s:=UpCase(s);  // Font contains only uppercase letters so...
  b:=length(s);
  blockwrite(bin,b,1);
  blockwrite(bin,s[1],b);
end;

procedure WriteInteger(i:integer);
begin
  blockwrite(bin,i,2);
end;

procedure WriteSubMenu;
begin
  if (submenu._name<>'') and (submenu._messageid>-1) then begin
    WriteString(submenu._name);
    WriteInteger(submenu._messageid);
    WriteInteger(submenu._messageparam);
    WriteString(submenu._hint);
    inc(submenucount);
  end;
end;

function GetMessageID(message:string):integer;
var i:integer;
begin
  if UpCase(message)='NONE' then begin  // Special, already defined in BDPSharedUnit.pas
    Result:=0;
    exit;
  end;
  if UpCase(message)='QUIT' then begin  // Special, already defined in BDPSharedUnit.pas
    Result:=1;
    exit;
  end;
  for i:=0 to length(msgs)-1 do
    if UpCase(message)=msgs[i] then begin
      Result:=NextMessageID+i;
      exit;
    end;
  Result:=NextMessageID+length(msgs);
  writeln(incl,'  MSG_',message,'=',Result,';');
  SetLength(msgs,length(msgs)+1);
  msgs[Length(msgs)-1]:=message;
end;

function GetNextMessageID(message:string):integer;
var s:string;i:integer;
begin
  if pos(',',message)>0 then begin
    s:=copy(message,pos(',',message)+1);
    message:=copy(message,1,pos(',',message)-1);
    val(s,submenu._messageparam,i);
  end;
  Result:=GetMessageID(message);
end;

{ TSubMenu }

constructor TSubMenu.Init(iName:String);
begin
  _name:=iName;
  _messageid:=-1;
  _messageparam:=0;
  _hint:='';
end;

begin
  assign(t,'mainmenu.txt');
  reset(t);
  assign(bin,'..\..\data\menu.bin');
  rewrite(bin,1);
  assign(incl,'..\..\source\MenuMessages.inc');
  rewrite(incl);
  writeln(incl,'// Menu message constants.');
  writeln(incl,'// Generated on ',{$I %DATE%},'.');
  writeln(incl);
  writeln(incl,'const');
  menucount:=0;
  submenucount:=0;
  blockwrite(bin,menucount,1);
  submenupos:=FileSize(bin);
  submenu:=TSubMenu.Init('');
  SetLength(msgs,0);
  while not eof(t) do begin
    readln(t,s);
    if (length(s)=0) then continue;
    if s[1]=';' then continue;
    level:=CountIndent(s);
    if (level=0) then begin
      if submenu._name<>'' then WriteSubMenu;
      submenu:=TSubMenu.Init('');
      if (submenucount>0) then begin
        seek(bin,submenupos);
        blockwrite(bin,submenucount,1);
        seek(bin,FileSize(bin));
      end;
      WriteString(AllTrim(s));
      submenucount:=0;
      submenupos:=FileSize(bin);
      blockwrite(bin,submenucount,1);
      inc(menucount);
    end else
    if (level=2) then begin
      if submenu._name<>'' then WriteSubMenu;
      submenu:=TSubMenu.Init(AllTrim(s));
    end else
    if (level=4) then begin
      if submenu._messageid=-1 then begin  // Message
        submenu._messageid:=GetNextMessageID(alltrim(s));
      end else
      if submenu._hint='' then begin  // Hint
        submenu._hint:=AllTrim(s);
      end else begin
        writeln('Too many level 4 lines!');
        writeln('The extra line was:');
        writeln(s);
        halt;
      end;
    end;
  end;
  if (submenucount>0) then begin
    seek(bin,submenupos);
    blockwrite(bin,submenucount,1);
  end;
  seek(bin,0);
  blockwrite(bin,menucount,1);
  closefile(incl);
  closefile(bin);
  closefile(t);
end.

