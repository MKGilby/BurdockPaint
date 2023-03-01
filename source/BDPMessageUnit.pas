unit BDPMessageUnit;

{$mode Delphi}

interface

type
  TMessage=record
    TypeID:integer;
//    DataString:string;
    DataInt:integer;
  end;

  { TMessageQueue }

  TMessageQueue=class
    constructor Create(iQueueSize:integer);
    destructor Destroy; override;
    procedure AddMessage(pTypeID:integer;pDataString:string='';pDataInt:integer=-1); overload;
    procedure AddMessage(pMessage:TMessage); overload;
    function HasNewMessage:boolean;
    function GetNextMessage:TMessage;
  private
    fMessages:array of TMessage;
    fInPTR,fOutPTR:integer;
  end;

implementation

uses SysUtils;

{ TMessageQueue }

constructor TMessageQueue.Create(iQueueSize:integer);
begin
  SetLength(fMessages,iQueueSize);
  fInPTR:=0;
  fOutPTR:=0;
end;

destructor TMessageQueue.Destroy;
begin
  inherited Destroy;
end;

procedure TMessageQueue.AddMessage(pTypeID:integer; pDataString:string;
  pDataInt:integer);
begin
  if not((fInPTR=fOutPTR-1) or ((fInPtr=length(fMessages)-1) and (fOutPTR=0))) then begin
    fMessages[fInPTR].TypeID:=pTypeID;
//    fMessages[fInPTR].DataString:=pDataString;
    fMessages[fInPTR].DataInt:=pDataInt;
    inc(fInPTR);
    if fInPTR=length(fMessages) then fInPTR:=0;
  end else
    raise Exception.Create('Message queue overflow!');
end;

procedure TMessageQueue.AddMessage(pMessage:TMessage);
begin
  if not((fInPTR=fOutPTR-1) or ((fInPtr=length(fMessages)-1) and (fOutPTR=0))) then begin
    fMessages[fInPTR]:=pMessage;
    inc(fInPTR);
    if fInPTR=length(fMessages) then fInPTR:=0;
  end else
    raise Exception.Create('Message queue overflow!');
end;

function TMessageQueue.HasNewMessage:boolean;
begin
  Result:=fInPTR<>fOutPTR;
end;

function TMessageQueue.GetNextMessage:TMessage;
begin
  if (fInPTR<>fOutPTR) then begin
    Result:=fMessages[fOutPTR];
    inc(fOutPTR);
    if fOutPTR=length(fMessages) then fOutPTR:=0;
  end else
    raise Exception.Create('Message queue underflow!');
end;

end.

