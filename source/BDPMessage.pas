{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szab� "Gilby" Zsolt / MKSZTSZ

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

unit BDPMessage;

{$mode Delphi}

interface

uses SysUtils;

type

  { TMessage }

  TMessage=record
    TypeID:integer;
    DataInt:integer;
    DataUInt32:uint32;
    constructor Init(iTypeID,iDataInt:integer;iDataUInt32:uint32);
  end;

  { TMessageQueue }

  TMessageQueue=class
    constructor Create(iQueueSize:integer);
    destructor Destroy; override;
    procedure AddMessage(pTypeID:integer;pDataInt:integer=-1;pDataUInt32:uint32=0); overload;
    procedure AddMessage(pMessage:TMessage); overload;
    function HasNewMessage:boolean;
    function GetNextMessage:TMessage;
    procedure LogMessages;
  private
    fMessages:array of TMessage;
    fInPTR,fOutPTR:integer;
  end;

implementation

uses Logger;

{ TMessage }

constructor TMessage.Init(iTypeID,iDataInt:integer; iDataUInt32:uint32);
begin
  TypeID:=iTypeID;
  DataInt:=iDataInt;
  DataUInt32:=iDataUInt32;
end;

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

procedure TMessageQueue.AddMessage(pTypeID:integer; pDataInt:integer;
  pDataUInt32:uint32);
begin
  if not((fInPTR=fOutPTR-1) or ((fInPtr=length(fMessages)-1) and (fOutPTR=0))) then begin
    fMessages[fInPTR]:=TMessage.Init(pTypeID,pDataInt,pDataUInt32);

    inc(fInPTR);
    if fInPTR=length(fMessages) then fInPTR:=0;
  end else begin
    LogMessages;
    raise Exception.Create('Message queue overflow!');
  end;
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

procedure TMessageQueue.LogMessages;
var i:integer;
begin
  i:=fOutPTR;
  while i<>fInPTR do with fMessages[i] do begin
    Log.LogDebug(Format('%d. TypeID: %d, DataInt: %d',[i,TypeID,DataInt]));
    inc(i);
    if i=length(fMessages) then i:=0;
  end;
end;

end.

