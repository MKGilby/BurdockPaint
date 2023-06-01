{ -[Name]--------------------------------------------------------------

                              LogTicks Unit

  -[Disclaimer]--------------------------------------------------------

  Written by Gilby
  Copyright 2011-2023 MKSZTSZ

  This unit is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  This unit is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this unit. If not, see <https://www.gnu.org/licenses/>.

  -[Description]-------------------------------------------------------

  Enables measuring processing times and logging it.

  ---------------------------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby
//     - Initial creation
//  V1.01: Gilby - 2011.12.08
//     - Ticks property added
//  V1.02: Gilby - 2013.02.01
//     * Destructor is now Destroy
//  V1.03: Gilby - 2013.04.09
//     + Added 16 registers to store times. AddTimeTo adds the ticks from the last
//       point to the selected register. ListRegisters lists them to the log.
//  V1.04: Gilby - 2014.03.25
//     * Register listing contains more info
//  V1.05: Gilby - 2018.12.20
//     * Register listing crashed when no measuring was made
//     * Register listing caused RunError 215 sometimes
//  V1.06: Gilby - 2020.11.11
//     * Registers can be named
//  V1.07: Gilby - 2023.05.30
//     * SDL_GetTicks replaced with GetTickCount64

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit LogTicksUnit;

interface

uses Logger;

type 
  TLogTicks=class(TLogger)
    constructor Create;
    destructor Destroy; override;
    procedure StartMeasuring;
    procedure EndMeasuring(iMessage:string);
    procedure AddTimeTo(iRegister:integer);
    procedure SetRegisterName(pRegister:integer;pName:string);
    procedure ListRegisters;
    procedure LogTicks(iMessage:string);
  private
    fPrevious:QWord;
    fRegisters:array[0..15] of integer;
    fRegisterNames:array[0..15] of string;
    function fGetTicks:QWord;
  public
    property Ticks:QWord read fGetTicks;  
  end;
    
var 
  LogTicks:TLogTicks;    
     
implementation

uses SysUtils;

const Fstr='LogTicksUnit.pas, ';
      Version='1.07';

constructor TLogTicks.Create;
var i:integer;
begin
  inherited Create('ticks.log');
  for i:=0 to 15 do
    fRegisterNames[i]:=Format('Reg[%.2d]',[i]);
end;

destructor TLogTicks.Destroy;
begin
  inherited ;
end;

procedure TLogTicks.StartMeasuring;
var i:integer;
begin
  for i:=0 to 15 do fRegisters[i]:=0;
  fPrevious:=GetTickCount64;
end;

procedure TLogTicks.EndMeasuring(iMessage:string);
var i:integer;
begin
  i:=GetTickCount64;
  Log.LogStatus(inttostr(i-fPrevious)+' ticks ('+iMessage+')');
end;

procedure TLogTicks.LogTicks(iMessage:string);
var i:cardinal;
begin
  i:=GetTickCount64;
  Log.LogStatus(inttostr(i-fPrevious)+' ticks ('+iMessage+')');
  fPrevious:=i;
end; 

function TLogTicks.fGetTicks:QWord;
begin
  Result:=GetTickCount64;
end;

procedure TLogTicks.AddTimeTo(iRegister:integer);
var i:integer;
begin
  if (iRegister>=0) and (iRegister<16) then begin
    i:=GetTickCount64;
    fRegisters[iRegister]+=i-fPrevious;
    fPrevious:=i;
  end;
end;

procedure TLogTicks.SetRegisterName(pRegister:integer;pName:string);
begin
  if (pRegister>=0) and (pRegister<16) then fRegisterNames[pRegister]:=pName;
end;

procedure TLogTicks.ListRegisters;
var i,j:integer;
begin
  Log.LogStatus('Listing LogTicks registers:');
  j:=0;
  for i:=0 to 15 do j+=fRegisters[i];
  if j>0 then begin
    for i:=0 to 15 do if fRegisters[i]<>0 then
      Log.LogStatus(Format('%.2d. %s=%d (%d%%)',[i,fRegisterNames[i],fRegisters[i],round(fRegisters[i]/j*100)]));
  end else begin
    Log.LogStatus('*** No measuring was made!');
  end;
  Log.LogStatus('Listing Logticks registers ended.');
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  LogTicks:=TLogTicks.Create;

finalization
  LogTicks.Free;

end.
