{ -[Name]-------------------------------------------

                MKSZTSZ Stack object

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2006-2015

  --------------------------------------------------

  -[Description]------------------------------------

   You can PUSH and POP values to stack using 
   LIFO processing.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby
//     * Initial creation
//  V1.01: 2011.06.22 - Gilby
//     + Size property added.
//     + More than 1 byte operations rewritten
//  V1.02: 2015.11.05 - Gilby
//     * Fixes to supress hints in Lazarus

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit StackUnit;

interface

type 
  TStack=class
    constructor Create;
    destructor Destroy; override;
    procedure Push1(value:byte);
    function Pop1:byte;
    procedure Push2(value:smallint);
    function Pop2:smallint;
    procedure Push4(value:integer);
    function Pop4:integer;
  private
    fStack:array of byte;
    function GetSize:integer;
  public
    property Size:integer read GetSize;  
  end;

implementation
  
uses Logger;

const Fstr='StackUnit.pas, ';
      Version='1.02';  
     
constructor TStack.Create;
begin
  SetLength(fStack,0);
end;

destructor TStack.Destroy;
begin
  SetLength(fStack,0);
  inherited ;
end;

procedure TStack.Push1(value:byte);
begin
  SetLength(fStack,length(fStack)+1);
  fStack[length(fStack)-1]:=value;
end;

function TStack.Pop1:byte;
const Istr=Fstr+'TStack.Pop2';
begin
  if length(fStack)>0 then begin
    Result:=fStack[length(fStack)-1];
    SetLength(fStack,length(fStack)-1);
  end else begin
    Log.LogWarning('Stack underflow!',Istr);
    Result:=0;
  end;
end;

procedure TStack.Push2(value:Smallint);
begin
  SetLength(fStack,length(fStack)+2);
  move(value,fStack[length(fStack)-2],2);
end;

function TStack.Pop2:SmallInt;
const Istr=Fstr+'TStack.Pop2';
begin
  Result:=0;
  if length(fStack)>1 then begin
    move(fStack[length(fStack)-2],Result,2);
    SetLength(fStack,length(fStack)-2);
  end else begin
    Log.LogWarning('Stack underflow!',Istr);
  end;
end;

procedure TStack.Push4(value:integer);
begin
  SetLength(fStack,length(fStack)+4);
  move(value,fStack[length(fStack)-4],4);
end;

function TStack.Pop4:integer;
const Istr=Fstr+'TStack.Pop4';
begin
  Result:=0;
  if length(fStack)>3 then begin
    move(fStack[length(fStack)-4],Result,4);
    SetLength(fStack,length(fStack)-4);
  end else begin
    Log.LogWarning('Stack underflow!',Istr);
  end;
end;

function TStack.GetSize:integer;
begin
  Result:=Length(fStack);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
