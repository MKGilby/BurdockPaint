{ -[Name]-------------------------------------------

              Binary Search Algorythm V1.01

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2006

  --------------------------------------------------

  -[Description]------------------------------------

   Binary search algorythm with callback to search
   a sorted array of data.

   Usage: You must provide a function to determine
   the relation of two elements (greater,equal,less).
   Result is the index of the searched element, or -1
   if not found.

  --------------------------------------------------

  -[Requirements]-----------------------------------

   Nope...

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby
//     - Initial creation
//  V1.01: Gilby - 2007.02.08
//     - Bugfix: it works correctly now...
//  V1.02: Gilby - 2023.05.31
//     - Callback procedure parameter changed to pointer.
//     - Callback procedure result is changed to widely used values.
//     - Procedures are rewritten based on an article on John's blog
//       (https://nachtimwald.com/2018/01/18/binary-search-and-insert/)

{$mode delphi}

unit BinarySearchUnit;

interface

type TSearchCompareProcO=function(index:integer;item:pointer):integer of object;
type TSearchCompareProc=function(index:integer;item:pointer):integer;
     // Compares item with list[index]
     // Result=-1 item < list[index]
     //         0 item = list[index]
     //         1 item > list[index]

function BinarySearch(Left,Right:integer;
                      CompareProc:TSearchCompareProc;
                      Item:pointer):integer; overload;

function BinarySearch(Left,Right:integer;
                      CompareProc:TSearchCompareProcO;
                      Item:pointer):integer; overload;

function BinaryInsert(Left,Right:integer;
                      CompareProc:TSearchCompareProcO;
                      Item:pointer):integer; overload;

implementation

uses Logger;

const 
  Fstr='BinarySearchUnit.pas, ';
  Version='1.02';

function BinarySearch(Left,Right:integer; CompareProc:TSearchCompareProc;
  Item:pointer):integer;
var mid,eq:integer;
begin
  eq:=-1;
  while (Left<Right) do begin
    mid:=(Left+Right) div 2;
    eq:=CompareProc(mid,Item);
    if (eq<0) then Right:=mid
    else if (eq>0) then Left:=mid+1
    else break;
  end;
  if eq=0 then begin
    while (mid >= left) do begin
      if CompareProc(mid,Item)<>0 then break;
      dec(mid);
    end;
    Result:=mid;
  end else Result:=-1;
end;

function BinarySearch(Left,Right:integer;CompareProc:TSearchCompareProcO;
  Item:pointer):integer;
var mid,eq:integer;
begin
  eq:=-1;
  while (Left<Right) do begin
    mid:=(Left+Right) div 2;
    eq:=CompareProc(mid,Item);
    if (eq<0) then Right:=mid
    else if (eq>0) then Left:=mid+1
    else break;
  end;
  if eq=0 then begin
    while (mid >= left) do begin
      if CompareProc(mid,Item)<>0 then break;
      dec(mid);
    end;
    Result:=mid;
  end else Result:=-1;
end;

function BinaryInsert(Left,Right:integer; CompareProc:TSearchCompareProcO;
  Item:pointer):integer;
var mid,eq:integer;
begin
  eq:=0;
  while (Left<Right) do begin
    mid:=(Left+Right) div 2;
    eq:=CompareProc(mid,Item);
    if (eq<0) then Right:=mid
    else if (eq>0) then Left:=mid+1
    else break;
  end;
  if (eq>0) then inc(mid);
  while (mid<Right) and (eq=0) do begin
    inc(mid);
    eq:=CompareProc(mid,Item);
  end;
  Result:=mid;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
