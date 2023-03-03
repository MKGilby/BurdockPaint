{ -[Name]-------------------------------------------

   MKSZTSZ Visual Component Collection for SDL2

                         Visual Component Container

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2023

  --------------------------------------------------

  -[Description]------------------------------------

   It is a container that can hold one or more child
   visual components. It can be invisible, but you
   can override Draw to make a visual appearance.
   (Put inherited Draw at the end to draw child objects)
   It covers the area behind it, so any mouseobjects
   behind this object won't get any MouseDown or Up events.

   You can set it up
     - from an .INI file (you can assign childs only from code yet),
     - or from code, assigning values to properties.

   A valid INI section:

   [Container]
   Left=280
   Top=228
   Width=80
   Height=24

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2023.01.17
//    * Initial creation from vcc_Container.
//  V1.01: Gilby - 2023.01.25
//    * Handling MOUSEWHEEL events too.
//  V1.02: Gilby - 2023.02.13
//    * Improved logging.
//    * Removed commented out parts.
//  V1.03: Gilby - 2023.03.03
//    * Added OnShow and OnHide events.

{$mode delphi}{$H+}

unit vcc_Container2;

interface

uses Classes, MKMouse2, MKINIFile, Lists, SDL2;

type
  TMouseObjectList=TNamedList<TMouseObject>;

  TContainer=class(TMouseObject)
    constructor Create; overload;
    constructor Create(iINI:TINIFile;iSection:string); overload;
    destructor Destroy; override;
    procedure Draw; override;
    procedure AddChild(pObject:TMouseObject);
    procedure DeleteChild(pObject:TMouseObject);  overload;
    procedure DeleteChild(pName:String);  overload;
    procedure ClearChilds;
    function HandleEvent(Event:PSDL_Event):boolean; override;
  private
    fChilds:TMouseObjectList;
    procedure fSetVisible(pValue:boolean); override;
  public
    property Left:integer read fLeft write fLeft;
    property Top:integer read fTop write fTop;
    property Width:integer read fWidth write fWidth;
    property Height:integer read fHeight write fHeight;
    property Visible:boolean read fVisible write fSetVisible;
  end;
     
implementation

uses SysUtils, Logger;
     
const
  Fstr='vcc_Container2.pas, ';
  Version='1.01';

constructor TContainer.Create;
begin
  inherited Create;
  fChilds:=TMouseObjectList.Create;
  fLeft:=0;
  fTop:=0;
  fWidth:=64;
  fHeight:=24;
  fClicked:=false;
  fVisible:=true;
end;

constructor TContainer.Create(iINI:TINIFile;iSection:string); overload;
begin
  inherited Create;
  fChilds:=TMouseObjectList.Create;
  Left:=iINI.ReadInteger(iSection,'Left',0);
  Top:=iINI.ReadInteger(iSection,'Top',0);
  Width:=iINI.ReadInteger(iSection,'Width',0);;
  Height:=iINI.ReadInteger(iSection,'Height',0);
  fClicked:=false;
end;

destructor TContainer.Destroy;
begin
  FreeAndNil(fChilds);
  inherited Destroy;
end;

procedure TContainer.Draw;
var i:integer;
begin
  if fVisible then
    for i:=fChilds.Count-1 downto 0 do fChilds[i].Draw;
end;

procedure TContainer.AddChild(pObject:TMouseObject);
begin
  fChilds.AddObject(pObject.Name,pObject);
  pObject.Visible:=fVisible;
end;

procedure TContainer.DeleteChild(pObject:TMouseObject);
var i:integer;
begin
  i:=fChilds.IndexOfObject(pObject);
  if i>0 then fChilds.Delete(i);
end;

procedure TContainer.DeleteChild(pName:String);
var i:integer;
begin
  i:=fChilds.IndexOf(pName);
  if i>0 then fChilds.Delete(i);
end;

procedure TContainer.ClearChilds;
begin
  fChilds.Clear;
end;

procedure TContainer.fSetVisible(pValue:boolean);
var i:integer;
begin
  inherited ;
  for i:=0 to fChilds.Count-1 do
    fChilds[i].Visible:=pValue;

  fVisible:=pValue;
end;

function TContainer.HandleEvent(Event:PSDL_Event):boolean;
var nx,ny,i:integer;
begin
//  Log.LogDebug(Format('Container "%s" got event.',[Name]));
  Result:=false;

  if not fVisible then begin Log.LogDebug('Not visible.');exit;end;
  Log.IncreaseIndent(2);
  nx:=Event^.Button.x;
  ny:=Event^.Button.y;
  if (((Event^.type_=SDL_MOUSEBUTTONDOWN) or (Event^.type_=SDL_MOUSEBUTTONUP))
      and IsOver(nx,ny)) or (Event^.type_=SDL_MOUSEMOTION) or (Event^.type_=SDL_KEYDOWN)
      or (Event^.type_=SDL_KEYUP) or (Event^.type_=SDL_MOUSEWHEEL) then begin
    i:=fChilds.Count;
    while (i>0) and (not Result) do begin
      dec(i);
      Log.LogDebug('Passing event to object number '+inttostr(i)+' ('+fChilds[i].Name+')');
      Result:=fChilds[i].HandleEvent(Event);
    end;
  end;
  Log.DecreaseIndent(2);
  if not Result then Result:=inherited HandleEvent(Event);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
