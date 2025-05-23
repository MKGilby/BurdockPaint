{ -[Name]-------------------------------------------

   MKSZTSZ Visual Component Collection for SDL2

                         Visual Component Container

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2023-2025

  --------------------------------------------------

  -[Description]------------------------------------

   It is a container that can hold one or more child
   visual components. It can be invisible, but you
   can override Draw to make a visual appearance.
   (Put inherited Draw at the end to draw child objects)
   It covers the area behind it, so any mouseobjects
   behind this object won't get any Mouse events.

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
//  V1.00: Gilby - 2023.11.17
//    * Initial creation from vcc2_Container.
//  V1.01: Gilby - 2025.04.23
//    * Following changes in used units.

{$mode delphi}{$H+}

unit vcc2_ContainerStatic;

interface

uses Classes, MKMouse2, MKINIFile, Lists, vcc2_VisibleControlStatic;

type
  TMouseObjectList=TNamedList<TMouseObject>;

  TContainerStatic=class(TVisibleControlStatic)
    constructor Create; overload;
    constructor Create(iINI:TINIFile;iSection:string); overload;
    destructor Destroy; override;
    procedure AddChild(pObject:TMouseObject);
    procedure DeleteChild(pObject:TMouseObject);  overload;
    procedure DeleteChild(pName:String);  overload;
    procedure ClearChildren;
  protected
    fChildren:TMouseObjectList;
    procedure fSetVisible(pValue:boolean); override;
  public
    property Visible:boolean read fVisible write fSetVisible;
  end;
     
implementation

uses SysUtils, Logger;
     
const
  Fstr={$I %FILE%}+', ';
  Version='1.01';

constructor TContainerStatic.Create;
begin
  inherited Create;
  fChildren:=TMouseObjectList.Create;
  fLeft:=0;
  fTop:=0;
  fWidth:=64;
  fHeight:=24;
  fClicked:=false;
  fVisible:=true;
end;

constructor TContainerStatic.Create(iINI:TINIFile;iSection:string); overload;
begin
  inherited Create;
  fChildren:=TMouseObjectList.Create;
  Left:=iINI.ReadInteger(iSection,'Left',0);
  Top:=iINI.ReadInteger(iSection,'Top',0);
  Width:=iINI.ReadInteger(iSection,'Width',0);;
  Height:=iINI.ReadInteger(iSection,'Height',0);
  fClicked:=false;
end;

destructor TContainerStatic.Destroy;
var i,j:integer;
begin
  if Assigned(fChildren) then begin
    for i:=0 to fChildren.Count-1 do begin
      j:=MouseObjects.IndexOf(fChildren.Items[i]);
      if j>-1 then MouseObjects.Delete(j);
    end;
    fChildren.Free;
  end;
  inherited Destroy;
end;

procedure TContainerStatic.AddChild(pObject:TMouseObject);
begin
  fChildren.AddObject(pObject.Name,pObject);
  pObject.Visible:=fVisible;
  MouseObjects.Add(pObject);
end;

procedure TContainerStatic.DeleteChild(pObject:TMouseObject);
var i,j:integer;
begin
  i:=fChildren.IndexOfObject(pObject);
  if i>0 then begin
    j:=MouseObjects.IndexOf(fChildren.Items[i]);
    if j>0 then MouseObjects.Delete(i);
    fChildren.Delete(i);
  end;
end;

procedure TContainerStatic.DeleteChild(pName:String);
var i,j:integer;
begin
  i:=fChildren.IndexOf(pName);
  if i>0 then begin
    j:=MouseObjects.IndexOf(fChildren.Items[i]);
    if j>0 then MouseObjects.Delete(i);
    fChildren.Delete(i);
  end;
end;

procedure TContainerStatic.ClearChildren;
var i,j:integer;
begin
  for i:=0 to fChildren.Count-1 do begin
    j:=MouseObjects.IndexOf(fChildren.Items[i]);
    Log.Trace('ClearChildren '+inttostr(i)+'. '+inttostr(j));
    if j>-1 then MouseObjects.Delete(j);
  end;
  fChildren.Clear;
end;

procedure TContainerStatic.fSetVisible(pValue:boolean);
var i:integer;
begin
  inherited ;
  for i:=0 to fChildren.Count-1 do
    fChildren.Items[i].Visible:=pValue;

  fVisible:=pValue;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
