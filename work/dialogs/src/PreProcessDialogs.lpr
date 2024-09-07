program PreProcessDialogs;

uses Classes, SysUtils, DOM, XmlRead, Logger, DialogTree, Lists;

type

  { TMain }

  TMain=class
    procedure Run;
  private
    fDialog:TDialog;
    procedure ProcessFile(pFilename:string);
    procedure ProcessHead(pNode:TDomNode);
    procedure ProcessControls(pNode:TDOMNode;pParent:TDialogItem);
    procedure ProcessLabel(pNode:TDOMNode;pParent:TDialogItem);
    procedure ProcessHorizontalSlider(pNode:TDOMNode;pParent:TDialogItem);
    procedure ProcessCheckbox(pNode:TDOMNode;pParent:TDialogItem);
    procedure ProcessSplit(pNode:TDOMNode;pParent:TDialogItem);
    procedure ProcessButton(pNode:TDOMNode;pParent:TDialogItem);
    procedure ProcessDivider(pNode:TDOMNode;pParent:TDialogItem);
  end;

{ TMain }

procedure TMain.Run;
var p1:string;SL:TFileSearchList;i:integer;
begin
  p1:=paramstr(1);
  if p1='' then p1:='*.xml';
  SL:=TFileSearchList.Create(p1,faAnyFile-faDirectory);
  try
    for i:=0 to SL.Count-1 do
      ProcessFile(SL[i]);
  finally
    SL.Free;
  end;
end;

procedure TMain.ProcessFile(pFilename: string);
var XML:TXMLDocument;Node:TDOMNode;
begin
  writeln('Processing '+pFilename+'...');
  ReadXMLFile(XML,pFilename);
  try
    Node:=XML.DocumentElement;
    with Node.ChildNodes do try
      if Count=2 then begin
        if uppercase(Item[0].NodeName)='HEAD' then ProcessHead(Item[0])
        else raise Exception.Create('First block must be HEAD inside DIALOG!');
        if uppercase(Item[1].NodeName)='CONTROLS' then ProcessControls(Item[1],fDialog)
        else raise  Exception.Create('Second block must be CONTROLS inside DIALOG!');
      end else
        raise Exception.Create('One HEAD and one CONTROLS block required inside DIALOG!');
    finally
      Free;
    end;
  finally
    XML.Free;
  end;
  if Assigned(fDialog) then begin
    fDialog.CalculatePositions(0,0);
    fDialog.Free;
  end;
end;

procedure TMain.ProcessHead(pNode: TDomNode);
var i:integer;Name,Caption,ClsName:String;
begin
  with pNode.ChildNodes do try
    for i:=0 to Count-1 do begin
      if uppercase(Item[i].NodeName)='NAME' then Name:=String(Item[i].TextContent)
      else if uppercase(Item[i].NodeName)='CLASSNAME' then ClsName:=String(Item[i].TextContent)
      else if uppercase(Item[i].NodeName)='CAPTION' then Caption:=String(Item[i].TextContent)
      else raise Exception.Create(Format('Unknown block inside HEAD! (%s)',[Item[i].NodeName]));
    end;
  finally
    Free;
  end;
  fDialog:=TDialog.Create(Name,Caption,ClsName);
end;

procedure TMain.ProcessControls(pNode: TDOMNode; pParent: TDialogItem);
var i:integer;
begin
  with pNode.ChildNodes do try
    for i:=0 to Count-1 do begin
      if uppercase(Item[i].NodeName)='LABEL' then ProcessLabel(Item[i],pParent)
      else if uppercase(Item[i].NodeName)='HORIZONTALSLIDER' then ProcessHorizontalSlider(Item[i],pParent)
      else if uppercase(Item[i].NodeName)='CHECKBOX' then ProcessCheckbox(Item[i],pParent)
      else if uppercase(Item[i].NodeName)='SPLIT' then ProcessSplit(Item[i],pParent)
      else if uppercase(Item[i].NodeName)='BUTTON' then ProcessButton(Item[i],pParent)
      else if uppercase(Item[i].NodeName)='DIVIDER' then ProcessDivider(Item[i],pParent)
      else raise Exception.Create(Format('Unknown block inside HEAD! (%s)',[Item[i].NodeName]));
    end;
  finally
    Free;
  end;
end;

procedure TMain.ProcessLabel(pNode: TDOMNode; pParent: TDialogItem);
var i:integer;Caption:string;
begin
  with pNode.ChildNodes do try
    for i:=0 to Count-1 do begin
      if uppercase(Item[i].NodeName)='CAPTION' then Caption:=String(Item[i].TextContent)
      else raise Exception.Create(Format('Unknown field inside LABEL! (%s)',[Item[i].NodeName]));
    end;
    pParent.AddItem(TLabel.Create(Caption));
  finally
    Free;
  end;
end;

procedure TMain.ProcessHorizontalSlider(pNode: TDOMNode; pParent: TDialogItem);
var i:integer;Name,SettingField:string;MinValue,MaxValue:integer;
begin
  with pNode.ChildNodes do try
    for i:=0 to Count-1 do begin
      if uppercase(Item[i].NodeName)='NAME' then Name:=String(Item[i].TextContent)
      else if uppercase(Item[i].NodeName)='SETTINGFIELD' then SettingField:=String(Item[i].TextContent)
      else if uppercase(Item[i].NodeName)='MINVALUE' then MinValue:=strtoint(String(Item[i].TextContent))
      else if uppercase(Item[i].NodeName)='MAXVALUE' then MaxValue:=strtoint(String(Item[i].TextContent))
      else raise Exception.Create(Format('Unknown field inside HORIZONTALSLIDER! (%s)',[Item[i].NodeName]));
    end;
    pParent.AddItem(THorizontalSlider.Create(Name,MinValue,MaxValue,SettingField));
  finally
    Free;
  end;
end;

procedure TMain.ProcessCheckbox(pNode: TDOMNode; pParent: TDialogItem);
var i:integer;Name,Caption,SettingField,Hint:string;
begin
  with pNode.ChildNodes do try
    for i:=0 to Count-1 do begin
      if uppercase(Item[i].NodeName)='NAME' then Name:=String(Item[i].TextContent)
      else if uppercase(Item[i].NodeName)='CAPTION' then Caption:=String(Item[i].TextContent)
      else if uppercase(Item[i].NodeName)='SETTINGFIELD' then SettingField:=String(Item[i].TextContent)
      else if uppercase(Item[i].NodeName)='HINT' then Hint:=String(Item[i].TextContent)
      else raise Exception.Create(Format('Unknown field inside CHECKBOX! (%s)',[Item[i].NodeName]));
    end;
    pParent.AddItem(TCheckBox.Create(Name,Caption,SettingField,Hint));
  finally
    Free;
  end;
end;

procedure TMain.ProcessSplit(pNode: TDOMNode; pParent: TDialogItem);
var s:string;atm:TDialogItem;
begin
  s:=uppercase(string(pNode.Attributes.GetNamedItem('direction').NodeValue));
  if s='HORIZONTAL' then atm:=THorizontalSplit.Create
  else if s='VERTICAL' then atm:=TVerticalSplit.Create
  else raise Exception.Create(Format('Unknown direction attribute in SPLIT! (%s)',[s]));

  pParent.AddItem(atm);
  ProcessControls(pNode, atm);
end;

procedure TMain.ProcessButton(pNode: TDOMNode; pParent: TDialogItem);
var
  Name,Caption,Hint,SettingField,Message:string;
  Save,Close:boolean;
  i,SettingValue,Group:integer;
begin
  Save:=false;
  Close:=false;
  SettingField:='';
  SettingValue:=0;
  Group:=0;
  with pNode.ChildNodes do try
    for i:=0 to Count-1 do begin
      if uppercase(Item[i].NodeName)='NAME' then Name:=String(Item[i].TextContent)
      else if uppercase(Item[i].NodeName)='CAPTION' then Caption:=String(Item[i].TextContent)
      else if uppercase(Item[i].NodeName)='SAVE' then Save:=String(Item[i].TextContent)='Y'
      else if uppercase(Item[i].NodeName)='CLOSE' then Close:=String(Item[i].TextContent)='Y'
      else if uppercase(Item[i].NodeName)='HINT' then Hint:=String(Item[i].TextContent)
      else if uppercase(Item[i].NodeName)='SETTINGFIELD' then SettingField:=String(Item[i].TextContent)
      else if uppercase(Item[i].NodeName)='SETTINGVALUE' then SettingValue:=strtoint(String(Item[i].TextContent))
      else if uppercase(Item[i].NodeName)='GROUP' then Group:=strtoint(String(Item[i].TextContent))
      else if uppercase(Item[i].NodeName)='MESSAGE' then Message:=String(Item[i].TextContent)
      else raise Exception.Create(Format('Unknown field inside BUTTON! (%s)',[Item[i].NodeName]));
    end;
    pParent.AddItem(TButton.Create(Name,Caption,Save,Close,Hint,SettingField,Message,SettingValue,Group));
  finally
    Free;
  end;
end;

procedure TMain.ProcessDivider(pNode: TDOMNode; pParent: TDialogItem);
begin
  pParent.AddItem(TDivider.Create);
end;

begin
  with TMain.Create do try Run; finally Free; end;
end.

