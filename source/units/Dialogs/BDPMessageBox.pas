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

unit BDPMessageBox;

{$mode Delphi}

interface

uses
  SysUtils, BDPModalDialog;

type

  { TBDMessageBox }

  TBDMessageBox=class(TBDModalDialog)
    constructor Create(iCaption,iMessage:string;iButtons:string='OK');
    procedure ReDraw; override;
    function Run:integer;
    function KeyDown(Sender:TObject;key:integer):boolean;
  private
    fMessage:string;
  end;

  function MessageBox(iCaption,iMessage:string;iButtons:string='OK'):integer;

implementation

uses MKMouse2, BDPButton, BDPShared, MKToolbox, sdl2, BDPMessage, mk_sdl2;

const
  MESSAGEBOXHEIGHT=96+MODALDIALOGCAPTIONHEIGHT-3;

function MessageBox(iCaption,iMessage:string; iButtons:string):integer;
var MessageBox:TBDMessageBox;j:integer;
begin
  MessageBox:=TBDMessageBox.Create(uppercase(iCaption), uppercase(iMessage),uppercase(iButtons));
  try
    Result:=MessageBox.Run;
    j:=MouseObjects.IndexOf(MessageBox);
    if j>-1 then MouseObjects.Delete(j);
  finally
    MessageBox.Free;
  end;
end;

{ TBDMessageBox }

constructor TBDMessageBox.Create(iCaption,iMessage:string; iButtons:string);
var atmB:TBDButton;i,x,buttoncount,key:integer;s:string;
begin
  if length(iButtons)=0 then iButtons:='OK';
  if iButtons[length(iButtons)]<>';' then iButtons+=';';
  buttoncount:=CountChar(';',iButtons);
  fMessage:=iMessage;
  inherited Create(max((length(iMessage)+2)*18,buttoncount*(NORMALBUTTONWIDTH+18)),MESSAGEBOXHEIGHT);
  fName:='MessageBox';
  ZIndex:=MODALDIALOG_ZINDEX;
  Caption:=iCaption;
  MouseObjects.Add(Self);
  x:=(fTexture.Width-(buttoncount*(NORMALBUTTONWIDTH+18))+18) div 2;
  i:=0;
  while length(iButtons)>0 do begin
    key:=-1;
    s:=copy(iButtons,1,pos(';',iButtons)-1);
    if (pos('^',s)>0) and (pos('^',s)<length(s)) then begin
      key:=SDL_SCANCODE_A+ord(s[pos('^',s)+1])-65;
      delete(s,pos('^',s),1);
    end;
    atmB:=TBDButton.Create(
      fLeft+x,
      fTop+MESSAGEBOXHEIGHT-44,
      NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
      s,'');
    atmB.Message:=TMessage.Init(MSG_MESSAGEBOXRESP,i,0);
    atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
    atmB.Tag:=key;
    atmB.OnKeyDown:=KeyDown;
    AddChild(atmB);
    delete(iButtons,1,pos(';',iButtons));
    inc(i);
    inc(x,NORMALBUTTONWIDTH+18);
  end;
  Visible:=true;
end;

procedure TBDMessageBox.ReDraw;
begin
  inherited ReDraw;
  if Assigned(fTexture) then begin
    MM.Fonts['Black'].OutText(fTexture.ARGBImage,fMessage,fTexture.Width div 2,42,1);
    fTexture.Update;
  end;
end;

function TBDMessageBox.Run:integer;
var msg:TMessage;
begin
  Result:=-1;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,48,12,24,255);
    SDL_RenderClear(PrimaryWindow.Renderer);
    MouseObjects.Draw;
    FlipNoLimit;
    HandleMessages;
    while MessageQueue.HasNewMessage do begin
      msg:=MessageQueue.GetNextMessage;
      case msg.TypeID of
        MSG_MESSAGEBOXRESP:Result:=msg.DataInt;
      end;
    end;
  until Result<>-1;
end;

function TBDMessageBox.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=false;
  if (Sender is TBDButton) then begin
    if key=TBDButton(Sender).Tag then begin
      MessageQueue.AddMessage(TBDButton(Sender).Message);
      Result:=true;
    end;
  end;
end;

end.

