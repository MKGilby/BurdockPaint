{ -[Name]-------------------------------------------

     TAnimationData class. Base for TAnimation.

  -[Disclaimer]-------------------------------------

     You can freely distribute it.

     Written by Gilby/MKSZTSZ   Hungary, 2020-

  -[Description]------------------------------------

    Contains animation data, except image.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.03.16
//     * Initial creation from Animation2Unit
//  V1.01: Gilby - 2020.03.22
//     + Added StartFrame
//  V1.02: Gilby - 2020.04.01
//     + Added TAnimationDatas type
//  V1.03: Gilby - 2020.04.01
//     + Name property added
//     * FrameCount is now equals the count of added frames,
//       thus not needed at Create.
//  V1.04: Gilby - 2021.03.03
//     * Added HotPointX and HotPointY property. It defines the hot point of
//       the animation, that will be at the x,y point of the sprite (if used in
//       AnimatedSprite) (effectively shifting the image -HotPointX,-HotPointY
//       pixels).
//  V1.05: Gilby - 2021.04.22
//     + Added LogData
//  V1.05a: Gilby - 2021.11.09
//     - Removed MKStream from uses.
//  V1.06: Gilby - 2022.03.18
//     * TAnimationDatas is now TNamedList (to be searchable for names).
//  V1.07: Gilby - 2022.06.23
//     + Added ReverseAnim.
//  V1.08: Gilby - 2022.07.19
//     + Added animation flag constants
//     + Added logging of animation flags

{$mode delphi}

unit AnimationDataUnit;

interface

uses
  SDL2, Lists;

const
  AF_LOOPED=1;
  AF_RANDOMSTART=2;
  AF_PAUSED=4;
  AF_PINGPONG=8;
  AF_REVERSEANIM=16;

type
  TAnimationData=class
    constructor Create(iWidth,iHeight:integer);  overload;
    procedure AddFrame(pX,pY:integer);
    procedure LogData;
  private
    function fGetFrame(index:integer):TSDL_Rect;
  protected
    fName:string;
    fWidth,fHeight:integer;
    fFrameDelay,fStartFrame,
    fLoopDelay:integer;
    fLooped,fRandomStart,fPaused,fPingPong,fReverseAnim:boolean;
    fFrames:array of TSDL_Rect;
    fHotPointX,fHotPointY:integer;
    function fGetFrameCount:integer;
  public
    property Width:integer read fWidth;
    property Height:integer read fHeight;
    property HotPointX:integer read fHotPointX write fHotPointX;
    property HotPointY:integer read fHotPointY write fHotPointY;
    property FrameCount:integer read fGetFrameCount;
    property Name:string read fName write fName;
    property FrameDelay:integer read fFrameDelay write fFrameDelay;
    property StartFrame:integer read fStartFrame write fStartFrame;
    property LoopDelay:integer read fLoopDelay write fLoopDelay;
    property Looped:boolean read fLooped write fLooped;
    property RandomStart:boolean read fRandomStart write fRandomStart;
    property ReverseAnim:boolean read fReverseAnim write fReverseAnim;
    property Paused:boolean read fPaused write fPaused;
    property PingPong:boolean read fPingPong write fPingPong;
    property Frames[index:integer]:TSDL_Rect read fGetFrame;
  end;

  TAnimationDatas=TNamedList<TAnimationData>;

implementation

uses SysUtils, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.08';

// -------------------------------------------------------[ TAnimationData ]---

constructor TAnimationData.Create(iWidth,iHeight:integer);
begin
  fWidth:=iWidth;
  fHeight:=iHeight;
  fHotPointX:=0;
  fHotPointY:=0;
  fStartFrame:=0;
  fFrameDelay:=0;
  fLoopDelay:=0;
  fLooped:=false;
  fRandomStart:=false;
  fPaused:=false;
  fPingPong:=false;
  fName:='';
end;

procedure TAnimationData.AddFrame(pX,pY:integer);
begin
  SetLength(fFrames,length(fFrames)+1);
  with fFrames[length(fFrames)-1] do begin
    x:=pX;
    y:=pY;
    w:=fWidth;
    h:=fHeight;
  end;
end;

function TAnimationData.fGetFrameCount:integer;
begin
  Result:=length(fFrames);
end;

function TAnimationData.fGetFrame(index:integer):TSDL_Rect;
begin
  if (index>=0) and (index<length(fFrames)) then
    Result:=fFrames[index]
  else begin
    Result.x:=0;
    Result.y:=0;
    Result.w:=1;
    Result.h:=1;
  end;
end;

procedure TAnimationData.LogData;
var i:integer;s:string;
begin
  Log.LogDebug('AnimationData logging starts:');
  Log.LogDebug(Format('Name: %s',[name]));
  Log.LogDebug(Format('Dimensions: %dx%d',[fWidth,fHeight]));
  Log.LogDebug(Format('Hotpoint: %d, %d',[fHotPointX,fHotPointY]));
  Log.LogDebug(Format('Frame and loopdelay: %d, %d',[fFrameDelay,fLoopDelay]));
  Log.LogDebug(Format('Framecount: %d',[length(fFrames)]));
  Log.LogDebug(Format('StartFrame: %d',[fStartFrame]));
  s:='     ';
  if fLooped then s[1]:='X';
  if fRandomStart then s[2]:='X';
  if fPaused then s[3]:='X';
  if fPingPong then s[4]:='X';
  if fReverseAnim then s[5]:='X';
  Log.LogDebug('Looped ['+s[1]+']  RandomStart ['+s[2]+']  Paused ['+s[3]+']  PingPong ['+s[4]+']  ReverseAnim ['+s[5]+']');
  Log.LogDebug('Frames:');
  for i:=0 to length(fFrames)-1 do with fFrames[i] do
    Log.LogDebug(Format('  %d. x=%d, y=%d, w=%d, h=%d',[i,x,y,w,h]));
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

