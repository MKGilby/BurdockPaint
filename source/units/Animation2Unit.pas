{ -[Name]-------------------------------------------

     Animation class for SDL2

  -[Disclaimer]-------------------------------------

     You can freely distribute it.

     Written by Gilby/MKSZTSZ   Hungary, 2020

  -[Description]------------------------------------

    [to be written]

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.02.10
//     * Initial creation from AnimationUnit
//  V1.01: Gilby - 2020.05.28
//     * Reworked based on TAnimationData
//  V1.02: Gilby - 2020.06.25
//     * Following changes in AnimationDataUnit
//     - Name not needed when creating from TAnimationData
//  V1.03: Gilby - 2020.11.23
//     + PutFrame added. Puts the specified frame onto the main window.
//  V1.04: Gilby - 2021.03.03
//     - AddFrame removed it is inherited from TAnimationData
//     + HotPoint data is copied when created from TAnimationData
//  V1.05: Gilby - 2022.06.23
//     * From now TAnimation holds the entire Animation logic
//  V1.06: Gilby - 2022.07.08
//     + Added PutFramePart (with clipping)

{$mode delphi}

unit Animation2Unit;

interface

uses
  Classes, Lists, SDL2, mk_sdl2, AnimationDataUnit;

type

  { TAnimation }

  TAnimation=class(TAnimationData)
    constructor Create(iTexture:TTexture;iAnimationData:TAnimationData); overload;
    constructor Create(iName:string;iTexture:TTexture;iWidth,iHeight:integer); overload;
    constructor CreateStill(iName:string;iTexture:TTexture);

    procedure ResetFrameIndex;

    procedure Animate;  // Updates current frame for the next frame
    procedure PutFrame(pX,pY:integer;pFrameIndex:integer=-1);
    procedure PutFramePart(tX,tY,sX,sY,sW,sH:integer;pFrameIndex:integer=-1);
    procedure LogData;
  private
    fTexture:TTexture;
    fCurrentFrameIndex,
    fFrameAdvance,fEndFrame,
    fFrameDelayCount,fLoopDelayCount:integer;
    fFinished:boolean;
    procedure fSetFrameIndex(pFrameIndex:integer);
    procedure fSetReverseAnim(pNewValue:boolean);
    procedure fSetFrameDelay(pValue:integer);
    procedure fSetLoopDelay(pValue:integer);
  public
    property Texture:TTexture read fTexture;
    property ReverseAnim:boolean read fReverseAnim write fSetReverseAnim;
    property FrameDelay:integer read fFrameDelay write fSetFrameDelay;
    property LoopDelay:integer read fLoopDelay write fSetLoopDelay;
    property CurrentFrameIndex:integer read fCurrentFrameIndex write fSetFrameIndex;
  end;

  { TAnimations }

  TAnimations=class(TNamedList<TAnimation>)
    procedure AnimateAll;
  end;

implementation

uses SysUtils, MKStream, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.06';

{ TAnimations }

procedure TAnimations.AnimateAll;
var i:integer;
begin
  for i:=0 to Count-1 do Self[i].Animate;
end;

// -----------------------------------------------------------[ TAnimation ]---

constructor TAnimation.Create(iTexture:TTexture;iAnimationData:TAnimationData);
var i:integer;
begin
  fTexture:=iTexture;
  fName:=iAnimationData.Name;
  fWidth:=iAnimationData.Width;
  fHeight:=iAnimationData.Height;
  fFrameDelay:=iAnimationData.FrameDelay;
  fStartFrame:=iAnimationData.StartFrame;
  fLoopDelay:=iAnimationData.LoopDelay;
  fLooped:=iAnimationData.Looped;
  fRandomStart:=iAnimationData.RandomStart;
  fReverseAnim:=iAnimationData.ReverseAnim;
  fPaused:=iAnimationData.Paused;
  fPingPong:=iAnimationData.PingPong;
  fHotPointX:=iAnimationData.HotPointX;
  fHotPointY:=iAnimationData.HotPointY;
  for i:=0 to iAnimationData.FrameCount-1 do
    AddFrame(iAnimationData.Frames[i].x,iAnimationData.Frames[i].y);
  ResetFrameIndex;
end;

constructor TAnimation.Create(iName:string;iTexture:TTexture;iWidth,iHeight:integer);
begin
  fName:=iName;
  fTexture:=iTexture;
  fWidth:=iWidth;
  fHeight:=iHeight;
  fFrameDelay:=0;
  fStartFrame:=0;
  fLoopDelay:=0;
  fLooped:=false;
  fRandomStart:=false;
  fReverseAnim:=false;
  fPaused:=false;
  fPingPong:=false;
  fHotPointX:=0;
  fHotPointY:=0;
  ResetFrameIndex;
end;

constructor TAnimation.CreateStill(iName:string;iTexture:TTexture);
begin
  fName:=iName;
  fTexture:=iTexture;
  fWidth:=fTexture.Width;
  fHeight:=fTexture.Height;
  AddFrame(0,0);
  fPaused:=true;
  fFrameDelay:=0;
  fStartFrame:=0;
  fLoopDelay:=0;
  fLooped:=false;
  fRandomStart:=false;
  fReverseAnim:=false;
  fPingPong:=false;
  fHotPointX:=0;
  fHotPointY:=0;
  ResetFrameIndex;
end;

procedure TAnimation.ResetFrameIndex;
begin
  if fRandomStart then begin
    fCurrentFrameIndex:=random(FrameCount)
  end else
    if not fReverseAnim then begin
      fCurrentFrameIndex:=fStartFrame;
    end else begin
      fCurrentFrameIndex:=FrameCount-1;
    end;
  if not fReverseAnim then begin
    fFrameAdvance:=1;
    fEndFrame:=FrameCount-1;
  end else begin
    fFrameAdvance:=-1;
    fEndFrame:=0;
  end;
  if FrameCount=1 then fFrameAdvance:=0;
  fFrameDelayCount:=fFrameDelay+1;
  fLoopDelayCount:=fLoopDelay+1;
  fFinished:=false;
end;

procedure TAnimation.Animate;
begin
  if not fPaused then dec(fFrameDelayCount);
  if fFrameDelayCount=0 then begin
    fFrameDelayCount:=fFrameDelay+1;
    if fCurrentFrameIndex=fEndFrame then begin
      if fPingPong and (fFrameAdvance=1) then begin
        fFrameAdvance:=-1;
        fCurrentFrameIndex:=FrameCount-2;
        fEndFrame:=0;
      end else
        if fLooped then begin
          dec(fLoopDelayCount);
          if fLoopDelayCount=0 then begin
            if not fReverseAnim then begin
              if not fPingPong then
                fCurrentFrameIndex:=0
              else begin
                fCurrentFrameIndex:=1;
                fFrameAdvance:=1;
              end;
              fEndFrame:=FrameCount-1;
            end else begin
              fCurrentFrameIndex:=FrameCount-1;
              fEndFrame:=0;
            end;
            fLoopDelayCount:=fLoopDelay+1;
          end;
        end else
          fFinished:=true;
    end else
      inc(fCurrentFrameIndex,fFrameAdvance);
  end;
end;

procedure TAnimation.PutFrame(pX, pY: integer; pFrameIndex: integer);
begin
  if (pFrameIndex>=0) and (pFrameIndex<length(fFrames)) then
    with fFrames[pFrameIndex] do PutTexturePart(pX,pY,x,y,w,h,fTexture)
  else
    with fFrames[fCurrentFrameIndex] do PutTexturePart(pX,pY,x,y,w,h,fTexture);
end;

procedure TAnimation.PutFramePart(tX, tY, sX, sY, sW, sH: integer;
  pFrameIndex: integer);
begin

  if not((pFrameIndex>=0) and (pFrameIndex<length(fFrames))) then pFrameIndex:=fCurrentFrameIndex;
  with fFrames[pFrameIndex] do begin
    // Is the source area valid?
    if (sW>0) and (sH>0) and (sX<w) and (sX+sW>0) and (sY<h) and (sY+sH>0) then begin
      // Still do some clipping
      if sX<0 then begin sW+=sX;sX:=0;end;
      if sX+sW>w then sW:=w-sX;
      if sY<0 then begin sH+=sY;sY:=0;end;
      if sY+sH>h then sH:=h-sY;
      PutTexturePart(tX,tY,x+sX,y+sY,sW,sH,fTexture);
    end;
  end;
end;

procedure TAnimation.LogData;
var s:string;
begin
  Log.LogDebug('------ Animation data starts ------');
  Log.LogDebug('FrameCount='+inttostr(FrameCount));
  Log.LogDebug('CurrentFrameIndex='+inttostr(fCurrentFrameIndex));
  Log.LogDebug('EndFrame='+inttostr(fEndFrame));
  Log.LogDebug('FrameAdvance='+inttostr(fFrameAdvance));
  Log.LogDebug('FrameDelay='+inttostr(fFramedelay));
  Log.LogDebug('LoopDelay='+inttostr(fLoopdelay));
  s:='[';
  if fLooped then s+='X' else s+=' ';
  s+='] Looped   [';
  if fPaused then s+='X' else s+=' ';
  s+='] Paused   [';
  if fFinished then s+='X' else s+=' ';
  s+='] Finished   [';
  if fReverseAnim then s+='X' else s+=' ';
  s+='] ReverseAnimation   [';
  if fPingPong then s+='X' else s+=' ';
  s+='] PingPong';
  Log.LogDebug(s);
  Log.LogDebug('------ Animation data ends ------');
end;

procedure TAnimation.fSetFrameIndex(pFrameIndex: integer);
begin
  if (pFrameIndex>=0) and (pFrameIndex<FrameCount) then
    fCurrentFrameIndex:=pFrameIndex;
end;

procedure TAnimation.fSetReverseAnim(pNewValue: boolean);
begin
  fReverseAnim:=pNewValue;
  if fReverseAnim then begin
    fFrameAdvance:=-1;
    fEndFrame:=0;
  end else begin
    fFrameAdvance:=1;
    fEndFrame:=FrameCount-1;
  end;
end;

procedure TAnimation.fSetFrameDelay(pValue: integer);
begin
  fFrameDelay:=pValue;
  fFrameDelayCount:=fFrameDelay+1;
end;

procedure TAnimation.fSetLoopDelay(pValue: integer);
begin
  fLoopDelay:=pValue;
  fLoopDelayCount:=fLoopDelay+1;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

