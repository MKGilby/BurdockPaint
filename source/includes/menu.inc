{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Menu message constants.
// Generated on 2024.09.18.

const
  MSG_NEWPROJECT=200;
  MSG_OPENPROJECT=201;
  MSG_SAVEPROJECT=202;
  MSG_SAVECLEARPROJECT=203;
  MSG_NEWIMAGE=204;
  MSG_OPENIMAGE=205;
  MSG_SAVEIMAGE=206;
  MSG_DUPLICATEIMAGE=207;
  MSG_REMOVEIMAGE=208;
  MSG_CLEARIMAGE=209;
  MSG_RESIZEIMAGE=210;
  MSG_CROPIMAGE=211;
  MSG_GETCEL=212;
  MSG_PUTCEL=213;
  MSG_CLIPCEL=214;
  MSG_RELEASECEL=215;
  MSG_OPENROTATECELDIALOG=216;
  MSG_FLIPCEL=217;
  MSG_OPENMAGNIFYCELDIALOG=218;
  MSG_GRAYSCALECEL=219;
  MSG_OPENCEL=220;
  MSG_SAVECEL=221;
  MSG_TOOLDRAW=222;
  MSG_TOOLBOX=223;
  MSG_TOOLLINE=224;
  MSG_TOOLCIRCLE=225;
  MSG_TOOLFILL=226;
  MSG_TOOLFILLTO=227;
  MSG_TOOLSEP=228;
  MSG_TOOLEDGE=229;
  MSG_INKOPAQUE=230;
  MSG_INKHGRAD=231;
  MSG_INKVGRAD=232;
  MSG_INKLGRAD=233;
  MSG_INKCGRAD=234;
  MSG_INKRGRAD=235;
  MSG_INKRANDOM=236;
  MSG_INKSOFTEN=237;
  MSG_INKTINT=238;
  MSG_OPENABOUTDIALOG=239;

const MenuBin:string=#6#7'PROJECT'#5#3'NEW'#200#0#0#0#21'CREATE A NEW PROJECT.'#4+
'OPEN'#201#0#0#0#23'OPEN PROJECT FROM FILE.'#4'SAVE'#202#0#0#0#21'SAVE PROJECT TO'+
' FILE.'#10'SAVE CLEAR'#203#0#0#0'7CLEAR UNDO DATA, RELEASE CEL THEN SAVE PROJECT'+
' TO FILE.'#4'QUIT'#1#0#1#0'!SAVE WORK STATE AND QUIT PROGRAM.'#5'IMAGE'#8#3'NEW'#204+
#0#0#0#27'ADD A NEW IMAGE TO PROJECT.'#4'OPEN'#205#0#0#0#21'OPEN IMAGE FROM FILE.'+
#4'SAVE'#206#0#0#0#19'SAVE IMAGE TO FILE.'#9'DUPLICATE'#207#0#0#0#16'DUPLICATE IM'+
'AGE.'#6'REMOVE'#208#0#0#0#26'REMOVE IMAGE FROM PROJECT.'#5'CLEAR'#209#0#0#0#25'C'+
'LEAR IMAGE TO KEY COLOR.'#6'RESIZE'#210#0#0#0#27'RESIZE IMAGE TO A NEW SIZE.'#4'C'+
'ROP'#211#0#0#0' CROP IMAGE TO THE SMALLEST SIZE.'#3'CEL'#11#3'GET'#212#0#0#0'4GE'+
'T A PART OF THE IMAGE INTO A TEMPORARY IMAGE (CEL)'#3'PUT'#213#0#0#0#21'PUT CEL '+
'TO THE IMAGE.'#4'CLIP'#214#0#0#0#27'GET CROPPED IMAGE INTO CEL.'#7'RELEASE'#215#0+
#0#0#19'FORGET CURRENT CEL.'#6'ROTATE'#216#0#0#0' ROTATE CEL BY 90'#176', 180'#176+
' OR 270'#176'.'#6'FLIP V'#217#0#0#0#20'FLIP CEL VERTICALLY.'#6'FLIP H'#217#0#1#0+
#22'FLIP CEL HORIZONTALLY.'#7'MAGNIFY'#218#0#0#0#28'MAGNIFY CEL TO 2X, 3X OR 5X.'#9+
'GRAYSCALE'#219#0#0#0#23'REMOVE COLORS FROM CEL.'#4'OPEN'#220#0#0#0'/OPEN CEL FRO'+
'M FILE. (LEGACY CEL, PNG, TGA, BMP)'#4'SAVE'#221#0#0#0#28'SAVE CEL TO FILE. (PNG'+
', TGA)'#5'TOOLS'#8#4'DRAW'#222#0#0#0')SELECT DRAW TOOL INSTEAD OF SELECTED ONE.'#3+
'BOX'#223#0#0#0'(SELECT BOX TOOL INSTEAD OF SELECTED ONE.'#4'LINE'#224#0#0#0')SEL'+
'ECT LINE TOOL INSTEAD OF SELECTED ONE.'#6'CIRCLE'#225#0#0#0'+SELECT CIRCLE TOOL '+
'INSTEAD OF SELECTED ONE.'#4'FILL'#226#0#0#0')SELECT FILL TOOL INSTEAD OF SELECTE'+
'D ONE.'#6'FILLTO'#227#0#0#0'+SELECT FILLTO TOOL INSTEAD OF SELECTED ONE.'#4'SEP.'+
#228#0#0#0'+SELECT "SEP." TOOL INSTEAD OF SELECTED ONE.'#4'EDGE'#229#0#0#0')SELEC'+
'T EDGE TOOL INSTEAD OF SELECTED ONE.'#4'INKS'#9#6'OPAQUE'#230#0#0#0'*SELECT OPAQ'+
'UE INK INSTEAD OF SELECTED ONE.'#6'H GRAD'#231#0#0#0'*SELECT H GRAD INK INSTEAD '+
'OF SELECTED ONE.'#6'V GRAD'#232#0#0#0'*SELECT V GRAD INK INSTEAD OF SELECTED ONE'+
'.'#6'L GRAD'#233#0#0#0'*SELECT L GRAD INK INSTEAD OF SELECTED ONE.'#6'C GRAD'#234+
#0#0#0'*SELECT C GRAD INK INSTEAD OF SELECTED ONE.'#6'R GRAD'#235#0#0#0'*SELECT R'+
' GRAD INK INSTEAD OF SELECTED ONE.'#6'RANDOM'#236#0#0#0'1SELECT RANDOM PIXELS IN'+
'K INSTEAD OF SELECTED ONE.'#6'SOFTEN'#237#0#0#0'*SELECT SOFTEN INK INSTEAD OF SE'+
'LECTED ONE.'#4'TINT'#238#0#0#0'(SELECT TINT INK INSTEAD OF SELECTED ONE.'#5'ABOU'+
'T'#1#5'ABOUT'#239#0#0#0#19'SHOWS ABOUT DIALOG.';

