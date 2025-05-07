call ..\setenv.bat

%TOOLSDIR%\fontbuild2 data\newfont_org.png bdpfont.png -charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,.-?:_;>*<}{@&#][|\'#22+!%%/=()$#80#81#82#83#B0#84#85#86" -fixed
%TOOLSDIR%\fontbuild2 data\logofont.cel logofont.png -charset "BURDOCKPAINTo"
%TOOLSDIR%\fontbuild2 data\smallerfont.png smallfont.png -charset "0123456789ABCDEF-/" -fixed
%TOOLSDIR%\pngout bdpfont.png bdpfont2.png /K1 /Y
%TOOLSDIR%\pngout logofont.png logofont2.png /K1 /Y
%TOOLSDIR%\pngout smallfont.png smallfont2.png /K1 /Y
%TOOLSDIR%\pngout data\burdock.png burdock2.png /Y
%FPCDIR%\data2inc -B -A bdpfont2.png font1.inc BDPFont
%FPCDIR%\data2inc -B -A logofont2.png font2.inc LogoFont
%FPCDIR%\data2inc -B -A smallfont2.png font3.inc SmallFont
%FPCDIR%\data2inc -B -A burdock2.png burdock.inc BurdockPNG
%FPCDIR%\data2inc -B -A data\ntsc.col ntsccol.inc NTSCCOL
copy /b ..\base_copyright_notice.txt+font1.inc+font2.inc+font3.inc+burdock.inc ..\..\source\includes\data.inc
copy /b ..\base_copyright_notice.txt+ntsccol.inc ..\..\source\includes\ntsccol.inc
del *.png
del *.inc
