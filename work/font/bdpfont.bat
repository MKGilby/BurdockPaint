fontbuild2 newfont_org.png bdpfont.png -charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,.-?:_;>*<}{@&#][|\'#22+!%%/=()$#80#81#82#83#B0#84#85#86" -fixed
fontbuild2 logofont.cel logofont.png -charset "BURDOCKPAINTo"
fontbuild2 smallerfont.png smallfont.png -charset "0123456789ABCDEF-/" -fixed
copy ..\base_copyright_notice.txt ..\..\source\includes\fonts.inc
pngout bdpfont.png bdpfont2.png /K1 /Y
pngout logofont.png logofont2.png /K1 /Y
pngout smallfont.png smallfont2.png /K1 /Y
bin2inc bdpfont2.png ..\..\source\includes\fonts.inc BDPFont /a
bin2inc logofont2.png ..\..\source\includes\fonts.inc LogoFont /a
bin2inc smallfont2.png ..\..\source\includes\fonts.inc SmallFont /a
del bdpfont2.png
del logofont2.png
del smallfont2.png
