fontbuild2 newfont2.bdc bdpfont.png -charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,.-?:_;>*<}{@&#][|\'#22+!%%/=()$#80#81#82#83#B0#84#85" -fixed
fontbuild2 logofont.cel logofont.png -charset "BURDOCKPAINTo"
copy ..\base_copyright_notice.txt ..\..\source\includes\fonts.inc
pngout bdpfont.png bdpfont2.png /K1 /Y
pngout logofont.png logofont2.png /K1 /Y
bin2inc bdpfont2.png ..\..\source\includes\fonts.inc BDPFont /a
bin2inc logofont2.png ..\..\source\includes\fonts.inc LogoFont /a
del bdpfont2.png
del logofont2.png
