mkconv2 convert.mc2
pngout burdock.png burdock2.png /Y
copy ..\base_copyright_notice.txt ..\..\source\includes\burdock.inc
bin2inc burdock2.png ..\..\source\includes\burdock.inc BurdockPNG /a
del burdock2.png
