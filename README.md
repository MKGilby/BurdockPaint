# BurdockPaint
My AAT tribute

## Programming language
FreePascal (Lazarus 4.0.0 with FPC 3.2.2 Windows x64 or x86 version or cross-compiler to Win32 or Win64)
[Lazarus homepage](https://www.lazarus-ide.org/)

## Source codes
SDL2 pascal headers (source\units\sdl2) is licensed under MPL or zlib license.

GitHub for SDL2 pascal headers: [PascalGameDevelopment/SDL2-for-Pascal](https://github.com/PascalGameDevelopment/SDL2-for-Pascal)

The rest of source code is licensed under GNU GPL v3 (or later).

## Tools
PNGOut tool is by Ken Silverman [His homepage](http://advsys.net/ken)

FontBuild2 tool was made by me.

## Compiling in windows environment
1. Go into folder "work" and edit setenv.bat, set FPCDIR to point to the folder
containing your fpc.exe
2. Go into folder "source" and run BuildRelease_x64.bat (or x86 as you wish).
   You need Lazarus cross compiler libraries to be installed to compile x64 on x86 systems and vice-versa.
   The .exe will be put into \release\x64 or x86 respectively.
3. Download and extract the latest SDL2.dll into \release\x64 or x86
   (be aware of bitness!). The latest DLLs can be found on the [SDL releases page](https://github.com/libsdl-org/SDL/releases).
   Scroll down to the latest 2.xx version, click assets and download file.
   At the time of writing of this document the latest SDL2 version is 2.32.6.

## Compiled binaries from current build with datafiles and DLLs
[x64](https://mksztsz.hu/tmpfiles/BurdockPaint_0.9.0.75.zip "Download x64 version") or
[x86](https://mksztsz.hu/tmpfiles/BurdockPaint_x86_0.9.0.75.zip "Download x86 version").

## What's new

### 2025.07.27 - Build 75
- Bugfix: Gradient reflects changes on Banding dialog.

### 2025.06.29 - Build 74
- Bugfix: Grayscale ink internal name changed to GraySc to fit on button.
- Removed "Save" and "Load" button from gradient selector.

### 2025.06.21 - Build 73
- Added Grayscale ink.

### 2025.05.08 - Build 72
- Added a warning, that changing backup settings need an application restart to take effect!
- Fixed a bug in backup retention time and max file count handling.

### 2025.05.06 - Build 71
- Expanded backup settings. Now every aspect of the backup can be set from
  SETTINGS/BACKUP.
- Fixed a bug where Circle drawing setting was incorrectly saved as "Center" but
  the dialog showed "Boxed".

### 2025.05.05 - Build 70
- Moved generated dialog creating and destroying into a unit.
- Backup interval setting is modified to be usable with generated dialog.
- Added new menu SETTINGS/BACKUP. You can set backup interval here, no retention yet.

### 2025.04.29 - Build 69
- Added IMAGE/EXPORT menu. It replaces IMAGE/SAVE, use it to export the image
  to simple image formats (PNG, TGA and BMP currently).
- IMAGE/SAVE now uses bpimg format, it contains not just the image, but
  information about gradients, palette and undo operations.
- IMAGE/OPEN opens saved and exported formats too.

### 2025.04.28 - Build 68
- Dithering and ColorBanding settings are now stored for each gradient
  and not in global settings.
- Dithering and ColorBanding button added to GradientControl.
- Left clicking D and B toggles it.
- Right clicking D and B invokes corresponding config dialog.

### 2025.04.23 - Build 67
- Alpha is visible in colorboxes. (Color selector, gradient editor)
- Alpha is visible in side palette.
- Alpha is visible in cursor color.

### 2025.04.15 - Build 66
- Project backup folder name is changed to ".burdockpaint".
- The settings .ini file is renamed to settings.ini and placed inside the
  project backup folder to prevent littering in project folder.

### 2025.03.24 - Build 65
- Added checking if desktop resolution is big enough for minimum window size.
  If not, a small error window appears and the program terminates.
- Added checking if desktop resolution is big enough for window size stored in settings.
  If not, the stored window size is set to fit to desktop with a small margin,
  and the program starts with the new size.
- Added compiler directive to enable heap usage logging.

### 2025.03.17 - Build 64
- Draw tool now works with inks those don't support OnTheFly drawing.
- Undo/Redo buttons are refreshed after starting program.
- Undo/Redo buttons are refreshed correctly after drawing with any tool.

### 2025.01.06 - Build 63
- Project/Save Clear menu is renamed to Clean. It won't save project just remove
  undo and CEL data after asked if really wanted to.
- Project/Save now saves file into current project file (except when the file
  name is temp.bpprj, then asks for project file name)
- Project file name is included in window caption.

### 2025.01.04 - Build 62
- New image, opened image and duplicated image now can be placed after current image.
- Fix in loading selected tool or ink in settings when tool or ink index was above 5.
- Sep. now works in boxed mode too.

### 2025.01.02 - Build 61
- Updated used units.
- Fixed GIF reading.

### 2025.01.02 - Build 60
- Fix in saving 7th and 8th tool and ink buttons.
- Added SEP. configure dialog. The setting is not working yet.

### 2024.11.18 - Build 59
- Better creation of TControl.
- If the window is wide enough, there are 8 tool buttons and 8 ink buttons.

### 2024.10.29 - Build 58
- You can set window size in INI file (Settings/WindowWidth and WindowHeight).
- You can not set it below 1280x720.

### 2024.09.18 - Build 57
- Added CEL/Grayscale.

### 2024.09.07 - Build 56
- Implemented Image/Resize.
- Updated PreProcessDialog tool.

### 2024.08.30 - Build 55
- Added color banding to dithering. You can configure it in the dithering
  configuration dialog.

### 2024.08.28 - Build 54
- Circle drawn by bounding box now have the largest possible radius.

### 2024.08.26 - Build 53
- Added Circle configuration dialog.
- Added alternative Circle drawing mode. (Draw bounding box.)
- Updated PreProcessDialog tool.
- Releases are split into different x86/x64 folders.
- Updated SDL2 library.

### 2024.08.03 - Build 52
- Added CEL/Clip. It clips the smallest possible CEL from the current image
  containing all pixels that are not opaque black.

### 2024.08.02 - Build 51
- Implemented Image/Open. (It was there but was not implemented)
- Only one opendialog is created, the title and filter changes on the fly.

### 2024.06.04 - Build 50
- Bugfix in Soften. (Didn't work at all)
- Added configure dialog to Soften. (Settings already were there.)
   (Generated, of course!)

### 2024.05.17 - Build 49
- Line now displays the length of the line too.

### 2024.02.23 - Build 48
- Some dialog's source code are now generated from XML.
- Removed BDC file format from the hints for CEL/Open and CEL/Save.

### 2024.01.19 - Build 47
- You can now have more than 8 gradients in GradientSelector.

### 2023.12.08 - Build 46
- Soften can process alpha channel too (settable in INI/SoftenAlphaToo).
- You can set Soften center pixel weight in INI/SoftenCenterWeight.

### 2023.12.06 - Build 45
- Fix in GradientSelector/Delete.
- Fix in ColorEditor/Palette.

### 2023.12.01 - Build 44
- Image/Save works. You can save image into PNG, TGA or BMP.

### 2023.12.01 - Build 43
- Added Image/Crop. It crops the image to the smallest size. This operation is
  undoable/redoable.

### 2023.12.01 - Build 42
- BUGFIX: ColorPalette2 not refreshes when changing active image.
- BUGFIX: ColorEditor not refreshes when changing active image.
- BUGFIX: ColorPalette2 not refreshes when changing palette in ColorEditor.

### 2023.11.27 - Build 41
- Added ConfigureTintDialog.
- Tint now works with CELs too.

### 2023.11.20
- DrawArea drawing is reworked, uses less memory and supports checkered alpha
  background when transparent pixels are used.
- Sliders and main Controls use static texture.
  Memory footprint is reduced to 40M.

### 2023.11.19
- Infobar uses static texture.

### 2023.11.17
- Controls using static texture are a thing now. They save memory.
  Max process memory is now 92M in TaskManager. (And I am not done yet!)

### 2023.11.16
- Trying to decrease memory footprint. Started with 141M now we are at 116M.
  (Measured with TaskManager, max process memory column.)
  Still feels too much for a drawing app...
  All visible controls are TStreamingTextures, probably we should introduce
  a static visible control ancestor for TButton and other rarely changing controls.

### 2023.11.15
- Added checkered background to gradients to visualize alpha channel.
- BUGFIX: App crashed when no gradient was selected, and you pressed Edit in GradientSelector.
- BUGFIX: Duplicating image not worked.
- BUGFIX: Adding new image added a bogus image.

### 2023.11.14
- Add and Delete buttons work in GradientSelector. Undo/Redo also works for them.
  (Don't add more than 8 gradients, you can only access first 8 yet.)
- GradientSelector refreshes when changing images.
- Gradient selector and editor show gradient without modifier flags
  (reversed, pingpong).

### 2023.11.13
- Fixed a bug with undoing/redoing drawing operations. (Image piece was put in
  the wrong place.)

### 2023.11.10
- GradientSelector Undo/Redo state is saved into the project.

### 2023.11.09
- Clicking Edit in GradientSelector opens GradientEditor with the selected gradient.
- In GradientSelector the Undo/Redo buttons work. (Undostate not yet saved.)
- GradientEditor discards Undo/Redo data from previous editing.

### 2023.11.06
- Clicking the down arrow in the GradientControl opens GradientSelector.
- Right clicking the gradient doesn't do anything, but it will open the
  GradientEditor directly with the current gradient.
- In the GradientSelector you can click on a gradient and select it by clicking
  on the Select button.
- The Add/Delete/Undo/Redo buttons are not working yet.

### 2023.11.03
- Backup is created only if the project is changed since the last backup.
- You can limit the amount of backed up files by file count.
- CoordinateBox shows color of palette entry when moving mouse over palette sidebar.

### 2023.11.02
- Added palette sidebar. Click to select color and use mouse wheel to browse
  all 256 palette colors.

### 2023.10.26
- OpenCEL can open BMPs.
- CEL can be saved as BMP.
- BUGFIX: Tried to use a non-existing ink when no INI file was found.
- BUGFIX: Selecting a color in the gradient removes the color indicator from
  ColorSelector.

### 2023.10.25
- GDT block can be compressed from now.

### 2023.10.24
- You can undo/redo color changes in GradientEditor.
- You can undo/redo ticking/unticking boxes in GradientEditor.
- You can undo/redo moving knobs in GradientEditor.

### 2023.10.20
- Gradients can have three inner colors. You can toggle them, set their colors
  and position in the GradientEditor.

### 2023.10.16
- Color clusters now are called Gradients, since they are no longer clusters of
  palette colors.

### 2023.10.09
- ColorEditor is now a ModalDialog.
- ColorEditor is rearranged, now showing entire palette (256 colors).
- Modal dialogs now darken the main window.

### 2023.10.03
- Modal dialogs now have titles.

### 2023.09.22
- CoordinateBox updated. It shows color components of color under the mouse.
- BUGFIX: New image added an invisible colorcluster, now it adds a black-white.

### 2023.09.19
- Fix in creating ink buttons.
- Any exceptions raised in TMain now are logged.
- Image count slider now displays proper value after starting with a project
  with more than one images.

### 2023.09.18
- New file format introduced. See fileformats.txt.
- Use ProjectUpdate1 to convert old bpprj files to new format.
  (Open in hex editor, if first 3 chars is not PRJ then it's old format)

### 2023.09.10
- Left clicking on palette color set sliders to that color.
- Right clicking on palette color stores the color into the palette.

### 2023.09.07
- Clicking on DrawArea when PaletteEditor is open sets sliders according to clicked color.

### 2023.09.06
- Added ColorPalette to PaletteEditor.
- Right-clicking on ColorSelector invokes PaletteEditor with the clicked color
  (only the two ends and the main color).
- Clicking Select button on PaletteEditor sets the color of the right clicked part
  of the color selector.
- Clicking Cancel button closes the PaletteEditor without changing any color.

### 2023.09.05
- Added buttons to PaletteEditor.
- Moved and enlarged ColorBox on PaletteEditor.

### 2023.09.04
- Values and color change as you move the sliders on the PaletteEditor.

### 2023.09.01
- Values change as you click and move mouse over the HSBox (big colorful area).
- Added Light slider below the HSBox.
- Values change as you click and move mouse over the Light slider.

### 2023.08.25
- Added new PaletteEditor.
- You can click the big colorful area to move the crosshair.
  (No values are changing yet.)

### 2023.08.11
- BUGFIX: Backup retention by size now works. Simply the settings was not passed
  to the FileBackup class.
- Added the new color selector, but cannot click it yet.

### 2023.08.10
- It seems that drawing and menus work again but in full true-color mode.
  The missing things are PaletteEditor and ColorSelector. Probably both should
  be reworked to work better with true-color mode.

### 2023.08.07
- Loading V1 and saving as V2 works.

### 2023.08.03
- Stripped project down to an empty TMain. Adding items back one by one.
- Menu is there, but clicking on anything is not working.
- About/About works.
- File/Quit (and pressing Q) works.

### 2023.07.31
- Decided to transition to true-color mode. Palette will only hold your favourite
  colors.

### 2023.07.18
- CEL/Export is merged into CEL/Save.
- CEL/Open now opens BDC, CEL, PNG and TGA.
- The loaded CEL file is color fitted to the current image's palette.

### 2023.07.15
- Added EDGE tool. It draws the clicked color's edge with the current ink.
  (It does it everywhere on the image!)

### 2023.07.14
- BUGFIX: Drawing one line thick vibrocolor in topmost row or in leftmost column.

### 2023.06.27
- BUGFIX: Drawing box upwards with "Add" ink only draws it's upper line.

### 2023.06.26
- You can set "backups" folder max size in .INI file (BackupFolderMaxSize, in bytes)
- You can set "backups" folder max retention time in .INI file (BackupFolderRetentionTime, in seconds)

### 2023.06.23
- Added "Image"/"Duplicate" menuitem. Same as "Image"/"New" but instead a
  new empty image, it duplicates the current image.
- BUGFIX: Changing active image doesn't refreshed visible color cluster.
- R Grad max repetition count is raised from 16 to 64.

### 2023.06.14
- Palette ramping is undoable/redoable now.
- Project file is save and backed up once per minute.
  (Backup interval is read from the settings.)

### 2023.06.13
- Changing a single color multiple times now counts as one undo operation.

### 2023.06.08
- Selecting a color cluster in Palette Editor now changes visible color cluster
  on Controls too.

### 2023.06.07
- Fill tool with Add ink is fixed.
- App can be closed with red X again.

### 2023.06.06
- "Center" button works in R Grad tool configuration.

### 2023.06.02
- New R Grad tool is ready.
- Right clicking R Grad tool invokes tool configuration.
  ("Center" button not working yet.)

### 2023.06.01
- Color cluster selector completed:
  - Click "ADD" to add new color cluster (up to 16)
  - Click "X" on the right side of the cluster to remove it.
  - The selected color cluster is highlighted with red border.
  - You cannot delete selected color cluster. (This ensures that at least one
    color cluster exists.)
- Right clicking on color cluster colors part behaves two way:
  - Opens palette editor if color cluster is on Controls.
  - Starts color cluster picking of color cluster is on PaletteEditor.
- "Add" ink no longer supports on the fly drawing. This fixes it's problem with
  the Circle tool.
- Renamed R Grad to C Grad (circular gradient).

### 2023.05.25
- Color cluster selector's width is matched with the calling control.
- "X" added to each color clusters in selector.
- Clicking "X" removes the color cluster.

### 2023.05.24
- Clicking on the arrow on the right of the color cluster invokes color cluster
  selector. Click on a color cluster to select it or click anywhere outside the
  window to close it.
- There's and ADD button on the color cluster selector. Click to add a new color
  cluster.

### 2023.05.23
- Changed licensing to GNU GPL v3.
- PutCEL now uses current ink to put CEL, as in original. To simply put CEL as
  it is, select Opaque ink.
- Added "ADD" ink. It adds the selected color index to the image's color index.
  The color index can't change color bank, if it under/overflows, it comes back
  from the other side. (So you can substract selecting colors from the end of the bank.)
- When used with PutCEL it processes CEL's every pixel with the underlying image's pixel.

### 2023.05.22
- Added Soften ink.
- Added menus for Tools and Inks. Clicking the items will change the currently
  selected tool or ink to the selected one.
- The tools/inks that are selected to the six button on Controls are disabled in
  TOOLS/INKS menu.

### 2023.05.19
- When passing a project file as parameter to open, the open/save dialogs will
  bring up the project's folder by default.

### 2023.05.18
- You can toggle (F), (C) and (D) buttons by keyboard (F2-F4 respectively).

### 2023.05.15
- Added CLUSTER menu with one item (RAMP).
- Menus are disabled/enabled as switching to PaletteEditor and back.
- Cluster/Ramp works. It creates a color ramp from the start color to the
  end color of the specified color cluster.

### 2023.05.11
- Right clicking on RGrad button hides Controls and invokes RGrad configuration.
- Added Random ink.

### 2023.05.10
- Added RGrad ink.

### 2023.05.09
- Added (D) button, it toggles gradient dithering. Right clicking it opens DitherDialog.
- Inks respects dithering settings.

### 2023.05.08
- Right clicking on the palette area in PaletteEditor activates color picker
  instead of closing PaletteEditor.
- Left clicking on color bars in ColorCluster selects the clicked color.
- Right clicking on color bars in ColorCluster activates color cluster picking.
  Click on start color then click on end color.
- Passing a filename to the program uses that file as temporary project file.
- Removed console window in RELEASE mode.

### 2023.05.04
- Fix in BDPMap. Pingpong and Reversed ColorCluster flags was mixed up.
- ColorCluster now has buttons for Reversed and Pingpong modes. They works!
- Fix in saving ColorCluster flags.

### 2023.05.03
- Fixes in Project and UndoSystem to compile to Win32 target.

### 2023.05.02
- New ColorCluster outfit. Added an arrow to the right side, it will be used to
  open the ColorClusterSelector.
- GetIndexAt and GetIndexAtDithered fix. The last color was not given back.
- PaletteEditor now shows the active image's color cluster.
- Project/Quit was broken, fixed.

### 2023.04.28
- Project/Open works.
- Project/"Save clear" works.

### 2023.04.27
- Added dithering to LGrad. It is fixed now, but the setup dialogs will solve this.

### 2023.04.26
- Color selector colors are increased to 9 to be the same width as ImageCountSlider.
- Added ImageCountSlider. You can change active image in Project with it.
- Undo/Redo buttons are refreshing as you change active image.
- Image/New works. It adds an empty 320x200 image to the Project. It brings up
  a dialog to select new image's place:
  - First: New image will be the first image of the Project.
  - Last: New image will be the last image of the Project.
  - Insert: New image will be inserted before the current image into the Project.
  - Cancel: Don't create image.
- Image/Remove works (if there are more than 1 images in Project). It brings up
  a confirmation dialog.
- Image/Remove is enabled/disabled based on image count in Project.
- Created a tool to show map of files created by BurdockPaint. (BDPMap)

### 2023.04.25
- BUGFIX: Losing the first click after receiving focus (after the external save dialog)
  is a feature in SDL2 which can be disabled with:
  SDL_SetHint(SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, '1');
- Default palette is loaded from BDP format instead of COL.
- Added Changed property to TBDPalette.

### 2023.04.24
- Project/Save works, a pop-up message appears on successful save.
- BUG: You have to click twice the pop-up message's button.
  First click is hijacked by some other control. (Need investigation!)

### 2023.04.21
- Merged button, slider and colorbox classes in one unit (BDPBasicControls).
- Renamed units (removed the word unit from the end of the names).

### 2023.04.14
- Menus are loaded from external file. (look at \work\menu)
- Added 16x zoom. (+1 level)
- Added About menu.
- Renamed SplashScreen to AboutDialog.
- Updated AboutDialog with licensing info and added a button to close it.

### 2023.04.13
- Removed system state from fileformats.txt.
- Fixed undo block types in fileformats.txt.

### 2023.04.12
- Restructured fileformats.txt.
- Added Extended Image data block (E) and Project data block (P) to fileformats.txt.
- Added project and extended image class with saving to and loading from stream.
- BDPUndoUnit variable names uniformized, added CreateFromStream where missed.
- Global variables are changed to Project properties:
  MainImage, OverlayImage, ColorClusters, CELImage, ImageUndoSystem, PaletteUndoSystem
- OverlayImage palette is separated from OverlayImage.

### 2023.04.11
- New color cluster system.
- Gradient inks uses the new color cluster system.
- Preparations for saving/loading color clusters and the visual control of it.
- Color clusters are saved into system state and read back on startup.
- Added ColorCluster to PaletteEditor.
- ColorCluster shows the selected color.

### 2023.04.09
- Added ColorBox to PaletteEditor. It shows the selected color in a bigger area.

### 2023.04.07
- Following changes in vcc2_VisibleControl unit.

### 2023.04.06
- Button's texture is only updated when button state (selected or enabled) is
  changed.

### 2023.04.05
- SelectColor tool hinting fixed, now it allows showing hints from other controls.
- ColorSelector added to PaletteEditor.
- Messages are forwarded only to visible controls.
- Right clicking on ColorSelector in PaletteEditor allows you to pick color from
  the image or from the palette.
- Added two mouse click icons to the new font. This makes the old font unusable,
  since it doesn't have these icons.
- Added mouse click tips to SelectColor and PickColor hints.
- Right clicking the palette area of the PaletteEditor closes it (as the hint says).
- Added the code that creates the four corner arches.
- Made the arches in buttons and sliders split into four corners. So the size
  of the control is variable.

### 2023.04.04
- Under the hood: MKMouse2 changes and follow ups.

### 2023.03.29
- UndoSystem is distilled to a base class and added ImageUndoSystem and ColorUndoSystem.
  The latter is not saved in system state yet.
- File format change in system state. This one breaks the compatibility with older
  saves. (Added Version so it won't do it in the future.)
- ColorUndoSystem is included in the system state save.
- After changing palette color with sliders it is possible to undo/redo the change.
- When leaving slider with button down, it goes back to original position.

### 2023.03.28
- Moving mouse over the palette in the PaletteEditor show color hint.
- When you are in the PaletteEditor, color hint only visible over the DrawArea or
  over the palette colors.
- Releasing RMB after panning no longer exits PaletteEditor.
- Added mouse panning enum, instead of numbers.
- Added Undo/Redo buttons to PaletteEditor. (Not working yet.)

### 2023.03.24
- Added a new, modern font for the modern look. (Drawn entirely with this program!)
- InfoBar now appears above the current panel (Controls or PaletteEditor),
  and on the top of the window when no panel is visible.
- GetCEL and PutCEL now shows coordinates in first state too.
- Added degree symbol to the rotate dialog.
- Submenus can have hints.

### 2023.03.23
- Code cleanup in MainUnit.
- When you release CEL, all menus requiring active CEL are disabled.
- When you get or load a CEL, all menus requiring active CEL are enabled.
- Under the hood: MouseObjects only calls draw for visible objects.
- CEL/Put works.
- CEL/Save works. It saves the CEL in .bdc format. (See fileformats.txt)

### 2023.03.22
- Modal dialogs are reworked, less code to create a new one.
- Fix in rotate, 180 and 270 crashed.
- Modal dialog units are merged into one file.

### 2023.03.21
- Some tools didn't clear the Infobar after them. Fixed.
- Rotate CEL dialog appears when selecting CEL/Rotate. You can select the amount
  to rotate by the buttons, or pressing keys 1..3. Clicking "OK" or pressing Enter
  will rotate the CEL, "Cancel" will close the dialog without doing anything.

### 2023.03.20
- Undo/Redo buttons now uses OnClick event instead of the MessageQueue.
- SubMenuItems now can be disabled. (Appears in grey and remains grey when the
  mouse points it.)
- File/Quit works, but it quits instantly without asking anything.
- Added CELSHOW tool. It shows CEL for a short interval (1s). Useful after
  loading or modifying cel.
- The loaded CEL is always moved to 0,0. After loading CEL the CEL is visible
  for a short time.
- Fix: MessageQueue.AddMessage still accepted string value but did nothing with it.
- CEL/Flip V and CEL/Flip H works.
- Magnify CEL dialog appears when selecting CEL/Magnify. Dialog items work, but
  CEL is not magnified when OK clicked. You can use key 2, 3 and 5 to select
  magnification, Enter to "OK" and Escape to "Cancel".
- Magnification works.
- PutCEL now shows CEL's top-left coordinate and the relative position to
  original place.

### 2023.03.19
- Fix: When right clicked on drawarea to cancel color picking, ToggleControls
  also occured.
- Fix: When right clicked to cancel any tool, ToggleControls also occured.
- ToggleControls now toggles MainMenu too, to let the user see the whole window.
- Closing the window with red X now shuts down app correctly.

### 2023.03.18
- Fix: You couldn't right click the key color to pick another color in color selector.
- Fix: When the same color appears more than once in the color selector, right
  clicking any of it caused all of them to appear as color picking target.

### 2023.03.17
- Color sliders moved just above the palette block in PaletteEditor.
- Fixed mouse wheel events.

### 2023.03.16
- MediaManager is reduced to GFXManager to remove the dependency on BASS.DLL.
  Should switch Bass to SDL2 but that's out of this project's scope.
- BuildRelease.bat added. It builds release executables (both x86 and x64) and data file
  into ..\_release.
- SDL2 dll names are changed to allow both dlls to be in the same folder.
  Source modified to pick the correct dll.

### 2023.03.15
- Sliders are visible again.
- BUG: Sliders don't receive MouseWheel event.
- Added and wired Alpha slider.
- Changed target architecture to x64.
- Changed SDL2 libraries to SDL2-for-Pascal (more up to date).

### 2023.03.14
- Color selector visible and works again.
- Still working under the hood.

### 2023.03.11
- Big rework under the hood regarding mouse events and visual components,
  so a few things are broken now:
  - Color selector is not visible.
  - Sliders are not visible. (If somehow you can invoke palette editor.)

### 2023.03.10
- SubMenu items insert a message into queue when clicked.
- Clicking on CEL/Load invokes the windows standard open file dialog. Browsing an
  original AAT CEL file and click open will load the file into CELImage.
  It doesn't yet color fits the loaded CEL.
- MSG_MOUSECOORDS messages are removed. They caused queue overflow because when
  a file dialog was open, but the mouse moved over drawarea, SDL accumulated the
  events and fired all at once when the dialog was closed. Coordinates are read
  from DrawArea and passed to Controls in main loop instead.
- Clicking on Picture/Clear clears MainImage to key color.
- Clicking on CEL/Release frees up CELImage. This operation is not UNDOable!
- GetCEL hides and shows MainMenu beside Controls.
- Clicking on CEL/Get invokes GetCEL tool.
- BUG: The whole fControls stopped working.

### 2023.03.09
- Submenu opens when hovering mouse on menu. Moving to another menu or outside
  the menu bar or submenu hides the submenu.
- Moving the mouse over the submenu moves the selection.

### 2023.03.08
- Using mouse wheel over the palette colors in PaletteEditor switches banks.
- Menu appears in the top row of the screen.

### 2023.03.07
- Moved the vertical slider's mouse wheel event to the vcc_SliderLogic unit, where it belongs.
- Added wheel event to the horizontal slider too.

### 2023.03.06
- You can change the vertical slider with mouse wheel.
- The vertical slider in PaletteEditor switches banks.
- Using the mouse wheel over the PaletteEditor no longer zooms DrawArea.

### 2023.03.03
- CreateRelease.bat added.
- KeyMapping is saved back to INI file.
- When in PaletteEditor, right clicking on DrawArea exits PaletteEditor.
- Added a vertical slider to PaletteEditor, it will switch the palette banks. (Not yet doing it.)

### 2023.03.02
- Added sliders to PaletteEditor. They are not changing anything yet.
- Sliders now change the currently selected colors RGB channels.
- When invoking PaletteEditor, the sliders are set to the selected color's RGB values.
- Clicking on the colors on the PaletteEditor changes the current color and sets
  the sliders accordingly.
- After invoking PaletteEditor, clicking on the draw area selects current color.

### 2023.03.01
- Removed string data part from Messaging subsystem.

### 2023.02.28
- New tool: PickColor (PICKCOL). When activated, click anywhere on the draw area
  to pick a color. That color is put on the MessageQueue in a MSG_PICKEDCOLOR message.
- Right clicking on ColorSelector invokes PICKCOL, and the picked color
  is put into the selected slot.
- The picked colors are saved in INI file.
- Middle click on ColorSelector to invoke PaletteEditor.
- The currently "picked" color slot is highlighted with VibroColors.
- FIX: Clicking the thin line beetween the key color and the other colors
  no longer invokes color picking for the selected color.
- Pressing F1 changes the ActiveColorIndex to the color under the cursor.
- Get rid of MSG_ACTIVATETOOL and MSG_ACTIVATEINK. Using the buttons TAG property
  and OnClick event instead.
- Changed mouse button checks to SDL_BUTTON* constants.

### 2023.02.27
- Undosystem now saves only region data instead of palette+region in region type
  undoitem. (It was correct in the file format text, only implementation was wrong.)
- New block type in filetypes (SystemState - S-block).
- System state (image current state, palette, undo state and CEL if exists) saved
  to state.bds.
- ColorSelector selects color only by left clicking on it.
- Removing some obsolete code and declarations here and there.
- Button appearance can be changed in INI file. (Settings/ModernGraphics)
- Right clicking on ColorSelector invokes PaletteEditor. Cannot go back, you have
  to quit (Q->Y).
- Palette Editor shows the first 256 colors, highlights the active color.

### 2023.02.26
- Undo/Redo buttons are disabled when nothing to undo/redo.
- BUGFix: Sometimes the render to texture failed to render. Especially when after
  changing zoom level the whole picture got outside of DrawArea.
- Message handling moved to TControls for messages needed to be handled by TControls.

### 2023.02.25
- UndoSystem state saved and loaded (temp.bdu).

### 2023.02.24
- File format changed.
- File format documented in fileformats.txt.

### 2023.02.22
- Added Undo/Redo code to Circle.
- Added Undo/Redo code to Draw.
- BugFix: Controls now captures OnMouseDown if none of its child does it.
  This prevents Draw to start drawing without pressing LMB over the draw area.
- Added Undo/Redo code to Fill.
- Added Undo/Redo code to FillTo.
- Added Undo/Redo code to Line.
- Added Undo/Redo code to Sep.
- Added Undo/Redo code to PutCEL.
- Added UndoLimit to Settings (16 steps by default).
- UndoSystem respects UndoLimit. If a new item is added over the limit,
  the oldest item is removed to keep the item count in the limit.
- Fix: TBDPalette.CopyColorFrom ignored start and count.

### 2023.02.21
- Added Undo/Redo mechanism. (Not tested yet...)
- Added Undo/Redo buttons. (Not working yet...)
- Added Undo/Redo code to Box. You can now undo and redo Box drawing.

### 2023.02.19
- PutCEL now works as intended.
- PutCEL respects the state of (K) button.
- Vibrocolors is now a class and changes colors based on ticks instead of program loops.

### 2023.02.17
- Created a logo font, it is used in the splash screen.
- Added icon.
- GetCEL now stores position too.
- Pressing ESC or ` when in GetCEL or PutCEL cancels curent operation.
- Image position saved into BDP file. (Needed in CEL)
- PutCEL almost works as expected. Only thing is missing putting the CEL onto the image.

### 2023.02.16
- Pressing ` (the key below ESC) invokes PUTCel. It doesn't work fully, you can
  just move the picked CEL image around with the mouse, until you quit.
  Cannot continue now, I drank a beer and cannot concentrate anymore. :)

### 2023.02.15
- Added color selector to controls.
- Added selected colors to Settings, so they are saved on exit and loaded on startup.
- Clicking on color selector selects the clicked color.
- The selected color is used when Opaque ink is selected.
- The selected color is saved into Settings.
- On startup the color selector selects the box that contains the color saved in Settings.

### 2023.02.14
- Small bugfix in writing color index under cursor.
- Removed (A) button. Has no meaning with paletted mode.

### 2023.02.13
- Bugfix with clickin anomaly. Click was on mouseup in TMouseObject, but was
  on mousedown in TDrawArea. Changed it in TDrawArea.

### 2023.02.12
- GETCEL really copies the selected area into CELImage.
  (It saves it to a PNG on exit, to be sure.)
- CEL is saved on exit and reloaded on startup.
- BUG narrowed down: The colored line appears only in PNG file and
  only with odd width. So the bug is in the PNG writer.
- BUG fixed: Writing paletted images with odd with and 4, 2 or 1 bitdepth was
  bugged. (Had to move the bit shift operations after adding the color bits.)

### 2023.02.10
- Added Pinnable property to tools, to differentiate between tools that can be
  selected in tool selector and the tools that cannot be selected. (The latter
  are used internally like GETCEL or PUTCEL.)
- Bugfix in BOX tool.

### 2023.02.02
- Added a small area showing cursor coordinates and color index under cursor.
  (Currently it is the part of Controls, but should separate it, to be visible
  even with hidden Controls.)

### 2023.02.01
- Drawing shapes respects the (F) button.
- Added a nice splash screen. (Turn it off in the .ini [Settings]/ShowSplash=No)

### 2023.01.31
- Added (F) (K) and (A) toggle buttons. (Fill shapes, clear key color and use alpha respectively)
- Cosmetics: Fix in system font in letter K and L. These were only 4 pixel width.

### 2023.01.30
- ESC hides controls and activates CEL picker. Right clicking in any phase will
  return controls and activates previous tool.

### 2023.01.27
- Using TStreamingTexture as DrawArea. It makes the FPS go over 60.
- Added secondary key mapping form zooming.

### 2023.01.26
- Changed vibrocolors.
- Added CEL picker. Currently you have to click on GETCEL button to activate.
- Added clipping for image drawing methods.
- Optimized render to screen.
- Added FPS counter. It seems that in lower zoom levels the fps count falls drastically.

### 2023.01.25
- Quit like in the original. Press Q, and click yes or no, or press Y or N.
- Key mapping added, stored and loaded from the settings.
- You can assign keys to Quit, Yes and No.
- Added keymapping to panning keys, centering and zooming.

### 2023.01.24
- Added tool and ink buttons.
- Clicking them changes the selection.
- Bugfix in TContainer.HandleEvent.
- Bugfix in TMouseObject.HandleEvent.
- Drawing with tools is working now, using the selected ink.

### 2023.01.23
- Cursor drawing optimized. (Removed setting color before every draw by
  calling direct SDL2 procedures instead of MK_SDL2 wrapped ones.)
- Added comment in MKMouse2 about ZIndex. (To be sure.)
- Added Inks.
- Added Tools.
- Added overlay image (not using it yet).
- Added Vibrocolors array.

### 2023.01.20
- Added ZIndex to TMouseObject.
- Added TMouseObjects.Sort to sort objects by ZIndex. (Could be better if Add
  would insert the object to it's proper place)
- DrawArea added.
- DrawArea is rendered to PrimaryWindow directly.
- Panning with right mouse buttons works.
- Zooming with mouse wheel works.
- Panning with arrow or WSAD keys works.
- Cursor added to DrawArea.


### 2023.01.19
- Palette and Image class added.
- Saving temporary image at exit.
- Loading temporary image at startup or creating a new empty one if file not found.
- Comments added on TBDImage class definition.

### 2023.01.17
- Bottom controls panel appears with one clickable button.
- Clicking the button will select or unselect it.

### 2023.01.13
- Restarted with SDL2.
- Window appears with a static infobar.

