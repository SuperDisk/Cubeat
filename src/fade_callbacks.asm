include "defines.asm"

SECTION "Fade callbacks", ROM0

goto_mainmenu::
  xor a
  ld [rLCDC], a

  pop af
  jp MainMenu

goto_titlescreen::
  xor a
  ld [rLCDC], a

  pop af
  jp TitleScreen

goto_levelsmenu::
  xor a
  ld [rLCDC], a

  pop af
  jp LevelsMenu

goto_musicplayermenu::
  xor a
  ld [rLCDC], a

  pop af
  jp MusicPlayerMenu

goto_gameplay::
  xor a
  ld [rLCDC], a

  pop af
  jp Intro