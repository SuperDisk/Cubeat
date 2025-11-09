include "defines.asm"

SECTION "Fade callbacks", ROM0

goto_mainmenu::
  pop af
  scf
  jp MainMenu

goto_mainmenu_with_sgb::
  xor a
  ld [rLCDC], a

  pop af
  or a
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

goto_creditsmenu::
  xor a
  ld [rLCDC], a

  pop af
  jp CreditsMenu

goto_musicplayermenu::
  call wait_vblank

  xor a
  ld [rLCDC], a

  pop af
  jp MusicPlayerMenu

goto_musicplayermenu_nosgb::
  call wait_vblank

  xor a
  ld [rLCDC], a

  pop af
  jp MusicPlayerMenu_nosgb

goto_gameplay::
  xor a
  ld [rLCDC], a

  pop af
  jp Kernel

goto_gameplay_restore::
  xor a
  ld [rLCDC], a

  pop af
  jp Kernel

goto_musicplayer::
  xor a
  ld [rLCDC], a

  pop af
  jp MusicPlayer