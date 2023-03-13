include "defines.asm"

include "res/menu/title_screen.menu.asm"

SECTION "Title Screen", ROM0

TitleScreen::
  ld de, playfield_buffer_rom
  ld hl, playfield_buffer
  ld bc, playfield_buffer_rom.end - playfield_buffer_rom
  call Memcpy

  ld de, static_ram_code
  ld hl, ram_code
  ld c, static_ram_code.end - static_ram_code
  rst MemcpySmall

  ld a, $31 ; ld sp, xxxx
  ld [update_bg_done], a

  ld a, $C3 ; jp xxxx
  ld [update_playfield_buffer], a

  ;; Copy initial tile data
  ld a, BANK(title_screen_gfx_init)
  ld [rROMB0], a

  ld a, LOW(title_screen_gfx_init)
  ld [ptr_next_update_bg], a
  ld a, HIGH(title_screen_gfx_init)
  ld [ptr_next_update_bg+1], a
  call update_bg

  ld a, BANK(title_screen_map_init)
  ld [next_map_bank], a
  ld a, LOW(title_screen_map_init)
  ld [update_playfield_buffer+1], a
  ld a, HIGH(title_screen_map_init)
  ld [update_playfield_buffer+2], a

  ld a, [next_map_bank]
  ld [rROMB0], a
  call update_playfield_buffer

  ;; Clear sprites
  ld hl, wShadowOAM
  ld c, NB_SPRITES * 4
  xor a
  rst MemsetSmall
  ld a, h ; ld a, HIGH(wShadowOAM)
  call hOAMDMA

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800 | LCDCF_OBJON
  ld [rLCDC], a

  ; call FadeIn
ohyeah:

  ld a, 8 ;$80
  ld [wFadeSteps], a
  ld a, 16
  ld [wFadeDelta], a
  xor a
  ld [wFadeAmount], a

  ld a, %10000000
  ld [wBGPaletteMask], a
  xor a
  ld [wOBJPaletteMask], a

  ld de, wBGPaletteBuffer
  ld hl, pal0+9
  REPT 4*3
  ld a, [hl+]
  ld [de], a
  inc de
  ENDR

  ld a, IEF_VBLANK
  ldh [rIE], a

yeah:
  xor a
  ld [rIF], a
  halt
  call FadePaletteBuffers

  ld a, [wFadeSteps]
  or a
  jr nz, yeah

  ld a, 8 ;$80
  ld [wFadeSteps], a
  ld a, -16
  ld [wFadeDelta], a

  ld a, IEF_VBLANK
  ldh [rIE], a

REPT 60
  xor a
  ld [rIF], a
  halt
ENDR

yeah2:
  xor a
  ld [rIF], a
  halt
  call FadePaletteBuffers

  ld a, [wFadeSteps]
  or a
  jr nz, yeah2
REPT 60
  xor a
  ld [rIF], a
  halt
ENDR

  jp ohyeah

title_loop:
  ; Wait 2 VBlanks
  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt
  xor a
  ld [rIF], a
  halt

  ld a, HIGH(wShadowOAM)
  call hOAMDMA

  ld a, [next_map_bank]
  ld [rROMB0], a
  call update_playfield_buffer

  call poll_joystick
  ld a, [hPressedKeys]
  bit PADB_START, a
  jr z, title_loop

  call FadeOut
  xor a
  ld [rLCDC], a

  jp MainMenu