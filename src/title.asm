include "defines.asm"

include "res/menu/title_screen.menu.asm"

SECTION "Title Screen", ROM0

TitleScreen::
  ;; Load colors to fade buffers
  ld hl, skin_title
  call colorize
  call safe_turn_off_lcd

  call init_playfield_buffer

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

  call clear_oam

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800 | LCDCF_OBJON
  ld [rLCDC], a

  call UnfreezeScreen

  call FadeInit
  ld hl, KnownRet
  call FadeIn

title_loop:
  ; First frame don't do anything since bgs run at 30hz
  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt

  call FadeStep
  call tick_sfx

  ;; Second frame
  xor a
  ld [rIF], a
  halt

  ld a, HIGH(wShadowOAM)
  call hOAMDMA

  ld a, [next_map_bank]
  ld [rROMB0], a
  call update_playfield_buffer

  call FadeStep
  call tick_sfx

  call poll_joystick
  ld a, [hPressedKeys]
  bit PADB_START, a
  jr z, title_loop

  ld hl, goto_mainmenu_with_sgb
  call FadeOut

  ld hl, sfx_alt_radar_destroy_echo
  call play_sfx

  jr title_loop
