include "defines.asm"

MACRO update_sprite  ; which sprite, x, y, tile
  ld a, \3+16
  ld [wShadowOAM+(4*\1)], a
  ld a, \2+8
  ld [wShadowOAM+(4*\1)+1], a
  ld a, \4
  ld [wShadowOAM+(4*\1)+2], a
ENDM

MACRO alt_palette
  ld a, %00010001
  ld [wShadowOAM+(4*\1)+3], a
ENDM

SECTION "Music Player gfx", ROMX
back_gfx: incbin "res/menu/back_sprite.2bpp"
.end:

SECTION "Music Player", ROM0

MusicPlayer::
  call clear_oam

  ld hl, skin0.border_bank
  call colorize
  call safe_turn_off_lcd

  call reset_opl3

  xor a
  ld [hPressedKeys], a
  ld [current_skin], a

  ld de, static_ram_code
  ld hl, ram_code
  ld c, static_ram_code.end - static_ram_code
  rst MemcpySmall

  ld a, $31 ; ld sp, xxxx
  ld [update_bg_done], a

  ld a, $C3 ; jp xxxx
  ld [update_playfield_buffer], a

  ;; Set transition state
  ld a, 1
  ld [transition_state], a

  ; ld a, $C3 ; JP
  ; ld [draw_block0], a
  ; ld [draw_block1], a

  ld a, [selected_music]
  add a
  ld hl, skin_table
  add_a_to_hl
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  call load_skin

  call init_playfield_buffer

  ;; Setup music player UI stuff
  ld a, BANK(back_gfx)
  ld [rROMB0], a

  ld de, back_gfx
  ld hl, $8000
  ld c, (back_gfx.end - back_gfx)
  rst MemcpySmall

  update_sprite 0, 8+(8*0), 7, 0
  update_sprite 1, 8+(8*1), 7, 1
  update_sprite 2, 8+(8*2), 7, 2
  update_sprite 3, 8+(8*3), 7, 3
  update_sprite 4, 8+(8*0), 7+8, 4
  alt_palette 0
  alt_palette 1
  alt_palette 2
  alt_palette 3
  alt_palette 4

  ld a, HIGH(wShadowOAM)
  call hOAMDMA

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800 | LCDCF_OBJON
  ld [rLCDC], a

  call UnfreezeScreen

  call FadeInit
  ld hl, KnownRet
  call FadeIn

  ;; fallthrough

music_player_loop:
  call poll_joystick

  ld a, [hPressedKeys]
  bit PADB_B, a
  jr z, .no_quit

  ld hl, sfx_ui_back
  call play_sfx

  ld hl, goto_musicplayermenu
  call FadeOut

.no_quit:
  call FadeStep
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Load buffer with new tile data
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [next_map_bank]
  ld [rROMB0], a

  call update_playfield_buffer

  ld a, [rLCDC]
  res 2, a
  ld [rLCDC], a
  ld a, HIGH(wShadowOAM)
  call hOAMDMA

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Execute buffer, loading tile data into unused map
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [current_bg]
  xor %00000100
  ld [current_bg], a
  ld h, a
  ld l, 0

  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt ; wait for VBlank
  nop

  assert IEF_VBLANK + 1 == IEF_STAT
  ld a, IEF_STAT
  ldh [rIE], a
  ld a, STATF_MODE00
  ldh [rSTAT], a ; Careful, this may make the STAT int pending

  call playfield_buffer
  call do_music
  call FadeStep

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Load gfx, and swap map
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [next_gfx_bank]
  ld [rROMB0], a

  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt ; wait for VBlank
  nop

  ; swap map
  ldh a, [rLCDC]
  xor %00001000
  ldh [rLCDC], a

  ld a, IEF_STAT
  ldh [rIE], a
  ld a, STATF_MODE00
  ldh [rSTAT], a ; Careful, this may make the STAT int pending

  call update_bg
  call do_music

  jp music_player_loop