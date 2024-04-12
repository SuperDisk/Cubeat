include "defines.asm"

SECTION "SFX Vars", WRAM0

sfx_pointer: dw
playing_sfx:: db

SECTION "Sound effects", ROMX

sfx_test_explode::            INCBIN "res/sfx/test_explode.sfx"
sfx_test_stereo::             INCBIN "res/sfx/test_stereo.sfx"
sfx_alt_block_land_smooth::   INCBIN "res/sfx/alt_block_land_smooth.sfx"
sfx_alt_combo_swirly::        INCBIN "res/sfx/alt_combo_swirly.sfx"
sfx_alt_game_starts_punch::   INCBIN "res/sfx/alt_game_starts_punch.sfx"
sfx_alt_radar_destroy_clean:: INCBIN "res/sfx/alt_radar_destroy_clean.sfx"
sfx_alt_radar_destroy_echo::  INCBIN "res/sfx/alt_radar_destroy_echo.sfx"
sfx_block_land_crunchy::      INCBIN "res/sfx/block_land_crunchy.sfx"
sfx_combo_clean::             INCBIN "res/sfx/combo_clean.sfx"
; game_over_alt::           INCBIN "res/sfx/game_over_alt.sfx"
; game_starts::             INCBIN "res/sfx/game_starts.sfx"
; move_left::               INCBIN "res/sfx/move_left.sfx"
; move_right::              INCBIN "res/sfx/move_right.sfx"
; radar_destroy_crunch::    INCBIN "res/sfx/radar_destroy_crunch.sfx"
; test_explode::            INCBIN "res/sfx/test_explode.sfx"
; test_stereo::             INCBIN "res/sfx/test_stereo.sfx"
sfx_ui_back::                 INCBIN "res/sfx/ui_back.sfx"
sfx_ui_move::                 INCBIN "res/sfx/ui_move.sfx"


SECTION "SFX Playback", ROM0

play_sfx::
  ld a, l
  ld [sfx_pointer], a
  ld a, h
  ld [sfx_pointer+1], a
  ld [playing_sfx], a ; High address byte will never be 00
  ret

tick_sfx::
  ld a, [playing_sfx]
  or a
  ret z

  ld a, BANK("Sound effects")
  ld [rROMB0], a

  ld hl, sfx_pointer
  ld a, [hl+]
  ld h, [hl]
  ld l, a

  jp hl

done_tick_sfx::
  pop hl

  ld a, [hl]
  inc a ; $FF is the sentinel to quit playing
  jr nz, .continue

  ld [playing_sfx], a ; A = 0
  ld [rNR12], a
  ld [rNR22], a
  ld [rNR30], a
  ld [rNR42], a
  ret

.continue:
  ld a, l
  ld [sfx_pointer], a
  ld a, h
  ld [sfx_pointer+1], a
  ret