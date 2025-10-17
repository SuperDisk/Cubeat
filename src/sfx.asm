include "defines.asm"

MACRO defsfx
 db \1
 INCBIN \2
ENDM

SECTION "SFX Vars", WRAM0

sfx_pointer: dw
playing_sfx:: db
sfx_priority:: db

SECTION "Sound effects", ROMX

sfx_test_explode::            defsfx 6, "res/sfx/test_explode.sfx"
; sfx_test_stereo::             defsfx 1, "res/sfx/test_stereo.sfx"
; sfx_alt_block_land_smooth::   defsfx 1, "res/sfx/alt_block_land_smooth.sfx"
; sfx_alt_combo_swirly::        defsfx 1, "res/sfx/alt_combo_swirly.sfx"
; sfx_alt_game_starts_punch::   defsfx 1, "res/sfx/alt_game_starts_punch.sfx"
; sfx_alt_radar_destroy_clean:: defsfx 1, "res/sfx/alt_radar_destroy_clean.sfx"
sfx_alt_radar_destroy_echo::  defsfx 5, "res/sfx/alt_radar_destroy_echo.sfx"
sfx_block_land_crunchy::      defsfx 1, "res/sfx/block_land_crunchy.sfx"
; sfx_combo_clean::             defsfx 1, "res/sfx/combo_clean.sfx"
; game_over_alt::               defsfx 1, "res/sfx/game_over_alt.sfx"
; game_starts::                 defsfx 1, "res/sfx/game_starts.sfx"
sfx_move_left::               defsfx 1, "res/sfx/move_left.sfx"
sfx_move_right::              defsfx 1, "res/sfx/move_right.sfx"
; radar_destroy_crunch::        defsfx 1, "res/sfx/radar_destroy_crunch.sfx"
sfx_ui_back::                 defsfx 1, "res/sfx/ui_back.sfx"
sfx_ui_move::                 defsfx 1, "res/sfx/ui_move.sfx"

SECTION "SFX Playback", ROM0

play_sfx::
  ret
  ld a, [wFading]
  or a
  ret nz

  ld a, BANK("Sound effects")
  ld [rROMB0], a

  ld a, [sfx_priority]
  cp [hl]
  jr z, .ok
  ret nc

.ok:
  ld a, [hl+]
  ld [sfx_priority], a

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
  ld [sfx_priority], a
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