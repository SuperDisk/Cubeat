include "defines.asm"

SECTION "SFX Vars", WRAM0

sfx_pointer: dw
playing_sfx:: db

SECTION "Sound effects", ROMX
test_explode:: INCBIN "res/sfx/test_explode.sfx"
test_stereo:: INCBIN "res/sfx/test_stereo.sfx"
test_radar:: INCBIN "res/sfx/radar.sfx"

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
  ret

.continue:
  ld a, l
  ld [sfx_pointer], a
  ld a, h
  ld [sfx_pointer+1], a
  ret