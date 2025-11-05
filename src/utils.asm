include "defines.asm"

SECTION "Utils", ROM0

wait_vblank::
  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt
  ret

safe_turn_off_lcd::
  ld a, [rLCDC]
  or a
  ret z

  call wait_vblank
  xor a
  ld [rLCDC], a
  ret

clear_oam::
  ld hl, wShadowOAM
  ld c, NB_SPRITES * 4
  xor a
  jp MemsetSmall

init_playfield_buffer::
  ld a, BANK(playfield_buffer_rom)
  ld [rROMB0], a

  ld de, playfield_buffer_rom
  ld hl, playfield_buffer
  ld bc, playfield_buffer_rom.end - playfield_buffer_rom
  jp Memcpy

reset_opl3::
  ld h, $A0

  lb bc, 0, $c0
  ld a, $b0
  call opl3_fill

  lb bc, $ff, $96
  ld a, $80
  ;; fallthrough

opl3_fill:
  ld l, 0

  ld [hl+], a
  ld [hl], b
  inc l

  ld [hl+], a
  ld [hl], b

  inc a
  cp c
  jr nz, opl3_fill
  ret
