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
  rst MemsetSmall
  ld a, h ; ld a, HIGH(wShadowOAM)
  jp hOAMDMA