include "defines.asm"

SECTION "Utils", ROM0

wait_vblank::
  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt
  ret