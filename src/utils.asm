include "defines.asm"

SECTION "Utils", ROM0

DEF BLACK = %11
DEF WHITE = %00
DEF LIGHT = %01
DEF DARK = %10

MACRO BGColor
  db \1 | (\2 << 2) | (\3 << 4) | (\4 << 6)
ENDM

fade_out_colors:
  BGColor DARK, WHITE, LIGHT, WHITE
  BGColor WHITE, WHITE, LIGHT, DARK
  BGColor LIGHT, DARK, WHITE, WHITE

  BGColor LIGHT, WHITE, WHITE, WHITE
  BGColor WHITE, WHITE, WHITE, LIGHT
  BGColor WHITE, LIGHT, WHITE, WHITE

  BGColor WHITE, WHITE, WHITE, WHITE
  BGColor WHITE, WHITE, WHITE, WHITE
  BGColor WHITE, WHITE, WHITE, WHITE

fade_in_colors:
  BGColor LIGHT, WHITE, WHITE, WHITE
  BGColor WHITE, WHITE, WHITE, LIGHT
  BGColor WHITE, LIGHT, WHITE, WHITE

  BGColor DARK, WHITE, LIGHT, WHITE
  BGColor WHITE, WHITE, LIGHT, DARK
  BGColor LIGHT, DARK, WHITE, WHITE

  BGColor BLACK, LIGHT, DARK, WHITE
  BGColor LIGHT, WHITE, DARK, BLACK
  BGColor DARK, BLACK, LIGHT, WHITE

wait_vblank::
  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt
  ret

FadeIn::
  ld hl, fade_in_colors
  jr Fade

FadeOut::
  ld hl, fade_out_colors

Fade:
  ld a, [hl+]
  ld [rBGP], a
  ld a, [hl+]
  ld [rOBP0], a
  ld a, [hl+]
  ld [rOBP1], a

  call wait_vblank
  call wait_vblank
  call wait_vblank

  ld a, [hl+]
  ld [rBGP], a
  ld a, [hl+]
  ld [rOBP0], a
  ld a, [hl+]
  ld [rOBP1], a

  call wait_vblank
  call wait_vblank
  call wait_vblank

  ld a, [hl+]
  ld [rBGP], a
  ld a, [hl+]
  ld [rOBP0], a
  ld a, [hl+]
  ld [rOBP1], a

  call wait_vblank
  call wait_vblank
  call wait_vblank

  ret