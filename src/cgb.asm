include "defines.asm"

MACRO cgb_palette
  ;; BGP
  dw (\1 & $F8) << 7 | (\1 & $F800) >> 6 | (\1 & $F80000) >> 19
  dw (\2 & $F8) << 7 | (\2 & $F800) >> 6 | (\2 & $F80000) >> 19
  dw (\3 & $F8) << 7 | (\3 & $F800) >> 6 | (\3 & $F80000) >> 19
  dw (\4 & $F8) << 7 | (\4 & $F800) >> 6 | (\4 & $F80000) >> 19

  ;; OBP1
  dw (\2 & $F8) << 7 | (\2 & $F800) >> 6 | (\2 & $F80000) >> 19
  dw (\4 & $F8) << 7 | (\4 & $F800) >> 6 | (\4 & $F80000) >> 19
  dw (\3 & $F8) << 7 | (\3 & $F800) >> 6 | (\3 & $F80000) >> 19
  dw (\1 & $F8) << 7 | (\1 & $F800) >> 6 | (\1 & $F80000) >> 19

  ;; OBP2
  dw (\3 & $F8) << 7 | (\3 & $F800) >> 6 | (\3 & $F80000) >> 19
  dw (\1 & $F8) << 7 | (\1 & $F800) >> 6 | (\1 & $F80000) >> 19
  dw (\2 & $F8) << 7 | (\2 & $F800) >> 6 | (\2 & $F80000) >> 19
  dw (\4 & $F8) << 7 | (\4 & $F800) >> 6 | (\4 & $F80000) >> 19
ENDM

SECTION "CGB routines", ROM0

blah:
  cgb_palette $FF9C00, $00639C, $104A84, $FFFFFF

DoCGBSetup::
  ld a, %10000000
  ld [rBCPS], a

  ld de, rBCPD
  ld hl, blah
  REPT 8
  ld a, [hl+]
  ld [de], a
  ENDR

  ld a, %10000000
  ld [rOCPS], a

  ld de, rOCPD
  REPT 16
  ld a, [hl+]
  ld [de], a
  ENDR

  ret