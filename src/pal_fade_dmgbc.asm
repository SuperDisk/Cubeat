;; Original by ISSOtm, modified by Evie
;; https://github.com/eievui5/esprit/blob/main/src/libs/pal_fade_dmgbc.asm

include "defines.asm"

section "Fading", rom0

FadeInit::
  ld a, $FF
  ld [wFadeAmount], a
  xor a
  ld [wFading], a
  ld [wFadeSteps], a

  ld a, %10000000
  ld [wBGPaletteMask], a
  ld a, %11000000
  ld [wOBJPaletteMask], a

  ld de, MosaicPacket
  ld hl, mosaic_packet_ram
  ld c, 6
  rst MemcpySmall

  ret

FadeOut::
  ld a, [wFading]
  or a
  ret nz

  ld a, 8 ;$80
  ld [wFadeSteps], a
  ld a, 16
  ld [wFadeDelta], a
  jr FadeIn.load_callback

FadeIn::
  ld a, [wFading]
  or a
  ret nz

  ld a, 8 ;$80
  ld [wFadeSteps], a
  ld a, -16
  ld [wFadeDelta], a

.load_callback:
  ld a, l
  ld [wFadeCallback], a
  ld a, h
  ld [wFadeCallback+1], a
  ld a, 1
  ld [wFading], a
KnownRet::
  ret

FadeStep::
  ld a, [wFading]
  or a
  ret z

  ld a, [wFadeSteps]
  or a
  jr nz, FadePaletteBuffers

  xor a
  ld [wFading], a
  inc a ; ld a, 1
  ld [wFadeDelta], a
  call FadePaletteBuffers

  ld hl, wFadeCallback
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  jp hl

FadeDMGToWhite:
  ld c, low(rBGP)
  ld hl, wBGP
.fadeDMGPalToWhite
  ld a, [hli]
  ld b, 4
.fadeDMGColorToWhite
  ld e, a
  and $C0
  sub d
  jr nc, .DMGNotWhite
  xor a
.DMGNotWhite
  xor e
  and $C0
  xor e
  rlca
  rlca
  dec b
  jr nz, .fadeDMGColorToWhite
  ldh [c], a
  ld a, [wOBJPaletteMask]
  and a, a
  ret z
  ld a, c
  inc c
  cp low(rOBP1)
  jr nz, .fadeDMGPalToWhite
  ret

MosaicSGB::
  ld a, [wFadeAmount]
  and %01111000
  add a
  or $0F
  ld [mosaic_packet_ram+5], a
  ld hl, mosaic_packet_ram
  jp SendPacketNoDelay

FadePaletteBuffers:: ;; --------- ENTRY POINT ------
  ld hl, wFadeSteps
  dec [hl]
  inc hl
  assert wFadeSteps + 1 == wFadeDelta

  ld a, [hli]
  ld e, a
  assert wFadeDelta + 1 == wFadeAmount
  add a, [hl]
  jr z, .clamp ; 0 is an illegal value
  ld d, a
  rra ; Get carry into bit 7
  xor e ; Expect bit 7 of offset to match carry
  add a, a
  ld a, d
  jr nc, .noOverflow
.clamp
  ; If we got an overflow, clamp depending on bit 7 of offset
  ; If the offset is positive, clamp at $FF; otherwise, at 1
  sla e ; Bit 7 (sign) into carry
  sbc a, a ; 0   or $FF
  and 2    ; 0   or 2
  dec a    ; $FF or 1
.noOverflow
  ld [hli], a
  assert wFadeAmount + 1 == wBGPaletteMask
  add a, a ; Test sign bit
  ld c, low(rBCPS)
  jr nc, .fadeToBlack

  ld d, a
  ; ld a, [hIsSGB]
  ; or a
  ; jr nz, MosaicSGB
  ld a, [hConsoleType]
  and a
  jr nz, FadeDMGToWhite
.fadeBufferToWhite
  ld a, $80
  ldh [c], a
  inc c
  ld a, [hli] ; Read palette mask
  scf
  adc a, a
.fadePaletteToWhite
  ldh [hPaletteMask], a
  jr nc, .noWhiteFade
  ld b, 4
  ld a, c
  cp low(rOCPD)
  jr nz, .fadeColorToWhite
  dec b
  ; Do two dummy writes to advance index
  ; The index is increased even if the writes fail
  ldh [c], a
  ldh [c], a
.fadeColorToWhite
  ld a, [hli] ; Read green
  add a, d
  jr nc, .notFullGreen
  sbc a, a ; ld a, $FF
.notFullGreen
  rlca
  rlca
  ld e, a
  wait_vram
  ld a, [hli] ; Read red
  add a, d
  jr nc, .notFullRed
  sbc a, a ; ld a, $FF
.notFullRed
  rra
  rra
  rra
  xor e
  and $1F
  xor e
  ldh [c], a
  wait_vram
  ld a, [hli] ; Read blue
  add a, d
  jr nc, .notFullBlue
  sbc a, a ; ld a, $FF
.notFullBlue
  rra
  xor e
  and $FC ; $7C works just as well, but $FC gets bit 7 always cleared
  xor e
  ldh [c], a
  dec b
  jr nz, .fadeColorToWhite
.fadedPaletteWhite
  ldh a, [hPaletteMask]
  add a, a
  jr nz, .fadePaletteToWhite
  inc c
  ld a, c
  cp low(rOCPS)
  jr z, .fadeBufferToWhite
  ret

.noWhiteFade
  dec c
  ldh a, [c]
  add a, 4 * 2
  ldh [c], a
  inc c
  ld a, l
  add a, 4 * 3
  ld l, a
  adc a, h
  sub l
  ld h, a
  jr .fadedPaletteWhite

.fadeToBlack
  cpl
  inc a
  ld d, a
  ldh a, [hConsoleType]
  and a
  jr nz, .fadeDMGToBlack
.fadeBufferToBlack
  ld a, $80
  ldh [c], a
  inc c
  ld a, [hli] ; Read palette mask
  scf
  adc a, a
.fadePaletteToBlack
  ldh [hPaletteMask], a
  jr nc, .noBlackFade
  ld b, 4
  ; OBJ palettes only have 3 colors
  ld a, c
  cp low(rOCPD)
  jr nz, .fadeColorToBlack
  dec b
  ; Do two dummy writes to advance index
  ; The index is increased even if the writes fail
  ldh [c], a
  ldh [c], a
.fadeColorToBlack
  ld a, [hli] ; Read green
  sub d
  jr nc, .stillSomeGreen
  xor a
.stillSomeGreen
  rlca
  rlca
  ld e, a
  wait_vram
  ld a, [hli] ; Read red
  sub d
  jr nc, .stillSomeRed
  xor a
.stillSomeRed
  rra
  rra
  rra
  xor e
  and $1F
  xor e
  ldh [c], a
  wait_vram
  ld a, [hli] ; Read blue
  sub d
  jr nc, .stillSomeBlue
  xor a
.stillSomeBlue
  rra
  xor e
  and $FC ; $7C works just as well, but $FC gets bit 7 always cleared
  xor e
  ldh [c], a
  dec b
  jr nz, .fadeColorToBlack
.fadedPaletteBlack
  ldh a, [hPaletteMask]
  add a, a
  jr nz, .fadePaletteToBlack
  inc c
  ld a, c
  cp low(rOCPS)
  jr z, .fadeBufferToBlack
  ret

.noBlackFade
  dec c
  ldh a, [c]
  add a, 4 * 2
  ldh [c], a
  inc c
  ld a, l
  add a, 4 * 3
  ld l, a
  adc a, h
  sub l
  ld h, a
  jr .fadedPaletteBlack

.fadeDMGToBlack
  ld c, low(rBGP)
  ld hl, wBGP
.fadeDMGPalToBlack
  ld a, [hli]
  ld b, 4
.fadeDMGColorToBlack
  ld e, a
  and $C0
  add a, d
  jr nc, .DMGNotBlack
  sbc a, a ; ld a, $FF
.DMGNotBlack
  xor e
  and $C0
  xor e
  rlca
  rlca
  dec b
  jr nz, .fadeDMGColorToBlack
  ldh [c], a
  ld a, [wOBJPaletteMask]
  and a, a
  ret z
  ld a, c
  inc c
  cp low(rOBP1)
  jr nz, .fadeDMGPalToBlack
  ret

PUSHS
section UNION "Scratch buffer", hram

hPaletteMask: db

section "Fade state memory", wram0
wFadeSteps:: db ; Number of fade steps to take
wFadeDelta:: db ; Value to add to wFadeAmount on each step

; 00    = bugged equivalent of 00, do not use
; 01-7F = 01 is fully black, 7F is barely faded
; 80    = not faded
; 81-FF = FF is fully white, 81 is barely faded
wFadeAmount:: db

wBGPaletteMask:: db ; Mask of which palettes to fade (01234567)
wBGPaletteBuffer:: ; 24-bit GRB, in this order
  ds 8 * 4 * 3 ; 8 palettes, 4 colors, 3 bytes
wOBJPaletteMask:: db
wOBJPaletteBuffer:: ; Same as above
  ds 8 * 3 * 3 ; Same, but only 3 colors

wBGP:: db
wOBP0:: db
wOBP1:: db

wFadeCallback:: dw
wFading:: db

mosaic_packet_ram: ds 6