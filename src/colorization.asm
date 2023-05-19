include "defines.asm"

SECTION "Colorization functions", ROM0

;;; Changes the colorization of the screen on SGB or CGB.
;;; Chooses the correct system automatically.
;;; HL = pointer to a structure with
;;;  0: bank
;;;  1-2: SGB border tiles
;;;  3-4: SGB border map
;;;  5-6: SGB palette packet
;;;  7-8: CGB palette
colorize::
  ld a, [hConsoleType]
  or a
  jr z, .cgb

  ld a, [hIsSGB]
  or a
  ret z

.sgb:
  ;; bank
  ld a, [hl+]
  ld [rROMB0], a

  ;; tiles
  ld e, [hl]
  inc hl
  ld d, [hl]
  inc hl

  ;; map
  ld c, [hl]
  inc hl
  ld b, [hl]
  inc hl

  ;; palette
  ld a, [hl+]
  ld h, [hl]
  ld l, a

  jp ChangeSGBBorder

.cgb:
  ld de, 5
  add hl, de
  ld a, [hl+]
  ld h, [hl]
  ld l, a

.cgb_atpacket:
  ld de, 9
  add hl, de

  ld de, wBGPaletteBuffer

  ld c, 3*4
.loop:
  ld a, [hl+]
  ld [de], a
  inc de

  dec c
  jr nz, .loop

  ld de, wOBJPaletteBuffer

  ld c, 3*6
.loop2:
  ld a, [hl+]
  ld [de], a
  inc de

  dec c
  jr nz, .loop2

  ret

colorize_noborder::
  ld a, [hConsoleType]
  or a
  jr z, colorize.cgb_atpacket

  ld a, [hIsSGB]
  or a
  ret z

  jp SendPackets