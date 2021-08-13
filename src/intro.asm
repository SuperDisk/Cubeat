include "defines.asm"

add_a_to_r16: MACRO
    add \2
    ld \2, a
    adc \1
    sub \2
    ld \1, a
ENDM

;; Thanks PinoBatch!
sub_from_r16: MACRO ;; (high, low, value)
    ld a, \2
    sub \3
    ld \2, a
    sbc a  ; A = -1 if borrow or 0 if not
    add \1
    ld \1, a
ENDM

add_a_to_hl: MACRO
    add_a_to_r16 h, l
ENDM

add_a_to_de: MACRO
    add_a_to_r16 d, e
ENDM

SECTION "shit", WRAM0
anim: db
old_sp: dw
ctr: db

SECTION "Intro", ROMX

Intro::

  ; call WaitVBlank
  ; ld a, 0
  ; ld hl, $9800
  ; rept 413
  ; ld [hl], a
  ; endr

; Turn the LCD off
	ld a, 0
	ld [hLCDC], a

.wait_lcdc_off:
  ld a, [rLCDC]
  and %10000000
  jr nz, .wait_lcdc_off

	; Copy the tile data
	ld de, title
	ld hl, _VRAM
	ld bc, title_end - title
CopyTiles:
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or a, c
	jp nz, CopyTiles

  ld a, 4
  ld [ctr], a

  xor a
  ld [anim], a
  ld [old_sp], sp

  call title_tilemap_initial

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000
	ld [hLCDC], a
	ld [rLCDC], a

Done::
  ld hl, old_sp
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  ld sp, hl

  call WaitVBlank

  ld hl, Done
  push hl

  ld a, [ctr]
  dec a
  jr nz, .no_renew_ctr
  ld a, 4
  ld [ctr], a
  jr .continue
.no_renew_ctr:
  ld [ctr], a
  jr Done
.continue:

  ld hl, anim
  inc [hl]
  ld a, 25
  cp [hl]
  jr nz, .dostuf
  ld [hl], 1
.dostuf:
  ld a, [hl]
  add [hl]
  add [hl]
  sub 3
  ld hl, .stoff
  add_a_to_hl
  jp hl
.stoff:
  jp title_tilemap0
  jp title_tilemap1
  jp title_tilemap2
  jp title_tilemap3
  jp title_tilemap4
  jp title_tilemap5
  jp title_tilemap6
  jp title_tilemap7
  jp title_tilemap8
  jp title_tilemap9
  jp title_tilemap10
  jp title_tilemap11
  jp title_tilemap12
  jp title_tilemap13
  jp title_tilemap14
  jp title_tilemap15
  jp title_tilemap16
  jp title_tilemap17
  jp title_tilemap18
  jp title_tilemap19
  jp title_tilemap20
  jp title_tilemap21
  jp title_tilemap22
  jp title_tilemap23
  jp title_tilemap24
  jp title_tilemap25
  jp Done

title:
; incbin "res/title.2bpp"
incbin "res/temp-bytes"
title_end:
title_tilemap_initial:
incbin "res/temp-tilemap-code-initial"
title_tilemap0:
incbin "res/temp-tilemap-code0"
title_tilemap1:
incbin "res/temp-tilemap-code1"
title_tilemap2:
incbin "res/temp-tilemap-code2"
title_tilemap3:
incbin "res/temp-tilemap-code3"
title_tilemap4:
incbin "res/temp-tilemap-code4"
title_tilemap5:
incbin "res/temp-tilemap-code5"
title_tilemap6:
incbin "res/temp-tilemap-code6"
title_tilemap7:
incbin "res/temp-tilemap-code7"
title_tilemap8:
incbin "res/temp-tilemap-code8"
title_tilemap9:
incbin "res/temp-tilemap-code9"
title_tilemap10:
incbin "res/temp-tilemap-code10"
title_tilemap11:
incbin "res/temp-tilemap-code11"
title_tilemap12:
incbin "res/temp-tilemap-code12"
title_tilemap13:
incbin "res/temp-tilemap-code13"
title_tilemap14:
incbin "res/temp-tilemap-code14"
title_tilemap15:
incbin "res/temp-tilemap-code15"
title_tilemap16:
incbin "res/temp-tilemap-code16"
title_tilemap17:
incbin "res/temp-tilemap-code17"
title_tilemap18:
incbin "res/temp-tilemap-code18"
title_tilemap19:
incbin "res/temp-tilemap-code19"
title_tilemap20:
incbin "res/temp-tilemap-code20"
title_tilemap21:
incbin "res/temp-tilemap-code21"
title_tilemap22:
incbin "res/temp-tilemap-code22"
title_tilemap23:
incbin "res/temp-tilemap-code23"
title_tilemap24:
incbin "res/temp-tilemap-code24"
title_tilemap25:
incbin "res/temp-tilemap-code25"
title_tilemap_end:
