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

SECTION "Animation vars", WRAM0
jump_next_frame_map: ds 3
jump_next_frame_gfx: ds 3
original_sp: dw
frame_counter: db

SECTION "Animation map", ROMX
include "res/map-code"

SECTION "Animation gfx", ROMX
include "res/graphics-code"

SECTION "Intro", ROM0

Intro::
  ; Turn the LCD off
	ld a, 0
	ld [hLCDC], a

.wait_lcdc_off:
  ld a, [rLCDC]
  and %10000000
  jr nz, .wait_lcdc_off

  ;; Copy initial tile data
  ld a, BANK(map_initial)
  ld [rROMB0], a
  call map_initial

  ;; Copy initial graphics data
  ld a, BANK(gfx_initial)
  ld [rROMB0], a
  ld hl, gfx_initial
  call call_save_sp

  ;; Store JP opcode in the pointers
  ld a, $C3 ; jp
  ld [jump_next_frame_map], a
  ld [jump_next_frame_gfx], a

  ld a, 4
  ld [frame_counter], a

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000
	ld [hLCDC], a
	ld [rLCDC], a

animation_loop:
  ld a, BANK(map_initial)
  ld [rROMB0], a

  call WaitVBlank
  di


  call jump_next_frame_map

  ld a, BANK(gfx_initial)
  ld [rROMB0], a
  ld hl, jump_next_frame_gfx
  call call_save_sp
  ;; Code to update graphics returns with RETI so interrupts are enabled.

  jp animation_loop

call_save_sp:
  ld [original_sp], sp
  jp hl