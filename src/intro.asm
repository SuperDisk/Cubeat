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
jump_next_frame: ds 3
next_frame_bank: db
original_sp: dw

include "res/code"

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
  ld a, BANK(frame_initial)
  ld [rROMB0], a
  ld hl, frame_initial
  call call_save_sp

  ;; Store JP opcode in the pointers
  ld a, $C3 ; jp
  ld [jump_next_frame], a

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000
	ld [hLCDC], a
	ld [rLCDC], a

animation_loop:
  ld a, [next_frame_bank]
  ld [rROMB0], a

  call WaitVBlank
  call WaitVBlank
  call WaitVBlank
  di

  ld hl, jump_next_frame
  call call_save_sp
  ;; Code to update graphics returns with RETI so interrupts are enabled.

  jp animation_loop

call_save_sp:
  ld [original_sp], sp
  jp hl