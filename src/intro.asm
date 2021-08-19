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
next_frame_bank: db


SECTION "Static RAM Code", ROM0
static_ram_code:
  xor a
  ldh [rIF], a
  assert IEF_VBLANK == 1
  inc a ; ld a, IEF_VBLANK
  ldh [rIE], a
  reti
.update_bg:
  ld [orig_sp], sp
  db $C3 ; jp xxxx
.end:

SECTION "RAM Code", WRAM0
update_bg_done: ds 1 ; ld sp, xxxx
orig_sp: ds 2 ; operand of above
ram_code:
  ds static_ram_code.update_bg - static_ram_code
update_bg:
  ds static_ram_code.end - static_ram_code.update_bg
ptr_next_update_bg:
  ds 2 ; operand of above

include "res/backgrounds/bg01.asm"
include "res/backgrounds/bg02.asm"
include "res/backgrounds/bg03.asm"
include "res/backgrounds/bg04.asm"
include "res/backgrounds/bg05.asm"
include "res/backgrounds/bg06.asm"
include "res/backgrounds/bg07.asm"
include "res/backgrounds/bg08.asm"
include "res/backgrounds/bg9.asm"
include "res/backgrounds/bg10.asm"
include "res/backgrounds/bg11.asm"
include "res/backgrounds/bg12.asm"
include "res/backgrounds/bg13.asm"
include "res/backgrounds/bg014.asm"
include "res/backgrounds/bg15.asm"
include "res/backgrounds/bg18.asm"
include "res/backgrounds/bg19.asm"
include "res/backgrounds/bg20.asm"
include "res/backgrounds/splash_screen.asm"

BRERB EQUS "bg12_init"

SECTION "Intro", ROM0

Intro::
  ; Turn the LCD off
	ld a, 0
	ld [hLCDC], a

  ld de, static_ram_code
  ld hl, ram_code
  ld c, static_ram_code.end - static_ram_code
  rst MemcpySmall

  ld a, $31 ; ld sp, xxxx
  ld [update_bg_done], a

.wait_lcdc_off:
  ld a, [rLCDC]
  and %10000000
  jr nz, .wait_lcdc_off

  ;; Copy initial tile data
  ld a, BANK(BRERB)
  ld [rROMB0], a

  ld a, LOW(BRERB)
  ld [ptr_next_update_bg], a
  ld a, HIGH(BRERB)
  ld [ptr_next_update_bg+1], a
  call update_bg

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800
	ld [hLCDC], a
	ld [rLCDC], a

animation_loop:
  call WaitVBlank
  ld a, [next_frame_bank]
  ld [rROMB0], a

  di

  ld a, IEF_VBLANK
  ldh [rIE], a
  halt ; wait for VBlank

  assert IEF_VBLANK + 1 == IEF_STAT
  inc a ; ld a, IEF_STAT
  ldh [rIE], a
  ld a, STATF_MODE00
  ldh [rSTAT], a ; Careful, this may make the STAT int pending

  ; TODO: do OAM DMA
  call update_bg
  ;; Code to update graphics returns with RETI so interrupts are enabled.

  jp animation_loop