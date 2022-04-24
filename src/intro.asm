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

SECTION "Game vars", WRAM0
drop_pos: db
frame_counter: db

SECTION "Animation vars", WRAM0
viewing_animation: db

current_bg: db
next_gfx_bank: db
next_map_bank: db
update_playfield_buffer: ds 3 ; includes jump opcode

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

;; These define their own sections
include "res/backgrounds/bg01.asm"
include "res/backgrounds/bg02.asm"
include "res/backgrounds/bg03.asm"
include "res/backgrounds/bg04.asm"
include "res/backgrounds/bg05.asm"
include "res/backgrounds/bg06.asm"
include "res/backgrounds/bg07.asm"
include "res/backgrounds/bg08.asm"
include "res/backgrounds/bg09.asm"
include "res/backgrounds/bg10.asm"
include "res/backgrounds/bg11.asm"
include "res/backgrounds/bg12.asm"
include "res/backgrounds/bg13.asm"
include "res/backgrounds/bg14.asm"
include "res/backgrounds/bg15.asm"
include "res/backgrounds/bg16.asm"
include "res/backgrounds/bg17.asm"
include "res/backgrounds/bg18.asm"
include "res/backgrounds/bg19.asm"
include "res/backgrounds/bg20.asm"
include "res/backgrounds/bg21.asm"
include "res/backgrounds/bg22.asm"
include "res/backgrounds/bg23.asm"
include "res/backgrounds/bg24.asm"
include "res/backgrounds/bg25.asm"

include "res/backgrounds/splash_screen.asm"

BRERB EQUS "splash_screen_gfx_init"
BRERB2 EQUS "splash_screen_map0"

CLORB EQUS "bg16_gfx_init"
CLORB2 EQUS "bg16_map0"

SECTION "shit", ROM0
cool_bgs:
db BANK(splash_screen_gfx_init)
dw splash_screen_gfx_init
db BANK(splash_screen_map0)
dw splash_screen_map0
db BANK(bg01_gfx_init)
dw bg01_gfx_init
db BANK(bg01_map0)
dw bg01_map0
db BANK(bg02_gfx_init)
dw bg02_gfx_init
db BANK(bg02_map0)
dw bg02_map0
db BANK(bg03_gfx_init)
dw bg03_gfx_init
db BANK(bg03_map0)
dw bg03_map0
db BANK(bg04_gfx_init)
dw bg04_gfx_init
db BANK(bg04_map0)
dw bg04_map0
db BANK(bg05_gfx_init)
dw bg05_gfx_init
db BANK(bg05_map0)
dw bg05_map0
db BANK(bg06_gfx_init)
dw bg06_gfx_init
db BANK(bg06_map0)
dw bg06_map0
db BANK(bg07_gfx_init)
dw bg07_gfx_init
db BANK(bg07_map0)
dw bg07_map0
db BANK(bg08_gfx_init)
dw bg08_gfx_init
db BANK(bg08_map0)
dw bg08_map0
db BANK(bg09_gfx_init)
dw bg09_gfx_init
db BANK(bg09_map0)
dw bg09_map0
db BANK(bg10_gfx_init)
dw bg10_gfx_init
db BANK(bg10_map0)
dw bg10_map0
db BANK(bg11_gfx_init)
dw bg11_gfx_init
db BANK(bg11_map0)
dw bg11_map0
db BANK(bg12_gfx_init)
dw bg12_gfx_init
db BANK(bg12_map0)
dw bg12_map0
db BANK(bg13_gfx_init)
dw bg13_gfx_init
db BANK(bg13_map0)
dw bg13_map0
db BANK(bg14_gfx_init)
dw bg14_gfx_init
db BANK(bg14_map0)
dw bg14_map0
db BANK(bg15_gfx_init)
dw bg15_gfx_init
db BANK(bg15_map0)
dw bg15_map0
db BANK(bg16_gfx_init)
dw bg16_gfx_init
db BANK(bg16_map0)
dw bg16_map0
db BANK(bg17_gfx_init)
dw bg17_gfx_init
db BANK(bg17_map0)
dw bg17_map0
db BANK(bg18_gfx_init)
dw bg18_gfx_init
db BANK(bg18_map0)
dw bg18_map0
db BANK(bg19_gfx_init)
dw bg19_gfx_init
db BANK(bg19_map0)
dw bg19_map0
db BANK(bg20_gfx_init)
dw bg20_gfx_init
db BANK(bg20_map0)
dw bg20_map0
db BANK(bg21_gfx_init)
dw bg21_gfx_init
db BANK(bg21_map0)
dw bg21_map0
db BANK(bg22_gfx_init)
dw bg22_gfx_init
db BANK(bg22_map0)
dw bg22_map0
db BANK(bg23_gfx_init)
dw bg23_gfx_init
db BANK(bg23_map0)
dw bg23_map0
db BANK(bg24_gfx_init)
dw bg24_gfx_init
db BANK(bg24_map0)
dw bg24_map0
db BANK(bg25_gfx_init)
dw bg25_gfx_init
db BANK(bg25_map0)
dw bg25_map0

SECTION "Sprite Graphics", ROMX
all_graphics:
incbin "res/leveltimescore.2bpp"
incbin "res/numbers_big_8x8.2bpp"
incbin "res/numbers_8x8_only.2bpp"
incbin "res/numbers_8x8_only.2bpp"
incbin "res/radar.2bpp"
incbin "res/pice_fall_highlight.2bpp"
all_graphics_end:

SECTION "Background Graphics", ROMX

SECTION "Playfield Buffer ROM", ROM0

playfield_buffer_rom:
include "playfield_buffer.inc"
.end:

SECTION "Playfield Buffer RAM", WRAM0
playfield_buffer:
ds (playfield_buffer_rom.end - playfield_buffer_rom)

SECTION "Intro", ROM0

Intro::
  ld a, $80
  ld [rAUDENA], a
  ld a, $FF
  ld [rAUDTERM], a
  ld a, $77
  ld [rAUDVOL], a

  ; Turn the LCD off
	ld a, 0
	ld [hLCDC], a

  ld de, static_ram_code
  ld hl, ram_code
  ld c, static_ram_code.end - static_ram_code
  rst MemcpySmall

  ld a, $31 ; ld sp, xxxx
  ld [update_bg_done], a

  ld a, $C3 ; jp xxxx
  ld [update_playfield_buffer], a

  ld a, $98
  ld [current_bg], a

  xor a
  ld [viewing_animation], a

.wait_lcdc_off:
  ld a, [rLCDC]
  and %10000000
  jr nz, .wait_lcdc_off

  ;; Copy sprite graphics
  ld a, BANK(all_graphics)
  ld [rROMB0], a
  ld de, all_graphics
  ld hl, $8000
  ld bc, (all_graphics_end - all_graphics)
  call Memcpy


  ld de, playfield_buffer_rom
  ld hl, playfield_buffer
  ld bc, (playfield_buffer_rom.end - playfield_buffer_rom)
  call Memcpy

  ld a, 16
  ld [wShadowOAM], a
  ld a, 8
  ld [wShadowOAM+1], a

update_sprite: macro ; which sprite, x, y, tile
  ld a, \3+16
  ld [wShadowOAM+(4*\1)], a
  ld a, \2+8
  ld [wShadowOAM+(4*\1)+1], a
  ld a, \4
  ld [wShadowOAM+(4*\1)+2], a
endm

  ; lvl
  update_sprite 0, 2+(8*0), 4, 0
  update_sprite 1, 2+(8*1), 4, 1

  ; 01
  update_sprite 9, 19+(8*0), 2, 8
  ; update_sprite 10, 18+(8*1), 2, 9

  ; time
  update_sprite 2, 70+(8*0), 4, 2
  update_sprite 3, 70+(8*1), 4, 3
  update_sprite 4, 70+(8*2), 4, 4

  ; score
  update_sprite 5, 125+(8*0), 4, 5
  update_sprite 6, 125+(8*1), 4, 6
  update_sprite 7, 125+(8*2), 4, 7
  update_sprite 8, 125+(8*3), 4, 4

  ; radar
  update_sprite 10, 124+(8*0), 32, $30
  update_sprite 12, 124+(8*2), 32, $31
  update_sprite 11, 124+(8*1), 32, $26

  ; radar "stem"
  update_sprite 13, 124+(8*1), 32+(8*0), $32
  update_sprite 14, 124+(8*1), 32+(8*1), $32
  update_sprite 15, 124+(8*1), 32+(8*2), $32
  update_sprite 16, 124+(8*1), 32+(8*3), $32
  update_sprite 17, 124+(8*1), 32+(8*4), $32
  update_sprite 18, 124+(8*1), 32+(8*5), $32
  update_sprite 19, 124+(8*1), 32+(8*6), $32
  update_sprite 20, 124+(8*1), 32+(8*7), $32
  update_sprite 21, 124+(8*1), 32+(8*8), $32
  update_sprite 22, 124+(8*1), 32+(8*9), $32
  update_sprite 23, 124+(8*1), 32+(8*10), $32
  update_sprite 24, 124+(8*1), 32+(8*11), $32
  update_sprite 25, 124+(8*1), 32+(8*12), $32

  ; Pice fall highlight
  update_sprite 26, 64, 47+(8*0), $33
  update_sprite 27, 64, 47+(8*1), $33
  update_sprite 28, 64, 47+(8*2), $33
  update_sprite 29, 64, 47+(8*3), $33
  update_sprite 30, 64, 47+(8*4), $33
  update_sprite 31, 64, 47+(8*5), $33
  update_sprite 32, 64, 47+(8*6), $33
  update_sprite 33, 64, 47+(8*7), $33
  update_sprite 34, 64, 47+(8*8), $33
  update_sprite 35, 64, 47+(8*9), $33
  update_sprite 36, 64, 47+(8*10), $33

  ld a, HIGH(wShadowOAM)
  call hOAMDMA

  ;; Copy initial tile data
  ld a, BANK(BRERB)
  ld [rROMB0], a

  ld a, LOW(BRERB)
  ld [ptr_next_update_bg], a
  ld a, HIGH(BRERB)
  ld [ptr_next_update_bg+1], a
  call update_bg

  ld a, BANK(BRERB2)
  ld [next_map_bank], a
  ld a, LOW(BRERB2)
  ld [update_playfield_buffer+1], a
  ld a, HIGH(BRERB2)
  ld [update_playfield_buffer+2], a

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800 | LCDCF_OBJON
	ld [hLCDC], a
	ld [rLCDC], a

animation_loop:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Load buffer with new tile data
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  call WaitVBlank

  ld a, [next_map_bank]
  ld [rROMB0], a

  call update_playfield_buffer

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Execute buffer, loading tile data into unused map
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [current_bg]
  xor %00000100
  ld [current_bg], a
  ld h, a
  ld l, 0

  di

  ld a, IEF_VBLANK
  ldh [rIE], a
  halt ; wait for VBlank
  nop

  assert IEF_VBLANK + 1 == IEF_STAT
  inc a ; ld a, IEF_STAT
  ldh [rIE], a
  ld a, STATF_MODE00
  ldh [rSTAT], a ; Careful, this may make the STAT int pending

  call playfield_buffer

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Load gfx, and swap map
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [next_gfx_bank]
  ld [rROMB0], a

  di

  ld a, IEF_VBLANK
  ldh [rIE], a
  halt ; wait for VBlank
  nop

  ; swap map
  ld a, [rLCDC]
  xor %00001000
  ld [rLCDC], a
  ld [hLCDC], a

  ld a, IEF_STAT
  ldh [rIE], a
  ld a, STATF_MODE00
  ldh [rSTAT], a ; Careful, this may make the STAT int pending

  ; TODO: do OAM DMA
  call update_bg
  ;; Code to update graphics returns with RETI so interrupts are enabled.

  ld a, [hPressedKeys]
  and $FF
  jr z, .no_button

  ;; User pressed a key

  ; Turn the LCD off
	ld a, 0
	ld [hLCDC], a

.wait_lcdc_off:
  ld a, [rLCDC]
  and %10000000
  jr nz, .wait_lcdc_off

  ld a, $98
  ld [current_bg], a

  ld hl, cool_bgs
  ld a, [viewing_animation]
  add a, 6
  cp 144+6+6
  jr nz, .no_reset
  xor a
.no_reset:
  ld [viewing_animation], a
  add_a_to_hl

  ld a, [hl+]
  ; ld a, BANK(CLORB)
  ld [rROMB0], a

  ld a, [hl+]
  ; ld a, LOW(CLORB)
  ld [ptr_next_update_bg], a
  ld a, [hl+]
  ; ld a, HIGH(CLORB)
  ld [ptr_next_update_bg+1], a
  call update_bg

  ld hl, cool_bgs
  ld a, [viewing_animation]
  add_a_to_hl
  inc hl
  inc hl
  inc hl

  ld a, [hl+]
  ; ld a, BANK(CLORB2)
  ld [next_map_bank], a
  ld a, [hl+]
  ; ld a, LOW(CLORB2)
  ld [update_playfield_buffer+1], a
  ld a, [hl+]
  ; ld a, HIGH(CLORB2)
  ld [update_playfield_buffer+2], a

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800 | LCDCF_OBJON
	ld [hLCDC], a
	ld [rLCDC], a

; .wait_for_below_play_area
;   ld a, [rLY]
;   cp 135 ; free to do OAM DMA here (past the play area)
;   jr nc, .wait_for_below_play_area

.no_button:

  jp animation_loop