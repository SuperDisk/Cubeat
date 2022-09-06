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
; include "res/backgrounds/bg02.asm"
; include "res/backgrounds/bg03.asm"
; include "res/backgrounds/bg04.asm"
; include "res/backgrounds/bg05.asm"
; include "res/backgrounds/bg06.asm"
; include "res/backgrounds/bg07.asm"
; include "res/backgrounds/bg08.asm"
; include "res/backgrounds/bg09.asm"
; include "res/backgrounds/bg10.asm"
; include "res/backgrounds/bg11.asm"
; include "res/backgrounds/bg12.asm"
; include "res/backgrounds/bg13.asm"
; include "res/backgrounds/bg14.asm"
; include "res/backgrounds/bg15.asm"
; include "res/backgrounds/bg16.asm"
; include "res/backgrounds/bg17.asm"
; include "res/backgrounds/bg18.asm"
; include "res/backgrounds/bg19.asm"
; include "res/backgrounds/bg20.asm"
; include "res/backgrounds/bg21.asm"
; include "res/backgrounds/bg22.asm"
; include "res/backgrounds/bg23.asm"
; include "res/backgrounds/bg24.asm"
; include "res/backgrounds/bg25.asm"

; include "res/backgrounds/splash_screen.asm"

BRERB EQUS "bg01_gfx_init"
BRERB2 EQUS "bg01_map0"

SECTION "Sprite Graphics", ROMX
all_graphics:
incbin "res/leveltimescore.2bpp"
incbin "res/numbers_big_8x8.2bpp"
incbin "res/numbers_8x8_only.2bpp"
incbin "res/numbers_8x8_only.2bpp"
incbin "res/pices_8x8_sprite/pice_sprite_0.2bpp"
incbin "res/pices_8x8_sprite/pice_sprite_1.2bpp"

incbin "res/radar.2bpp"
incbin "res/pice_fall_highlight.2bpp"
all_graphics_end:

block_gfx:
incbin "res/pices_8x8_Grid-Alpha-Bomb.2bpp"
.end:

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

  ;; Copy "pice" graphics to bg area
  ld a, BANK(block_gfx)
  ld [rROMB0], a
  ld de, block_gfx
  ld hl, $8800
  ld c, 32*2
  rst MemcpySmall


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

update_sprite2: macro ; which sprite, x, y, tile
  ld a, \3+16
  ld [wShadowOAM2+(4*\1)], a
  ld a, \2+8
  ld [wShadowOAM2+(4*\1)+1], a
  ld a, \4
  ld [wShadowOAM2+(4*\1)+2], a
endm

  ; lvl
  update_sprite 0, 2+(8*0), 4-3, 0
  update_sprite 1, 2+(8*1), 4-3, 1

  ; 01
  update_sprite 9, 19+(8*0), 2, 8
  ; update_sprite 10, 18+(8*1), 2, 9

  ; time
  update_sprite 2, 70+(8*0), 4-3, 2
  update_sprite 3, 70+(8*1), 4-3, 3
  update_sprite 4, 70+(8*2), 4-3, 4

  ; score
  update_sprite 5, 125+(8*0), 4-3, 5
  update_sprite 6, 125+(8*1), 4-3, 6
  update_sprite 7, 125+(8*2), 4-3, 7
  update_sprite 8, 125+(8*3), 4-3, 4

  ; Pice preview
  update_sprite 10, 2, 17, $26
  update_sprite 11, 10, 17, $27
  update_sprite 12, 2, 17+8, $27
  update_sprite 13, 10, 17+8, $26

  update_sprite 14, 20, 17, $26
  update_sprite 15, 28, 17, $27
  update_sprite 16, 20, 17+8, $27
  update_sprite 17, 28, 17+8, $26

  ; Score numbers
  update_sprite 18, 115, 9, $12
  update_sprite 19, 115+(6*1), 9, $12
  update_sprite 20, 115+(6*2), 9, $12
  update_sprite 21, 115+(6*3), 9, $12
  update_sprite 22, 115+(6*4), 9, $12
  update_sprite 23, 115+(6*5), 9, $12
  update_sprite 24, 115+(6*6), 9, $12
  update_sprite 25, 115+(6*7), 9, $12

  ; radar
  update_sprite2 0, 124+(8*0), 32, $3E
  update_sprite2 1, 124+(8*2), 32, $40
  update_sprite2 2, 124+(8*1), 32, $2A

  ; radar "stem"
  update_sprite2 3, 124+(8*1), 32+(16*1), $42
  update_sprite2 4, 124+(8*1), 32+(16*2), $42
  update_sprite2 5, 124+(8*1), 32+(16*3), $42
  update_sprite2 6, 124+(8*1), 32+(16*4), $42
  update_sprite2 7, 124+(8*1), 32+(16*5), $42
  update_sprite2 8, 124+(8*1), 32+(16*6), $42

  ; Pice fall highlight
  update_sprite2 9, 64, 49+(16*0), $44
  update_sprite2 10, 64, 49+(16*1), $44
  update_sprite2 11, 64, 49+(16*2), $44
  update_sprite2 12, 64, 49+(16*3), $44
  update_sprite2 13, 64, 49+(16*4), $44
  update_sprite2 14, 64, 49+(16*5), $44

  update_sprite2 15, 64-16, 49+(16*0), $44
  update_sprite2 16, 64-16, 49+(16*1), $44
  update_sprite2 17, 64-16, 49+(16*2), $44
  update_sprite2 18, 64-16, 49+(16*3), $44
  update_sprite2 19, 64-16, 49+(16*4), $44
  update_sprite2 20, 64-16, 49+(16*5), $44

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

  call init_game

animation_loop:
  ;;;;;;;;;;;;;;;;;;;;
  ;; Run game logic update
  ;;;;;;;;;;;;;;;;;;;;

  call game_step

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Load buffer with new tile data
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [next_map_bank]
  ld [rROMB0], a

  call update_playfield_buffer

.wait_for_below_play_area
  ld a, [rLY]
  cp 136 ; free to do OAM DMA here (past the play area)
  jr nz, .wait_for_below_play_area

  ld a, [rLCDC]
  res 2, a
  ld [rLCDC], a
  ld a, HIGH(wShadowOAM)
  call hOAMDMA

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

.wait_for_before_radar
  ld a, [rLY]
  cp 32 ; free to do OAM DMA here (past the play area)
  jr nz, .wait_for_before_radar

  ld a, [rLCDC]
  set 2, a
  ld [rLCDC], a
  ld a, HIGH(wShadowOAM2)
  call hOAMDMA

.wait_for_below_play_area0
  ld a, [rLY]
  cp 136 ; free to do OAM DMA here (past the play area)
  jr nz, .wait_for_below_play_area0

  ld a, [rLCDC]
  res 2, a
  ld [rLCDC], a
  ld a, HIGH(wShadowOAM)
  call hOAMDMA

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
  ldh a, [rLCDC]
  xor %00001000
  ldh [rLCDC], a
  ld [hLCDC], a

  ld a, IEF_STAT
  ldh [rIE], a
  ld a, STATF_MODE00
  ldh [rSTAT], a ; Careful, this may make the STAT int pending

  call update_bg
  ;; Code to update graphics returns with RETI so interrupts are enabled.

  jr animation_loop