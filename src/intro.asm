include "defines.asm"

MACRO add_a_to_r16
    add \2
    ld \2, a
    adc \1
    sub \2
    ld \1, a
ENDM

;; Thanks PinoBatch!
MACRO sub_from_r16  ;; (high, low, value)
    ld a, \2
    sub \3
    ld \2, a
    sbc a  ; A = -1 if borrow or 0 if not
    add \1
    ld \1, a
ENDM

MACRO add_a_to_hl
    add_a_to_r16 h, l
ENDM

MACRO add_a_to_de
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

include "res/backgrounds/splash_screen.asm"

BRERB EQUS "bg01_gfx_init"
BRERB2 EQUS "bg01_map0"

;; This defines its own sections
include "res/sprite_block_gfx.sep1.2bpp.asm"

SECTION "Sprite Graphics", ROMX
all_graphics:
incbin "res/levelscore.2bppu"
incbin "res/Clock_Icon.2bppu"
ds 16 ; alignment
incbin "res/numbers_big_8x8.2bppu"
incbin "res/numbers_8x8_only.2bppu"

incbin "res/radar.2bpp"
incbin "res/pice_fall_highlight.2bpp"
.end:

block_underlay:
REPT 7
dw `02222222
ENDR
dw `00000000
.end:

block_sprite_gfx::
incbin "res/sprite_block_gfx.sep1.2bpp"
.end:

block_gfx::
incbin "res/pices_8x8_Grid-Alpha-Bomb.2bpp"
.end:

block_highlight:
incbin "res/HighLight_blocks.2bpp"
.end:

block_match_anim:
incbin "res/anim_match_found.2bpp"
.end:

explosion_anim:
incbin "res/anim_explosion.2bpp"
.end:

SECTION "Playfield Buffer ROM", ROM0

playfield_buffer_rom:
include "playfield_buffer.inc"
.end:

SECTION "Playfield Buffer RAM", WRAM0
playfield_buffer::
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
  ld bc, (all_graphics.end - all_graphics)
  call Memcpy

  ;; Copy block graphics to sprite area (for preview)
  ld de, block_sprite_gfx
  ld c, 32
  rst MemcpySmall

  ;; Leave 2 sprites of empty space in sprite area for block graphics
  ld a, (16*4)
  add_a_to_hl

  ;; Put 2 copies of the block underlay
  ld de, block_underlay
  ld c, 16
  rst MemcpySmall
  ld de, block_underlay
  ld c, 16
  rst MemcpySmall

  ;; Copy block match animation to sprite area
  ld de, block_match_anim
  ld bc, block_match_anim.end - block_match_anim
  call Memcpy


  ;; Copy explosion animation to sprite area
  ld de, explosion_anim
  ld c, explosion_anim.end - explosion_anim
  rst MemcpySmall

  ;; Copy "pice" graphics to bg area
  ld a, BANK(block_gfx)
  ld [rROMB0], a
  ld de, block_gfx
  ld hl, $8800
  ld c, 32*2
  rst MemcpySmall

  ld de, block_highlight
  ld c, block_highlight.end - block_highlight
  rst MemcpySmall

  ld de, playfield_buffer_rom
  ld hl, playfield_buffer
  ld bc, playfield_buffer_rom.end - playfield_buffer_rom
  call Memcpy

  ld a, 16
  ld [wShadowOAM], a
  ld a, 8
  ld [wShadowOAM+1], a

macro update_sprite  ; which sprite, x, y, tile
  ld a, \3+16
  ld [wShadowOAM+(4*\1)], a
  ld a, \2+8
  ld [wShadowOAM+(4*\1)+1], a
  ld a, \4
  ld [wShadowOAM+(4*\1)+2], a
endm

macro update_sprite2  ; which sprite, x, y, tile
  ld a, \3+16
  ld [wShadowOAM2+(4*\1)], a
  ld a, \2+8
  ld [wShadowOAM2+(4*\1)+1], a
  ld a, \4
  ld [wShadowOAM2+(4*\1)+2], a
endm

macro alt_palette2
  ld a, 1 << 4
  ld [wShadowOAM2+(4*\1)+3], a
endm

  ; lvl
  update_sprite 0, 2+(8*0), 2, 0
  update_sprite 1, 2+(8*1), 2, 1

  ; 01
  update_sprite 2, 19+(7*0), 2, 8
  update_sprite 3, 19+(7*1), 2, 9

  ; clock
  update_sprite 4, 70+(8*1)-1, 4-3, 6

  ; score
  update_sprite 5, 128+(8*0), 4-3, 2
  update_sprite 6, 128+(8*1), 4-3, 3
  update_sprite 7, 128+(8*2), 4-3, 4
  update_sprite 8, 128+(8*3), 4-3, 5

  ; Pice preview
  update_sprite 10, 2, 16, $38
  update_sprite 11, 10, 16, $39
  update_sprite 12, 2, 16+8, $39
  update_sprite 13, 10, 16+8, $38

  update_sprite 14, 20, 16, $38
  update_sprite 15, 28, 16, $39
  update_sprite 16, 20, 16+8, $39
  update_sprite 17, 28, 16+8, $38

  ; Score numbers
  ; update_sprite 18, 115-(6*1), 9, $12
  ; update_sprite 19, 115+(6*0), 9, $12
  update_sprite 20, 115+(6*1)-1, 9, $12
  update_sprite 21, 115+(6*2)-1, 9, $12
  update_sprite 22, 115+(6*3)-1, 9, $12
  update_sprite 23, 115+(6*4)-1, 9, $12
  update_sprite 24, 115+(6*5)-1, 9, $12
  update_sprite 25, 115+(6*6)-1, 9, $12

  ; time numbers
  update_sprite 26, 80-15, 9, $12
  update_sprite 27, 80-15+6, 9, $12
  ; :
  update_sprite 28, 80-15+11+4, 9, $12
  update_sprite 29, 80-15+11+4+6, 9, $12

  ; radar
  update_sprite2 0, 124+(8*0), 33, $30
  update_sprite2 1, 124+(8*2), 33, $32
  update_sprite2 2, 124+(8*1), 33, $1C

  ; radar "stem"
  update_sprite2 3, 124+(8*1), 32+(16*1), $34
  update_sprite2 4, 124+(8*1), 32+(16*2), $34
  update_sprite2 5, 124+(8*1), 32+(16*3), $34
  update_sprite2 6, 124+(8*1), 32+(16*4), $34
  update_sprite2 7, 124+(8*1), 32+(16*5), $34
  update_sprite2 8, 124+(8*1), 32+(16*6), $34

  ; Pice fall highlight
  update_sprite2 9, 64, 49+(16*0), $36
  update_sprite2 10, 64, 49+(16*1), $36
  update_sprite2 11, 64, 49+(16*2), $36
  update_sprite2 12, 64, 49+(16*3), $36
  update_sprite2 13, 64, 49+(16*4), $36
  update_sprite2 14, 64, 49+(16*5), $36

  ; Falling block
  update_sprite2 15, 16, 48, $3A
  update_sprite2 16, 16+8, 48, $3C
  update_sprite2 17, 16, 48, $3E
  update_sprite2 18, 16+8, 48, $3E
  alt_palette2 17
  alt_palette2 18

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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Load buffer with new tile data
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [next_map_bank]
  ld [rROMB0], a

  call update_playfield_buffer

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Run game logic update
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  call game_step

.wait_for_below_play_area
  ld a, [rLY]
  cp 135 ; free to do OAM DMA here (past the play area)
  jr nz, .wait_for_below_play_area
.wait_for_below_play_area_hblank
  ld a, [rSTAT]
  and %0000011
  jr nz, .wait_for_below_play_area_hblank

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
  cp 30
  jr nz, .wait_for_before_radar
.wait_for_before_radar_hblank
  ld a, [rSTAT]
  and %0000011
  jr nz, .wait_for_before_radar_hblank

  ld a, [rLCDC]
  set 2, a
  ld [rLCDC], a
  ld a, HIGH(wShadowOAM2)
  call hOAMDMA

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Run game logic update
  ;; Anything we couldn't cram into the previous step...
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  call game_step2

.wait_for_below_play_area0
  ld a, [rLY]
  cp 135 ; free to do OAM DMA here (past the play area)
  jr nz, .wait_for_below_play_area0
.wait_for_below_play_area_hblank0
  ld a, [rSTAT]
  and %0000011
  jr nz, .wait_for_below_play_area_hblank0

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

  jp animation_loop