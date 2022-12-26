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

MACRO update_sprite  ; which sprite, x, y, tile
  ld a, \3+16
  ld [wShadowOAM+(4*\1)], a
  ld a, \2+8
  ld [wShadowOAM+(4*\1)+1], a
  ld a, \4
  ld [wShadowOAM+(4*\1)+2], a
ENDM

MACRO update_sprite2  ; which sprite, x, y, tile
  ld a, \3+16
  ld [wShadowOAM2+(4*\1)], a
  ld a, \2+8
  ld [wShadowOAM2+(4*\1)+1], a
  ld a, \4
  ld [wShadowOAM2+(4*\1)+2], a
ENDM

MACRO alt_palette2
  ld a, 1 << 4
  ld [wShadowOAM2+(4*\1)+3], a
ENDM

DEF BLACK = %11
DEF WHITE = %00
DEF LIGHT = %01
DEF DARK = %10

MACRO BGColor
  ld a, \1 | (\2 << 2) | (\3 << 4) | (\4 << 6)
ENDM

SECTION "Music vars", WRAM0
music_bank:: db
music_pointer:: dw

SECTION "Animation vars", WRAM0
current_bg:: db
next_gfx_bank:: db
next_map_bank:: db
update_playfield_buffer:: ds 3 ; includes jump opcode

transition_state: db

SECTION "Blockset drawing ram code", WRAM0
current_blockset_bank: db
draw_block0: ds 3
draw_block1: ds 3

SECTION "Static RAM Code", ROM0
static_ram_code:
  xor a
  ldh [rIF], a
  assert IEF_VBLANK == 1
  inc a ; ld a, IEF_VBLANK
  ldh [rIE], a
  ret
.update_bg:
  ld [orig_sp], sp
  db $C3 ; jp xxxx
.end:

SECTION "RAM Code", WRAM0
update_bg_done:: ds 1 ; ld sp, xxxx
orig_sp: ds 2 ; operand of above
ram_code:
  ds static_ram_code.update_bg - static_ram_code
update_bg:
  ds static_ram_code.end - static_ram_code.update_bg
ptr_next_update_bg::
  ds 2 ; operand of above

;; This defines its own sections

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
incbin "res/bg_block_gfx.2bpp"
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
  ; ld a, BANK(asof0)
  ; ld [music_bank], a
  ; ld hl, asof0
  ; ld a, l
  ; ld [music_pointer], a
  ; ld a, h
  ; ld [music_pointer+1], a

  ; Turn the LCD off
	xor a
	ld [hLCDC], a
.wait_lcdc_off:
  ld a, [rLCDC]
  and %10000000
  jr nz, .wait_lcdc_off

  di

  ;; Beyond this point, IME never comes back on

  xor a
  ld [hPressedKeys], a

  ld de, static_ram_code
  ld hl, ram_code
  ld c, static_ram_code.end - static_ram_code
  rst MemcpySmall

  ld a, $31 ; ld sp, xxxx
  ld [update_bg_done], a

  ld a, $C3 ; jp xxxx
  ld [update_playfield_buffer], a

  ;; Set transition state
  ld a, 1
  ld [transition_state], a

  ld a, $C3 ; JP
  ld [draw_block0], a
  ld [draw_block1], a

  ld hl, skin0
  call load_skin

  ld de, playfield_buffer_rom
  ld hl, playfield_buffer
  ld bc, playfield_buffer_rom.end - playfield_buffer_rom
  call Memcpy

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

  ; Pice preview underlay
  update_sprite 30, 2, 16, $3E
  update_sprite 31, 10, 16, $3E
  update_sprite 32, 2, 16+8, $3E
  update_sprite 33, 10, 16+8, $3E

  update_sprite 34, 20, 16, $3E
  update_sprite 35, 28, 16, $3E
  update_sprite 36, 20, 16+8, $3E
  update_sprite 37, 28, 16+8, $3E

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

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800 | LCDCF_OBJON
	ld [rLCDC], a

  call UnfreezeScreen

  call init_game
  ;; fallthrough

kernel_loop:
  ld a, [hPressedKeys]
  bit PADB_START, a
  jr z, .no_begin_transition
  ld a, 16
  ld [transition_state], a

.no_begin_transition:
  ld a, [transition_state]
  dec a
  call nz, transition_stage

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

  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt ; wait for VBlank
  nop

  assert IEF_VBLANK + 1 == IEF_STAT
  ld a, IEF_STAT
  ldh [rIE], a
  ld a, STATF_MODE00
  ldh [rSTAT], a ; Careful, this may make the STAT int pending

  call playfield_buffer

  call do_music

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Update falling block GFX
  ;; (happens during downtime waiting for the OAM scanline)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [current_blockset_bank]
  ld [rROMB0], a

  ld hl, $83A0

  ld a, [block+0]
  and 1
  call z, draw_block0
  call nz, draw_block1

  ld a, [block+2]
  and 1
  call z, draw_block0
  call nz, draw_block1

  ld a, [block+1]
  and 1
  call z, draw_block0
  call nz, draw_block1

  ld a, [block+3]
  and 1
  call z, draw_block0
  call nz, draw_block1

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

  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt ; wait for VBlank
  nop

  ; swap map
  ldh a, [rLCDC]
  xor %00001000
  ldh [rLCDC], a

  ld a, IEF_STAT
  ldh [rIE], a
  ld a, STATF_MODE00
  ldh [rSTAT], a ; Careful, this may make the STAT int pending

  call update_bg
  call do_music

  jp kernel_loop

load_skin:
  ;; Set up initial block set
  ld a, [hl+]
  ld [current_blockset_bank], a

  ld a, [hl+]
  ld [draw_block0+1], a
  ld a, [hl+]
  ld [draw_block0+2], a

  ld a, [hl+]
  ld [draw_block1+1], a
  ld a, [hl+]
  ld [draw_block1+2], a

  ;; Block gfx offset
  ld a, [hl+]
  push hl
  ld h, [hl]
  ld l, a

  push hl
  push hl

  ;; Copy sprite graphics
  ld a, BANK(all_graphics)
  ld [rROMB0], a
  ld de, all_graphics
  ld hl, $8000
  ld bc, (all_graphics.end - all_graphics)
  call Memcpy

  ;; Copy block graphics to sprite area (for preview)
  pop de

  ld a, e
  add LOW(block_sprite_gfx)
  ld e, a
  adc d
  sub e
  add HIGH(block_sprite_gfx)
  ld d, a

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

  pop de

  ld a, e
  add LOW(block_gfx)
  ld e, a
  adc d
  sub e
  add HIGH(block_gfx)
  ld d, a

  ld hl, $8800
  ld c, 32*2
  rst MemcpySmall

  ld de, block_highlight
  ld c, block_highlight.end - block_highlight
  rst MemcpySmall

  ;; Setup animated BG
  pop hl
  inc hl

  ld a, $98
  ld [current_bg], a

  ;; Copy initial tile data
  ld a, [hl+]
  ld [rROMB0], a

  ld a, [hl+]
  ld [ptr_next_update_bg], a
  ld a, [hl+]
  ld [ptr_next_update_bg+1], a
  call update_bg

  ld a, [hl+]
  ld [next_map_bank], a
  ld a, [hl+]
  ld [update_playfield_buffer+1], a
  ld a, [hl+]
  ld [update_playfield_buffer+2], a

  ret

transition_stage:
  ;; Fade out
  ld [transition_state], a
  cp 16
  jr nz, .no_step1

  BGColor DARK, WHITE, LIGHT, WHITE
  ld [rBGP], a
  BGColor WHITE, WHITE, LIGHT, DARK
  ld [rOBP0], a
  BGColor LIGHT, DARK, WHITE, WHITE
  ld [rOBP1], a
  ret

.no_step1:
  cp 14
  jr nz, .no_step2

  BGColor LIGHT, WHITE, WHITE, WHITE
  ld [rBGP], a
  BGColor WHITE, WHITE, WHITE, LIGHT
  ld [rOBP0], a
  BGColor WHITE, LIGHT, WHITE, WHITE
  ld [rOBP1], a
  ret

.no_step2:
  cp 12
  jr nz, .no_step3

  BGColor WHITE, WHITE, WHITE, WHITE
  ld [rBGP], a
  ld [rOBP0], a
  ld [rOBP1], a
  ret

.no_step3:
  cp 10
  jp nz, .no_step4

  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt ; wait for VBlank

  ld hl, skin1.border_bank
  call colorize

  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt ; wait for VBlank

  ;; Turn the LCD off
  xor a
  ldh [rLCDC], a

  ld hl, skin1
  call load_skin

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800 | LCDCF_OBJON
	ld [rLCDC], a

  call UnfreezeScreen

  pop af
  jp kernel_loop

.no_step4:
  cp 8
  jr nz, .no_step5

  BGColor LIGHT, WHITE, WHITE, WHITE
  ld [rBGP], a
  BGColor WHITE, WHITE, WHITE, LIGHT
  ld [rOBP0], a
  BGColor WHITE, LIGHT, WHITE, WHITE
  ld [rOBP1], a
  ret

.no_step5:
  cp 6
  jr nz, .no_step6

  BGColor DARK, WHITE, LIGHT, WHITE
  ld [rBGP], a
  BGColor WHITE, WHITE, LIGHT, DARK
  ld [rOBP0], a
  BGColor LIGHT, DARK, WHITE, WHITE
  ld [rOBP1], a
  ret

.no_step6:
  cp 4
  jr nz, .no_step7

  BGColor BLACK, LIGHT, DARK, WHITE
  ld [rBGP], a
  BGColor LIGHT, WHITE, DARK, BLACK
  ld [rOBP0], a
  BGColor DARK, BLACK, LIGHT, WHITE
  ld [rOBP1], a
  ret

.no_step7:
  cp 2
  ret nz

  ld a, 1
  ld [transition_state], a

  ret

do_music:
  ret
  ld a, [music_bank]
  ld [rROMB0], a
  ld a, [music_pointer]
  ld l, a
  ld a, [music_pointer+1]
  ld h, a
  ld de, $0001

  jp hl