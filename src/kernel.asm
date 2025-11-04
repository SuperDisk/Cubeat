include "defines.asm"

DEF DBG_SCANLINES1 = 1
DEF DBG_SCANLINES2 = 1

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

MACRO alt_palette
  ld a, %00010001
  ld [wShadowOAM+(4*\1)+3], a
ENDM

MACRO alt_palette2
  ld a, %00010001
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
music_bank: db
music_pointer: dw
decompress_in: dw
decompress_out: dw

SECTION "Music Buffer", WRAM0[$D000]
music_buffer: ds $FFF

SECTION "Animation vars", WRAM0
current_skin:: db
current_bg:: db
next_gfx_bank:: db
next_map_bank:: db
update_playfield_buffer:: ds 3 ; includes jump opcode

transition_state:: db

SECTION "Blockset drawing ram code", WRAM0
current_blockset_bank: db
draw_block0: ds 3
draw_block1: ds 3

SECTION "Static RAM Code", ROM0
static_ram_code::
  xor a
  ldh [rIF], a
  assert IEF_VBLANK == 1
  inc a ; ld a, IEF_VBLANK
  ldh [rIE], a
  ret
.update_bg:
  ld [orig_sp], sp
  db $C3 ; jp xxxx
.end::

SECTION "RAM Code", WRAM0
update_bg_done:: ds 1 ; ld sp, xxxx
orig_sp: ds 2 ; operand of above
ram_code::
  ds static_ram_code.update_bg - static_ram_code
update_bg::
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

bonus_anim:
incbin "res/anim_bonus.2bpp"
.end:

SECTION "Playfield Buffer ROM", ROMX

playfield_buffer_rom::
include "playfield_buffer.inc"
.end::

SECTION "Playfield Buffer RAM", WRAM0
playfield_buffer::
ds (playfield_buffer_rom.end - playfield_buffer_rom)

; include "res/music/sinstrat.asm"

include "res/music/asof.asm"
purge dwbe
include "res/music/cutie.asm"
purge dwbe
include "res/music/db.asm"
purge dwbe
include "res/music/haunted.asm"
purge dwbe
include "res/music/hydroplane.asm"
purge dwbe
include "res/music/jazzberr.asm"
purge dwbe
include "res/music/josss.asm"
purge dwbe
include "res/music/journey.asm"
purge dwbe
include "res/music/keisari.asm"
purge dwbe
include "res/music/leafpile.asm"
purge dwbe
include "res/music/nerve.asm"
purge dwbe
include "res/music/opl3_journey_to_the_forgotten_star.asm"
purge dwbe
include "res/music/opl3thing.asm"
purge dwbe
include "res/music/orca.asm"
purge dwbe
include "res/music/restart.asm"
purge dwbe
include "res/music/sinstrat.asm"
purge dwbe
include "res/music/sxtnt.asm"
purge dwbe
include "res/music/test.asm"
purge dwbe
include "res/music/thumb.asm"
purge dwbe
include "res/music/trance.asm"
purge dwbe
include "res/music/trial.asm"
purge dwbe
include "res/music/try_again.asm"
purge dwbe
include "res/music/wtf.asm"
purge dwbe
include "res/music/zen.asm"
purge dwbe
; include "res/music/asof.asm"
; purge dwbe

SECTION "Kernel", ROM0

mus_table:

db LOW(BANK(asof0))
dw asof0
db 0

db LOW(BANK(cutie0))
dw cutie0
db 0

db LOW(BANK(db0))
dw db0
db 0

db LOW(BANK(opl3thing0))
dw opl3thing0
db 0

db LOW(BANK(hydroplane0))
dw hydroplane0
db 0

db LOW(BANK(jazzberr0))
dw jazzberr0
db 0

db LOW(BANK(josss0))
dw josss0
db 0

db LOW(BANK(journey0))
dw journey0
db 0

db LOW(BANK(keisari0))
dw keisari0
db 0

db LOW(BANK(keisari0))
dw keisari0
db 0

db LOW(BANK(nerve0))
dw nerve0
db 0

db LOW(BANK(opl3_journey_to_the_forgotten_star0))
dw opl3_journey_to_the_forgotten_star0
db 0

db LOW(BANK(opl3thing0))
dw opl3thing0
db 0

db LOW(BANK(orca0))
dw orca0
db 0

db LOW(BANK(restart0))
dw restart0
db 0

db LOW(BANK(sinstrat0))
dw sinstrat0
db 0

db LOW(BANK(sxtnt0))
dw sxtnt0
db 0

db LOW(BANK(thumb0))
dw test0
db 0

db LOW(BANK(thumb0))
dw thumb0
db 0

db LOW(BANK(trance0))
dw trance0
db 0

db LOW(BANK(trial0))
dw trial0
db 0

db LOW(BANK(try_again0))
dw try_again0
db 0

db LOW(BANK(wtf0))
dw wtf0
db 0

db LOW(BANK(zen0))
dw zen0
db 0

db LOW(BANK(zen0))
dw zen0
db 0

Kernel::
  call clear_oam

  ld hl, skin0.border_bank
  call colorize
  call safe_turn_off_lcd

  call reset_opl3

  ld a, [level_num]
  ld hl, mus_table
  add_a_to_hl
  ld a, [hl+]
  ld [music_bank], a
  ld a, [hl+]
  ld [music_pointer], a
  ld a, [hl]
  ld [music_pointer+1], a

  ; ld a, LOW(BANK(sinstrat0))
  ; ld [music_bank], a
  ; ld a, LOW(sinstrat0)
  ; ld [music_pointer], a
  ; ld a, HIGH(sinstrat0)
  ; ld [music_pointer+1], a

  xor a
  ld [hPressedKeys], a
  ld [current_skin], a

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

  call init_playfield_buffer

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
  alt_palette 30
  alt_palette 31
  alt_palette 32
  alt_palette 33

  update_sprite 34, 20, 16, $3E
  update_sprite 35, 28, 16, $3E
  update_sprite 36, 20, 16+8, $3E
  update_sprite 37, 28, 16+8, $3E
  alt_palette 34
  alt_palette 35
  alt_palette 36
  alt_palette 37

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

  call FadeInit
  ld hl, KnownRet
  call FadeIn

  call init_game
  ;; fallthrough

kernel_loop:
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

IF DEF(DBG_SCANLINES1)
  ld a, $00
  ld [rBGP], a
ENDC
  call game_step
IF DEF(DBG_SCANLINES1)
  ld a, %00_10_01_11
  ld [rBGP], a
ENDC

.wait_for_below_play_area:
  ld a, [rLY]
  cp 135 ; free to do OAM DMA here (past the play area)
  jr c, .wait_for_below_play_area
.wait_for_below_play_area_hblank:
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

  call FadeStep

.wait_for_before_radar:
  ld a, [rLY]
  cp 30
  jr nz, .wait_for_before_radar
.wait_for_before_radar_hblank:
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

IF DEF(DBG_SCANLINES2)
  ld a, $FF
  ld [rBGP], a
ENDC
  call game_step2
  ; ld a, %00_10_01_11
  ; ld [rBGP], a

  ; ld a, $00
  ; ld [rBGP], a
  call do_music
IF DEF(DBG_SCANLINES2)
  ld a, %00_10_01_11
  ld [rBGP], a
ENDC

  call tick_sfx

.wait_for_below_play_area0:
  ld a, [rLY]
  cp 135 ; free to do OAM DMA here (past the play area)
  jr c, .wait_for_below_play_area0
.wait_for_below_play_area_hblank0:
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

IF DEF(DBG_SCANLINES1)
  ld a, $00
  ld [rBGP], a
ENDC
  call do_music
IF DEF(DBG_SCANLINES1)
  ld a, %00_10_01_11
  ld [rBGP], a
ENDC

  call tick_sfx

  jp kernel_loop

load_skin:
  ld a, BANK(skins)
  ld [rROMB0], a

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

  ;; Copy bonus anim to sprite area
  ld de, bonus_anim
  ld bc, bonus_anim.end - bonus_anim
  call Memcpy

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

  ld a, BANK(skins)
  ld [rROMB0], a

  ld a, $98
  ld [current_bg], a

  ;; Copy initial tile data
  ld a, [hl+]
  ld e, a
  ; ld [rROMB0], a

  ld a, [hl+]
  ld [ptr_next_update_bg], a
  ld a, [hl+]
  ld [ptr_next_update_bg+1], a

  ld a, e
  ld [rROMB0], a
  call update_bg

  ld a, BANK(skins)
  ld [rROMB0], a

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

  ld a, BANK(skin_table)
  ld [rROMB0], a

  ld a, [current_skin]
  add a
  ld hl, skin_table
  ld d, 0
  ld e, a
  add hl, de
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  push hl

  ld e, (skin0.border_bank-skin0)
  add hl, de
  call colorize

  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt ; wait for VBlank

  ;; Turn the LCD off
  xor a
  ldh [rLCDC], a

  pop hl
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

SECTION "Music Jumptable actions", ROM0, ALIGN[8]
jumptable:
  jr vgm_literals_end
  jr vgm_literals
  jr switch_bank
  jr switch_port0

switch_port0:
  dec sp
switch_port:
  dec c
  dec c
  jr nz, decode_done
  ;; fallthrough

mus_loop:
  pop hl
  ld a, l
  cp 4
  jr nc, .phrase
  add a
  ld l, a
  ld a, h
  ld h, HIGH(jumptable)
  jp hl

.phrase:
  ld e, l
  res 7, l

  ld a, h
  ld h, l
  ld l, a
REPT 2
  ld a, [hl+]
  ld [bc], a
  inc c
  ld a, [hl+]
  ld [bc], a
  dec c
ENDR

REPT 2
  ld a, [hl+]
  or a
  jr z, .phrase_done

  ld [bc], a
  inc c
  ld a, [hl+]
  ld [bc], a
  dec c
ENDR

.phrase_done:
  bit 7, e
  jr nz, switch_port
  jr mus_loop


vgm_literals_end:
  ld h, b
  ld l, c

.lit_loop:
  pop de
  ld [hl], e
  inc l
  ld [hl], d
  dec l

  dec a
  jr nz, .lit_loop
  jr switch_port

vgm_literals:
  ld h, b
  ld l, c

.lit_loop:
  pop de

  ld [hl], e
  inc l
  ld [hl], d
  dec l

  dec a
  jr nz, .lit_loop
  jr mus_loop

switch_bank:
  pop hl
  ld sp, hl
  ld [rROMB0], a
  ld [music_bank], a
  jr mus_loop

do_music::
  ; ret

  ld a, [music_bank]
  ld hl, $2FFF
  ld [hl+], a
  ld [hl], 1
  assert $2FFF + 1 == rROMB1

  ld sp, music_pointer
  pop hl
  ld sp, hl

  ld bc, $A002
  jp mus_loop

decode_done:
  ld [music_pointer], sp

  ld hl, rROMB1
  ld [hl], 0

  ld sp, wStackBottom-2
  ret