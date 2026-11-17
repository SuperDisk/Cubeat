include "defines.asm"

;; Graphical bits:
;; %0000000x = normal block
;; %0000001x = bomb
;; %0000010x = marked for destruction
;; %0000011x = getting swept by radar

;; Behavioral bits:
;; bit 7 = always there
;; bit 6 = "marked" for deletion by the radar
;; bit 5 = zero, necessary due to how match finding works
;; bit 4 = is a bomb
;; bit 3 = falling (set by the fall loop on a tile it moved that hasn't landed
;;         yet, and on placement for a half-block with nothing under it).
;;         Bombs neither spread from nor onto a falling tile. Settled tiles
;;         always have it clear, which the match finder's `add` relies on.
;; bit 1-2 = visual appearance (checked for "getting swept")
;; bit 0 = color

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug toggles to make development easier
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; DEF DBG_BLOCK = $80
DEF DBG_DONTFALL = 1
; DEF DBG_DONTANIMATE = 1
DEF SELECT_PAUSES_RADAR = 1
; DEF DBG_NO_BLOCK_PHYSICS = 1
; DEF DBG_NO_BOMB_PHYSICS = 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DEF DPAD_HOLD_FRAMES = 7

DEF BOARD_W = 18
DEF BOARD_H = 11
DEF ROW = 18

DEF NUM_ANIMS = 11

DEF EMPTY = 0
DEF WHITE = $80
DEF BLACK = $81
DEF WHITEBOMB = $D2
DEF BLACKBOMB = $D3
; there are more...

MACRO update_sprite2  ; which sprite, x, y, tile
  ld a, \3+16
  ld [wShadowOAM2+(4*\1)], a
  ld a, \2+8
  ld [wShadowOAM2+(4*\1)+1], a
  ld a, \4
  ld [wShadowOAM2+(4*\1)+2], a
ENDM

;;; Gives the tile at [hl] the "marked" graphic, but only if it is marked
;;; (bit 6), not a bomb (bit 4), and has the normal graphic (bits 1-2 clear).
;;; Destroy: AF
MACRO set_marked_graphic
  ld a, [hl]
  and %01010110
  cp %01000000
  jr nz, :+
  set 2, [hl]
:
ENDM

MACRO spriteX ; which sprite
  ld [wShadowOAM2+(4*(\1))+1], a
ENDM

MACRO spriteY ; which sprite
  ld [wShadowOAM2+(4*(\1))], a
ENDM

MACRO spriteTile1 ; tile
  ld [wShadowOAM+(4*(\1))+2], a
ENDM

MACRO spriteTile2 ; tile
  ld [wShadowOAM2+(4*(\1))+2], a
ENDM

MACRO spriteX1 ; which sprite
  ld [wShadowOAM+(4*(\1))+1], a
ENDM

MACRO deflevel
  ; Level number
  db \1

  ; Curve score thousands (bcd)
  db ((\2 / 10**2) % 10) << 4 | ((\2 / 10**1) % 10)
  db ((\2 / 10**0) % 10) << 4

  ; Radar speed (seconds for full sweep)
  dw MUL(DIV(BOARD_W * 8.0, (\3) * 30.0), 255.0) / 1.0

  ; Block fall wait (seconds)
  db \4*30

  ; Block fall rate (seconds to ground)
  db DIV(\5*30.0, BOARD_H*1.0)/1.0

  ; Skin number (0 for no change)
  db \6-1
ENDM

SECTION "Levels", ROMX

levels::
include "levels.inc"

level_table:
FOR I, 100
  level{d:I}: dw levels+(I*8)
ENDR

SECTION "Board", WRAM0, ALIGN[8]

;; We start the board at position $xxxx0001 so that we can
;; just use a single register as the pointer essentially, and detect
;; when we've bottomed out after an inc/dec, which only sets the Z flag.
board_start_sentinel: db

;; Board is 18 wide, 11 high (including the two tiles on top that are out of bounds)
board: ds (18*11)
.end:

board_end_sentinel: db

;; Match finding uses the same low byte to look up whether a cell is on the
;; left edge. Keep that lookup exactly one page above the board so switching
;; between them only requires INC H / DEC H.
ds $100 - (1 + (BOARD_W * BOARD_H) + 1)
board_edge_start_sentinel: db
edge_array: ds (BOARD_W * BOARD_H)

ASSERT LOW(edge_array) == LOW(board)
ASSERT HIGH(edge_array) == HIGH(board) + 1

SECTION "Other constant data", ROM0

initial_free_sprites:
FOR SPR, $4C, $9C+4, 4
  db SPR
ENDR

;; Dummy sprite entries so that we don't read out of bounds
ds 64, LOW(dummy_sprite)

__test_board:

; Blank
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


;; Buggy
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$81,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$81,$81,$81,$80,$81,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$81,$80,$81,$80,$80,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$80,$80,$81,$80,$81,$80,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$81,$80,$81,$80,$80,$81,$00,$00,$00,$00,$00,$00,$00,$00

; Alternating
; db $81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81
; db $80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80
; db $81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81
; db $80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80
; db $81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81
; db $80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80
; db $81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81
; db $80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80
; db $81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81
; db $80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80
; db $81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81,$80,$80,$81,$81

REPT 10
db $D2,$80,$D2,$80,$D2,$80,$D2,$80,$D2,$80,$D2,$80,$D2,$80,$D2,$80,$D2,$80
db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
db $80,$D2,$80,$D2,$80,$D2,$80,$D2,$80,$D2,$80,$D2,$80,$D2,$80,$D2,$80,$D2
db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
ENDR

; Maze
db $00,$80,$80,$80,$80,$80,$00,$00,$00,$00,$00,$00,$00,$80,$80,$80,$80,$80
db $00,$80,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$80
db $00,$80,$00,$00,$00,$80,$00,$80,$80,$00,$00,$00,$00,$80,$00,$00,$00,$80
db $00,$80,$00,$00,$00,$80,$80,$80,$80,$00,$00,$80,$80,$80,$00,$00,$00,$80
db $00,$80,$00,$00,$00,$00,$00,$00,$80,$80,$00,$80,$00,$00,$00,$00,$00,$80
db $00,$80,$00,$00,$00,$00,$00,$00,$00,$80,$00,$80,$00,$00,$00,$00,$00,$80
db $00,$80,$80,$80,$80,$00,$00,$00,$00,$80,$00,$80,$00,$00,$00,$00,$00,$80
db $00,$80,$80,$00,$80,$00,$80,$00,$80,$80,$00,$80,$00,$00,$00,$00,$00,$80
db $00,$80,$80,$00,$80,$00,$80,$00,$80,$00,$00,$80,$00,$00,$00,$00,$00,$80
db $00,$00,$00,$00,$80,$00,$80,$00,$80,$00,$00,$D2,$00,$00,$00,$00,$00,$80
db $00,$00,$00,$80,$80,$00,$80,$00,$80,$80,$80,$D2,$80,$80,$80,$00,$00,$00


; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$D2,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$D2,$00,$00,$00,$00,$00,$00,$00

; V
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80
db $80,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00
db $00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00
db $00,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00
db $00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00
db $00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$80,$80,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$80,$D2,$80,$00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00,$D2,$00,$00,$00,$00,$00,$00,$00,$00,$00

; X
; db $00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00
; db $00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$80,$80,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$80,$80,$80,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$D2,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$80,$80,$80,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$80,$80,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00
; db $00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00

;; falling white bomb
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$D2,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00

;; falling white match

;; this works since the fall logic will always cause the bottom left corner to
;; be empty if a slice of blocks are falling

; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$81,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$81,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$81,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$81,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$81,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$81,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$81,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$81,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$81,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$81,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$81,$00,$00,$00,$00,$00,$00

;; Bomb in middle of all white
; db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
; db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
; db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
; db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
; db $80,$80,$80,$80,$80,$80,$80,$80,$D2,$80,$80,$80,$80,$80,$80,$80,$80,$80
; db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
; db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
; db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
; db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
; db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
; db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80

; REPT 11
; db $E3,$E3,$E3,$E3,$E3,$E3,$E3,$E3,$E3,$E3,$E3,$E3,$E3,$E3,$E3,$E3,$E3,$E3
; ENDR

;; Bomb in middle of all black
; db $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
; db $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
; db $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
; db $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
; db $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
; db $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
; db $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
; db $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
; db $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
; db $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
; db $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83

;; All bomb
; REPT 11
; db $80,$82,$82,$82,$82,$82,$82,$82,$82,$82,$82,$82,$82,$82,$82,$82,$82,$80
; ENDR

;; All white
REPT 11
db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
ENDR

;; All black
; REPT 11
; db $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
; ENDR

SECTION "Board edge array 2", ROM0, ALIGN[8]
edge_array2:
; db 0
; REPT 11
; db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
; ENDR

;; TODO: Move these out of rom0
SECTION "X lookup table", ROM0, ALIGN[8]
x_lookup_table:
db 0
REPT BOARD_H
  FOR X, 0, BOARD_W
   db X
  ENDR
ENDR

SECTION "Y lookup table", ROM0, ALIGN[8]
y_lookup_table:
db 0
FOR Y, 0, BOARD_H
  REPT BOARD_W
    db Y
  ENDR
ENDR


SECTION "Game vars", WRAM0
score: ds 4 ; 7 digits
score_counter: dw
score_curve: dw ; needed score to advance to the next level (3 digits)

level_num:: db
locked_level:: db

drop_pos: db

time: dw
frame_counter: db
second_counter: db
radar_pos: dw
radar_speed: dw

can_quickdrop: db
falling_block_wait: db
falling_block_rate: db
falling_block_timer: db
falling_block_y: db

IF DEF(SELECT_PAUSES_RADAR)
radar_paused: db
ENDC

dpad_frames: db

blocks:
block:: ds 4
next_block1: ds 4
next_block2: ds 4

;; 0 = not marking
;; 1 = marking
radar_marking_state: db
need_to_destroy: db
num_destroyed: db

animations:
REPT NUM_ANIMS ; number of animation slots
db ; running
db ; xoff
db ; yoff
db ; palette
dw ; animation
db ; info
ENDR
anim_end_sentinel: db

anim_x_temp: db
anim_y_temp: db
anim_palette_temp: db
oam_clear_after: db

SECTION "Engine code", ROM0
init_game::
  xor a
  ld [frame_counter], a
  ld [second_counter], a
  ld [drop_pos], a
  ld [can_quickdrop], a
  ld [falling_block_y], a
  ld [radar_pos], a
  ld [radar_pos+1], a
  ld [radar_marking_state], a
  ld [need_to_destroy], a
  ld [score+0], a
  ld [score+1], a
  ld [score+2], a
  ld [score+3], a
  ld [score_counter+0], a
  ld [score_counter+1], a
  ld [num_destroyed], a
  ld [time+0], a
  ld [time+1], a
  ld [anim_x_temp], a ; silences an uninitialized mem warning
IF DEF(SELECT_PAUSES_RADAR)
  inc a
  ld [radar_paused], a
  dec a
ENDC

  ; A = 0 here
  ld hl, animations
  ld c, 7*NUM_ANIMS
  rst MemsetSmall

  ;; Build the left-edge mask one page above the board. Each row starts with
  ;; zero; every other cell is $FF.
  ld hl, edge_array
  ld c, BOARD_H
.init_edge_row:
  xor a
  ld [hl+], a
  ld b, BOARD_W - 1
  dec a
.init_edge_columns:
  ld [hl+], a
  dec b
  jr nz, .init_edge_columns
  dec c
  jr nz, .init_edge_row

  ld de, __test_board
  ld hl, board
  ld c, board.end - board
  rst MemcpySmall

  ;; Clear the padding between the board and the edge array. The flood fill
  ;; looks one row above and below the board without bounds checks.
  ld hl, board_end_sentinel + 1
  ld c, LOW(-(board_end_sentinel + 1))
  xor a
  rst MemsetSmall

  ld a, $F0
  ld [anim_end_sentinel], a

  ld a, $FF
  ld [board_start_sentinel], a
  ld [board_end_sentinel], a

  ;; Spread any bombs the board started with. After this, bombs only need
  ;; looking at when a tile lands.
  IF !DEF(DBG_NO_BOMB_PHYSICS)
  call bomb_scan_all
  ENDC

  ld a, BANK(levels)
  ld [rROMB0], a
  ld a, [level_num]
  ld hl, level_table
  add a
  add_a_to_hl
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  call load_level

  ld a, DPAD_HOLD_FRAMES
  ld [dpad_frames], a

  ;; Seed the random number generator
  ld hl, randstate
  ld a, [rDIV]
  ld [hl+], a
  ld a, [rDIV]
  ld [hl+], a
  ld a, [rDIV]
  ld [hl+], a
  ld a, [rDIV]
  ld [hl], a

  ld hl, blocks
  call generate_block
  call generate_block
  jp generate_block

;;; Generates a new block at the pointer in HL
;;; Param: HL = The level to load
;;; Destroy: HL BC AF
load_level::
  ld a, [hl+]
  ld [level_num], a

  ld a, [hl+]
  ld [score_curve], a
  ld a, [hl+]
  ld [score_curve+1], a

  ld a, [hl+]
  ld [radar_speed], a
  ld a, [hl+]
  ld [radar_speed+1], a

  ld a, [hl+]
  ld [falling_block_wait], a
  ld b, a

  ld a, [hl+]
  ld [falling_block_rate], a

  add b
  ld [falling_block_timer], a

  ld d, [hl]
  ld a, [current_skin]
  cp d
  ret z

  ld a, d
  ld [current_skin], a
  ld a, 16
  ld [transition_state], a

  ret

game_step::
  call poll_joystick

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Increment frame counter
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [frame_counter]
  inc a
  ld [frame_counter], a


  ld a, [hPressedKeys]
  bit PADB_START, a
  jp z, .no_shit

  ; ld a, [level_num]
  ; add 4
  ; ld [level_num], a

  ld hl, goto_pause_menu
  call FadeOut

;;;;;;;;;;;;;;;;;;;;;;;

  ; ld a, BANK(levels)
  ; ld [rROMB0], a

  ; ld hl, level_num
  ; ld a, [hl]
  ; inc a
  ; ld [hl], a

  ; ld hl, level_table
  ; add a
  ; add_a_to_hl
  ; ld a, [hl+]
  ; ld h, [hl]
  ; ld l, a
  ; call load_level

;;;;;;;;;;

  ; ld hl, goto_titlescreen
  ; call FadeOut

  ; xor a
  ; ld [anim_y_temp], a
  ; ld [anim_x_temp], a
  ; ld d, 80
  ; ld e, 80
  ; ld bc, anim_bonus
  ; call create_animation

.no_shit:


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Move radar right
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IF DEF(SELECT_PAUSES_RADAR)
  ld a, [hPressedKeys]
  bit PADB_SELECT, a
  jr z, .no_pause
  ld a, [radar_paused]
  xor 1
  ld [radar_paused], a
.no_pause:

  ld a, [radar_paused]
  or a
  jr nz, .check_rotate_piece
  ; jr .check_rotate_piece
ENDC

  assert radar_speed == radar_pos+2
  ld hl, radar_speed+1

  ld a, [hl-]
  ld d, a
  ld a, [hl-]
  ld e, a

  ld a, [hl-]
  ld l, [hl]
  ld h, a

  add hl, de
  ld a, h

  cp 163-16-3
  jr c, .no_reset_radar

  ;; immediately perform a destroy when we reset radar.

  ;; TODO: This happens in game_step1, so this could potentially take up too
  ;; much time. Look into this if we start having problems with our frame budget
  push hl
  call perform_destroy
  pop hl

  ld a, [num_destroyed]

  ld h, 0
  ld l, a

  ; hl *= 32
  swap l
  add hl, hl

  cp 4
  jr c, .less_than4

  ; hl *= 4
  add hl, hl
  add hl, hl

.less_than4:
  ld a, [score_counter]
  ld c, a
  ld a, [score_counter+1]
  ld b, a
  add hl, bc

  ld a, l
  ld [score_counter], a
  ld a, h
  ld [score_counter+1], a

  xor a
  ld [num_destroyed], a
  ld h, a
  ld l, a
.no_reset_radar:
  ld a, l
  ld [radar_pos], a
  ld a, h
  ld [radar_pos+1], a

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Rotate piece
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.check_rotate_piece:
  ld a, [hPressedKeys]
  bit PADB_A, a
  jr z, .no_rotate_left

  ld a, [block+0]
  ld h, a

  ld a, [block+1]
  ld [block+0], a

  ld a, [block+3]
  ld [block+1], a

  ld a, [block+2]
  ld [block+3], a

  ld a, h
  ld [block+2], a

.no_rotate_left:

  ld a, [hPressedKeys]
  bit PADB_B, a
  jr z, .no_rotate_right

  ld a, [block+3]
  ld h, a

  ld a, [block+1]
  ld [block+3], a

  ld a, [block+0]
  ld [block+1], a

  ld a, [block+2]
  ld [block+0], a

  ld a, h
  ld [block+2], a

.no_rotate_right:

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Move the drop position around
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [drop_pos]
  ld b, a
  ld a, [falling_block_y]
  sub 2
  jr c, .no_goto
  ld c, a
  call goto_xy_pos
  ld de, ROW
.no_goto:

  ld a, [hHeldKeys]
  and %00110000
  jr nz, .did_hold_key

.no_hold_key:
  ld a, DPAD_HOLD_FRAMES
  ld [dpad_frames], a
  jr .done

.did_hold_key:
  ;; User is pressing a direction key, prepare to rapidly move the block
  ld a, [dpad_frames]
  dec a
  jr nz, .no_slide
  ld a, [hHeldKeys]
  jr .do_slide

.no_slide:
  ld [dpad_frames], a

  ld a, [hPressedKeys]
.do_slide:
  ld c, a
  ld a, [drop_pos]
  ld b, a

  bit PADB_RIGHT, c
  jr z, .no_right

  ;; Player pressed right
  inc b

  ;; Check hit right side of board
  cp BOARD_W-2
  jr z, .revert

  ;; If we're above the board, don't check for collision
  ld a, [falling_block_y]
  cp 2
  jr c, .save

  ;; Check hit block
  push hl
  inc hl
  inc hl
  xor a
  or [hl]
  add hl, de
  or [hl]
  pop hl
  jr z, .save

.revert:
  dec b
  jr .save

.no_right:
  bit PADB_LEFT, c
  jr z, .done

  ;; Player pressed left
  dec b

  ;; Check hit left side of board
  bit 7, b
  jr nz, .revert2


  ;; If we're above the board, don't check for collision
  ld a, [falling_block_y]
  cp 2
  jr c, .save

  ;; Check hit block
  dec hl
  xor a
  or [hl]
  add hl, de
  or [hl]
  jr z, .save

.revert2:
  inc b

.save:
  ld hl, drop_pos
  ld a, [hl]
  cp b
  ld [hl], b

  jr z, .done
  jr nc, .moved_left
  ld hl, sfx_move_right
  jr .now_play
.moved_left:
  ld hl, sfx_move_left
.now_play:
  call play_sfx

.done:

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Perform a quick drop
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld hl, can_quickdrop

  ld a, [hPressedKeys]
  and PADF_DOWN
  or [hl]
  ld [hl], a

  ld a, [hHeldKeys]
  and [hl]
  jr nz, .do_fall

.no_down:

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Update falling block Y pos
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [falling_block_timer]
  dec a
IF DEF(DBG_DONTFALL)
  jr .no_fall
ELSE
  jr nz, .no_fall
ENDC

.do_fall:
  ;; Block needs to fall

  ld a, PADF_DOWN
  ld [can_quickdrop], a

  ld a, [falling_block_y]
  inc a
  ld [falling_block_y], a

  ld a, [falling_block_rate]

.no_fall:
  ld [falling_block_timer], a

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check if falling block collided
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [falling_block_y]
  sub 2

  ;; If we're above the board, don't check for collision
  jp c, .no_collide_other_block

  ;; If we're past the bottom of the board, we collided
  cp BOARD_H-1
  jr z, .block_collision

  ; If we're on the first row, don't check for collision
  or a
  jp z, .no_collide_other_block

  ;; Check if collided with other blocks
  inc a
  ld c, a
  ld a, [drop_pos]
  ld b, a

  call goto_xy_pos
  xor a
  or [hl]
  inc hl
  or [hl]

  jp z, .no_collide_other_block

.block_collision:
  ;; Block hit the bottom. Place there.

  ld hl, sfx_block_land_crunchy
  call play_sfx

  ld a, [falling_block_wait]
  ld hl, falling_block_timer
  add [hl]
  ld [hl], a

  ;; Reset the "down" held key
  xor a
  ld [can_quickdrop], a

  ld a, [falling_block_y]
  sub 3
  ld c, a
  ld a, [drop_pos]
  ld b, a

  call goto_xy_pos_with_vram

  ;; Load up the board AND playfield buffer with the new block tiles

  ld a, [block+0]
  ld [hl+], a
  ld a, [block+1]
  ld [hl], a

  ld a, 17
  add_a_to_hl

  ld a, [block+2]
  ld [hl+], a
  ld a, [block+3]
  ld [hl], a

  ;; A half of the block with nothing under it is about to fall. Flag it now,
  ;; since bombs get a chance to spread before the fall loop first moves it.
  call flag_unsupported_column ; right half
  dec l
  call flag_unsupported_column ; left half

  ;; Tiles that are resting on something have landed: check them for bomb
  ;; contact. (A falling half gets checked by the fall loop when it lands.)
  IF !DEF(DBG_NO_BOMB_PHYSICS)
  call bomb_check_if_settled ; bottom left
  inc l
  call bomb_check_if_settled ; bottom right
  ld a, l
  sub ROW
  ld l, a
  call bomb_check_if_settled ; top right
  dec l
  call bomb_check_if_settled ; top left
  ENDC

  ;; Reset falling block position
  xor a
  ld [falling_block_y], a

  ;; Paint block directly onto background to avoid flicker

  ;; TODO: Just paint the one BG that needs it, this solution is
  ;; currently ridiculous since it paints both unnecessarily

  ld a, [drop_pos]
  add_a_to_bc
  inc bc

  ld a, b
  xor %00000100
  ld h, a
  ld l, c

  ld de, $20-2

  ld a, IEF_STAT
  ldh [rIE], a
  ld a, STATF_MODE00
  ldh [rSTAT], a

  xor a
  ldh [rIF], a
  halt
  nop

  ld a, [block+0]
  and %10000011
  ld [bc], a
  inc bc
  ld [hl+], a
  ld a, [block+1]
  and %10000011
  ld [bc], a
  inc bc
  ld [hl+], a

  add hl, de
  ; dec hl
  ; dec hl

  ld a, h
  xor %00000100
  ld b, a
  ld c, l

  xor a
  ldh [rIF], a
  halt
  nop

  ld a, [block+2]
  and %10000011
  ld [bc], a
  inc bc
  ld [hl+], a
  ld a, [block+3]
  and %10000011
  ld [bc], a
  ld [hl+], a

  ld hl, block
  ld de, next_block1
  ld c, 4*2
  rst MemcpySmall

  ;; Generate a new random block
  ld hl, next_block2
  call generate_block

.no_collide_other_block:

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Mark tiles touching radar
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.radar_scan:
  ld a, [radar_pos+1]
  srl a
  jr z, .done_scanning ; don't scan if radar is 0
  srl a
  srl a
  ld b, a  ; x
  ld c, 0 ; y

  call goto_xy_pos

  ld c, BOARD_H
  ld de, ROW

  ld b, 0 ; marked a tile this loop?
.mark_loop:
  bit 6, [hl] ; Check if it is a marked tile
  jr z, .continue_scan
  bit 3, [hl] ; Leave it alone while it's still falling
  jr nz, .continue_scan
  ;; Turn it into the destroyable block. A bomb stays a bomb, so tiles that
  ;; land next to it after the radar has swept it still join the chain (see
  ;; `bomb_check_landed`, which never copies the swept state).
  ld a, [hl]
  or %110
  ld [hl], a
  ld b, 1
.continue_scan:
  add hl, de
  dec c
  jr nz, .mark_loop

  ;; Check if we need to perform a destroy
  ld a, [radar_marking_state]
  cp b
  jr z, .done_scanning
  ;; Current marking state and B are different
  or a
  jr z, .done_scanning
  ;; B is 0, which means we just stopped marking. Perform a destroy.
  ld [need_to_destroy], a ; A != B so A = 1

.done_scanning:
  ld a, b
  ld [radar_marking_state], a


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Update time
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [second_counter]
  inc a
  cp 30
  jr c, .no_wrap_second

  ld hl, time
  ld a, [hl]
  add 1
  daa
  cp $60
  ccf
  jr nz, .not_sixty
  xor a
  scf
.not_sixty:
  ld [hl+], a
  ld a, [hl]
  adc 0
  daa
  ld [hl], a

  xor a
.no_wrap_second:
  ld [second_counter], a

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Update score count
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; TODO: Refactor this, it sucks.

  ld c, 0

  ld hl, score_counter
  ld a, [hl]
  or a

  jr nz, .lsb_has_score

  inc hl
  or [hl]
  jr z, .counter_empty

  dec [hl]
  dec hl

  ld a, $FF
  ld c, 1
  ld [hl], a

.lsb_has_score:
  sub 8
  ld d, 8
  jr nc, .no_carry
  add 8

  ld d, a
  xor a

.no_carry:
  add c
  ld [hl], a

  ld hl, score
  ld a, d

  add [hl]
  daa
  ld [hl+], a

  ld a, 0
  adc [hl]
  daa
  ld [hl+], a

  ld a, 0
  adc [hl]
  daa
  ld [hl+], a

  ; ld a, 0
  ; adc [hl]
  ; daa
  ; ld [hl+], a

.counter_empty:

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Advance level if necessary
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [locked_level]
  or a
  jr nz, .curve_not_reached

  ld hl, score_curve+1
  ld a, [score+1]
  sub [hl]
  dec hl
  ld a, [score+2]
  sbc [hl]

  jr c, .curve_not_reached

.curve_reached:
  ld a, BANK(levels)
  ld [rROMB0], a

  ld hl, level_num
  ld a, [hl]
  inc a
  ld [hl], a

  ld hl, level_table
  add a
  add_a_to_hl
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  call load_level

.curve_not_reached:

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Refresh the graphics buffers
  ;; (shadow OAM and playfield buffer)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

update_graphics:
  ;; Upcoming block tiles
FOR OFS, 4
  ld a, [next_block1+OFS]
  and %11
  add $38
  spriteTile1 10+OFS
ENDR

FOR OFS, 4
  ld a, [next_block2+OFS]
  and %11
  add $38
  spriteTile1 14+OFS
ENDR

  ;; Radar position

  ld a, [radar_pos+1]
  add 10
  spriteX 3
  spriteX 4
  spriteX 5
  spriteX 6
  spriteX 7
  spriteX 8

  sub 8
  spriteX 0

  add 8
  spriteX 2

  add 8
  spriteX 1

  ;; Radar head number
  ld a, [num_destroyed]
  cp 9
  jr c, .no_cap
  ld a, 9
.no_cap: ; fr fr
  add a
  add $1C
  spriteTile2 2

  ;; Drop pos
  ld a, [drop_pos]
  add 2
  add a
  add a
  add a

  spriteX 9
  spriteX 10
  spriteX 11
  spriteX 12
  spriteX 13
  spriteX 14

  add 16

  ;; Falling block
  sub 16
  spriteX 15
  spriteX 17
  add 8
  spriteX 16
  spriteX 18

  ld a, [falling_block_y]
  add a
  add a
  add a
  jr nz, .no_correct
  inc a
.no_correct
  add 48
  spriteY 15
  spriteY 16
  spriteY 17
  spriteY 18


  ;; Time
  ld a, [time+1]
  ld e, a
  and $F0
  swap a
  add $12
  spriteTile1 26

  ld a, e
  and $F
  add $12
  spriteTile1 27

  ld a, [time+0]
  ld e, a
  and $F0
  swap a
  add $12
  spriteTile1 28

  ld a, e
  and $F
  add $12
  spriteTile1 29

  ;; Score
  ld a, [score+0]
  ld e, a
  and $F
  add $12
  spriteTile1 25
  ld a, e
  and $F0
  swap a
  add $12
  spriteTile1 24

  ld a, [score+1]
  ld e, a
  and $F
  add $12
  spriteTile1 23
  ld a, e
  and $F0
  swap a
  add $12
  spriteTile1 22

  ld a, [score+2]
  ld e, a
  and $F
  add $12
  spriteTile1 21
  ld a, e
  and $F0
  swap a
  add $12
  spriteTile1 20

  ;; RIP
  ; ld a, [score+3]
  ; ld e, a
  ; and $F
  ; add $12
  ; spriteTile1 19
  ; ld a, e
  ; and $F0
  ; swap a
  ; add $12
  ; spriteTile1 18

  ;; Level
  ld a, 19+(7*1)+8
  spriteX1 3

  ld a, [level_num]

  ld b, 0           ; Initialize B to store the tens digit
.loop_tens:
  sub 10             ; Compare A with 10
  jr c, .done_tens  ; If A < 10, skip the tens subtraction
  inc b             ; Increment B (the tens digit)
  jr .loop_tens     ; Repeat the loop for the next tens digit
.done_tens:
  add 10
  ld d, b           ; Move the tens digit to register D
  ld e, a           ; Move the remaining value (ones digit) to register E

  ld a, e
  add 8
  cp 9
  spriteTile1 3
  jr nz, .normal_num

  ld a, 19+(7*1)+8-1
  spriteX1 3

.normal_num:
  ld a, d
  add 8
  spriteTile1 2

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Process running animations
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Only run animations every other frame
  ld a, [frame_counter]
  and 1
  jp z, playfield_update

  ;; Clear animation OAM
  xor a
FOR SPR, $4C, $9C+4, 4
  ld [wShadowOAM2+SPR], a
ENDR

.animation_stuff:
  ld bc, initial_free_sprites

FOR OFS, 0, NUM_ANIMS*7, 7
  ld hl, animations+OFS
  call .animate
ENDR

  jr playfield_update

.animate:
  ld a, [hl+] ; enabled
  dec a
  ret nz

  ld a, [hl+]
  ld [anim_x_temp], a
  ld a, [hl+]
  ld [anim_y_temp], a
  ld a, [hl+]
  ld [anim_palette_temp], a

  push hl

  ld a, [hl+]
  ld h, [hl]
  ld l, a

  ld a, [hl+]
.process_anim:
  cp $AE ; Animation End
  jr nz, .anim_continue

  ;; animation is done
  pop de
  push hl

  ld h, d
  ld l, e

  ld de, -4
  add hl, de
  ld [hl], 0 ; disabled

  ret ; jp hl

.anim_continue:
  cp $FE ; Frame End
  jr nz, .anim_continue2
  ;; move to next frame
  ld d, h
  ld a, l
  pop hl
  ld [hl+], a
  ld [hl], d
  ret

.anim_continue2:
  ld a, [bc] ; sprite ID
  inc bc
  ld d, HIGH(wShadowOAM2)
  ld e, a

  ld a, [anim_y_temp]
  add [hl] ; Y
  inc hl
  IF !DEF(DBG_DONTANIMATE)
  ld [de], a
  ENDC
  inc e

  ld a, [anim_x_temp]
  add [hl] ; X
  inc hl
  IF !DEF(DBG_DONTANIMATE)
  ld [de], a
  ENDC
  inc e

  ld a, [hl+] ; tile
  IF !DEF(DBG_DONTANIMATE)
  ld [de], a
  ENDC
  inc e

  ld a, [anim_palette_temp]
  or [hl] ; flip attrs and palette
  inc hl
  IF !DEF(DBG_DONTANIMATE)
  ld [de], a
  ENDC

  ld a, [hl+]
  jr .process_anim ;.anim_continue

playfield_update::
  ld hl, board
  include "playfield_update.inc"

game_step_done:
  ret

game_step2::

  ;; Bombs don't need a pass of their own: they spread the moment a tile lands
  ;; (see `bomb_check_landed`, called from the fall loop and block placement).

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Make blocks fall and try to find matches
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if DEF(DBG_NO_BLOCK_PHYSICS)
  jp .end_frame_destroy
  ENDC

  ld bc, board.end-1 - ROW
  ld hl, board.end-1
  ;; mask for
  ;; high bit (is solid)
  ;; bomb bit and bomb carry
  ;; low bit (same color)
  ld e, %10110001
  xor a

.fall_loop:
  or [hl] ; load with flags
  jr nz, .try_find_match

.fall_block:
  ;; [hl] is empty. If there's a block above it, that block falls one cell.
  ld a, [bc]
  or a
  jr nz, .move_block
  dec l
  dec c
  jr nz, .fall_loop
.done:
  jp .end_frame_destroy

.move_block:
  ;; A = the block that is falling into [hl]. Work out whether it has landed:
  ;; it has if [hl] is on the floor, or the tile under [hl] is solid and isn't
  ;; itself still falling. (Everything below this row was already processed
  ;; this frame, so the falling bits down there are up to date.)
  and %11110111
  ld d, a ; assume it landed
  ld a, l
  cp LOW(board.end) - ROW
  jr nc, .place_block ; bottom row
  add ROW
  ld l, a
  ld a, [hl]
  and %10001000
  cp %10000000
  jr z, .landed
  set 3, d ; still falling
.landed:
  ld a, l
  sub ROW
  ld l, a
.place_block:
  xor a
  ld [bc], a
  ld [hl], d
  ;; If it has landed, this is the only moment a new bomb/non-bomb contact can
  ;; appear, so deal with it right here.
  IF !DEF(DBG_NO_BOMB_PHYSICS)
  bit 3, d
  call z, bomb_check_landed
  ENDC
  dec l
  xor a
  dec c
  jr nz, .fall_loop
  jp .end_frame_destroy

.diag:
  ;; The top left compare failed. A = $80 means it only failed because top
  ;; left and bottom right are both already marked (or top left is empty).
  ;; That is still a new match if top right or bottom left isn't marked yet.
  cp $80
  jr nz, .failed_match ; color or bomb mismatch
  inc c
  ld a, [bc] ; top right
  dec c
  and [hl] ; bottom left
  bit 6, a
  jr nz, .failed_match ; all four are marked already
  ld a, [bc]
  add a
  jr nc, .failed_match ; top left is empty
  jr .check_bottom_left

.no_take:
  dec h
  dec l
  dec c
  jr nz, .fall_loop
  jp .end_frame_destroy

.no_take2:
  dec l
  dec c
  jr z, .done

.failed_match:
  xor a
  or [hl]
  jr z, .fall_block

.try_find_match:
  ;; Check if it's a new match.

  ;; In order to be a match, all four blocks must be solid, the same color,
  ;; and not bombs, and at least one of them must not be marked already.

  ;; Top right and bottom left are compared against bottom right with `xor`,
  ;; which checks bit 7 (solid), bit 4 (bomb) and bit 0 (color) are equal.

  ;; Top left is compared with `add` instead. On the masked bits it behaves
  ;; like `xor`, except carries propagate: two bomb bits carry into bit 5,
  ;; and two marked bits (bit 6) carry into bit 7. Either one fails the
  ;; compare. The bomb case is a real failure. The marked case goes to `.diag`,
  ;; which only rejects the match if the other two blocks are marked as well.

  ld d, a

  ;; compare bottom right and top right
  ld a, [bc]
  xor d ; compare top and bottom bits
  and e
  jr nz, .no_take2

  ;; A = 0 here (the compare passed). If we're right on the left edge, don't
  ;; form a match.
  inc h ; the edge masks are one page above the board in ram
  or [hl]
  jr z, .no_take
  dec h

  dec c
  dec l

  ;; compare bottom right and top left
  ld a, [bc]
  add d ; not xor: we need the carries out of bit 4 and bit 6 (see above)
  and e
  jr nz, .diag

.check_bottom_left:
  ;; compare bottom right and bottom left
  ld a, d
  xor [hl] ; compare top and bottom bits
  and e
  jr nz, .failed_match

  ;;;;;;; match found

  ;; Check if the rightmost tile is touching the radar. If so, make ourselves
  ;; preemptively marked. This prevents the problem of detecting a match that is
  ;; half-detected by the radar already.
  ld a, [radar_pos+1]

  srl a ; /2
  srl a ; /4
  srl a ; /8

  ld e, a
  ld h, HIGH(x_lookup_table)
  inc l
  ld a, [hl-]
  cp e
  ld h, HIGH(board)

  ;; E = the bits to add to each block
  ld e, $80 | (1 << 6)
  jr nz, .not_radared

  ld e, $80 | (1 << 6) | %110 ; turn it into a marked block

  ld a, 1
  ld [radar_marking_state], a

.not_radared:
  ;; OR the bits into each block rather than storing one value in all four, so
  ;; blocks that were already marked or swept keep their state.
  ld a, [hl]
  or e
  ld [hl+], a ; bottom left
  ld a, [hl]
  or e
  ld [hl], a ; bottom right
  ld a, [bc]
  or e
  ld [bc], a ; top left
  inc c
  ld a, [bc]
  or e
  ld [bc], a ; top right

  ;; if we had to pre-mark the match, don't make the animation
  bit 1, e
  ld e, %10110001
  jr nz, .no_take2

  ;; unintuitive, but we reuse anim_y_temp as the palette param to `create_animation`
  and 1
  ld [anim_y_temp], a

  ;; unintuitive, but we reuse anim_x_temp as the "info" param to `create_animation`
  ld a, c
  ld [anim_x_temp], a

  ;; Split out XY coords from board pos ; TODO: Might be a good candidate for a lookup table...
  ld d, 0
  ld a, l
  dec a
.div_loop:
  inc d
  sub ROW
  jr nc, .div_loop

  add ROW
  add a
  add a
  add a
  sub 2
  ld e, a

  ld a, d
  add a
  add a
  add a
  add 47-8-1-8
  ld d, a

  ; E = X
  ; D = Y

  push hl
  push bc

  ld bc, anim_match_appear
  call create_animation

  ld hl, sfx_alt_radar_destroy_echo
  call play_sfx

  pop bc
  pop hl

  ld e, %10110001
  jp .no_take2

.end_frame_destroy:
  ld a, [need_to_destroy]
  or a
  call nz, perform_destroy

update_graphics2:
  ld a, [drop_pos]
  add a
  add a
  add a

  add 32

  spriteX 9
  spriteX 10
  spriteX 11
  spriteX 12
  spriteX 13
  spriteX 14

  ret

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Perform a destroy if necessary
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

perform_destroy:
  ld hl, board.end-1
  ld bc, board.end-1-ROW-1

  ld e, %110
.destroy_loop:
  ld a, [hl]
  and e
  cp e
  jr z, .do_destroy

.no_destroy:
  dec l
.no_destroy2:
  dec c
  jr nz, .destroy_loop
  jr .done_destroy_loop

.do_destroy:
  ;; Compare ignoring the bomb and falling bits: a swept bomb and a swept
  ;; normal block of the same color count as the same thing here.
  ld a, [bc]
  xor [hl]
  and %11100111
  ld [hl], 0
  jr nz, .no_destroy

  dec l
  ld a, [bc]
  xor [hl]
  and %11100111
  jr nz, .no_destroy2

  ;; Split out XY coords from board pos TODO: Might be a good candidate for a lookup table...
  ld d, 0
  ld a, l
  inc l
.div_loop2:
  inc d
  sub ROW
  jr nc, .div_loop2

  dec d
  dec a

  add ROW
  add a
  add a
  add a
  add 9
  ld e, a

  ld a, d
  add a
  add a
  add a
  add (47-8-1-8)+11
  ld d, a

  push hl
  push bc

  ld bc, anim_explosion
  xor a
  ld [anim_y_temp], a

  call create_animation

  ld hl, num_destroyed
  inc [hl]

  ld hl, sfx_test_explode
  call play_sfx

  pop bc
  pop hl

  ld e, %110
  jr .no_destroy

.done_destroy_loop:
  ;; clear out the last row on top, since we stop when the upper pointer hits
  ;; the end
  ld a, [hl]
  and e
  cp e
  jr nz, .no_clear
  ld [hl], 0
.no_clear
  dec l
  jr nz, .done_destroy_loop

  xor a
  ld [need_to_destroy], a
  jp flag_floating_tiles

;;; Creates an animation.
;;; Param: E = X
;;; Param: D = Y
;;; Param: BC = the animation
;;; Destroy: AF DE BC HL
create_animation:
  push de

  ;; Find an empty animation slot
  ld hl, animations-7
  ld de, 7
.seek_anim_loop:
  add hl, de

  bit 0, [hl] ; either currently running, or needs cleanup
  jr nz, .seek_anim_loop

  pop de

  ;; Cancel creating the animation if we are at the sentinel-- no free slots.
  bit 7, [hl]
  ret nz

  ld [hl], 1 ; enabled
  inc hl
  ld [hl], e ; x
  inc hl
  ld [hl], d ; y
  inc hl
  inc hl ; skip palette for now
  ld [hl], c ; LOW(the anim)
  inc hl
  ld [hl], b ; HIGH(the anim)
  inc hl

  ld a, [anim_x_temp]
  ld [hl-], a ; info

  dec hl
  dec hl ; back to palette

  ld a, [anim_y_temp]
  ld b, a
  swap b
  or b
  ld [hl+], a ; palette

  ret

;;; Generates a new block at the pointer in HL
;;; Param: HL = Pointer where you want the new block
;;; Return: [HL] = The new block
;;; Return: HL = HL+4
;;; Destroy: AF DE BC
generate_block:
  push hl
  call rand
  pop hl

REPT 4
  ld a, [rDIV]
  and %111
  push af

  ld a, c
  and 1
  or $80
  IF DEF(DBG_BLOCK)
  ld a, DBG_BLOCK
  ENDC

  ld d, a
  pop af
  ld a, d
  jr nz, :+
  ;; is bomb
  or %11010010
:
  ld [hl+], a
  rrc c
ENDR

  ret

;;; Sets some pointers to a block position in the board.
;;; Param: C = Y position on board
;;; Param: B = X position on board
;;; Return: BC = Pointer into VRAM of coord
;;; Return: HL = Pointer into board of coord
;;; Destroy: AF DE
goto_xy_pos_with_vram:
  xor a

  ld hl, $9CC0
  ld de, $20
.mult:
  add ROW
  add hl, de
  dec c
  jr nz, .mult

  add b

  ld bc, board
  add_a_to_bc

  push hl
  ld h, b
  ld l, c
  pop bc

  ret

;;; Flags a just-placed column of two tiles as falling if nothing is holding it
;;; up, using the same rule as the fall loop's `.move_block`.
;;; Param: HL = Pointer into board of the bottom tile of the column
;;; Return: HL = unchanged
;;; Destroy: AF E
flag_unsupported_column:
  ld a, l
  cp LOW(board.end) - ROW
  ret nc ; on the floor
  ld e, l
  add ROW
  ld l, a
  ld a, [hl]
  ld l, e
  and %10001000
  cp %10000000
  ret z ; resting on a tile that isn't falling
  set 3, [hl]
  ld a, l
  sub ROW
  ld l, a
  set 3, [hl]
  ld l, e
  ret

;;; Sets some pointers to a block position in the board.
;;; Param: C = Y position on board
;;; Param: B = X position on board
;;; Return: HL = Pointer into board of coord
;;; Destroy: AF BC
goto_xy_pos:
  ld a, c
  or a
  jr z, .done
  xor a

.mult:
  add ROW
  dec c
  jr nz, .mult

.done:
  add b
  ld hl, board
  add_a_to_hl

  ret

poll_joystick::
  ld c, LOW(rP1)
  ld a, $20 ; Select D-pad
  ldh [c], a
REPT 6
  ldh a, [c]
ENDR
  or $F0 ; Set 4 upper bits (give them consistency)
  ld b, a

  ; Filter impossible D-pad combinations
  and $0C ; Filter only Down and Up
  ld a, b
  jr nz, .notUpAndDown
  or $0C ; If both are pressed, "unpress" them
  ld b, a
.notUpAndDown
  and $03 ; Filter only Left and Right
  jr nz, .notLeftAndRight
  ; If both are pressed, "unpress" them
  inc b
  inc b
  inc b
.notLeftAndRight
  swap b ; Put D-pad buttons in upper nibble

  ld a, $10 ; Select buttons
  ldh [c], a
REPT 6
  ldh a, [c]
ENDR
  ; On SsAB held, soft-reset
  and $0F
  ; jr z, .perhapsReset
.dontReset

  or $F0 ; Set 4 upper bits
  xor b ; Mix with D-pad bits, and invert all bits (such that pressed=1) thanks to "or $F0"
  ld b, a

  ; Release joypad
  ld a, $30
  ldh [c], a

  ldh a, [hHeldKeys]
  cpl
  and b
  ldh [hPressedKeys], a
  ld a, b
  ldh [hHeldKeys], a
  ret

MACRO anim_sprite
  db $0 ; sprite update
  db \3 + 16; y
  db \2 + 8 ; x
  db (\4*2)+$42      ; tile
  db (\5 << 5) | (\6 << 6) ; flip flags
ENDM

MACRO anim_frame_end
  db $FE
ENDM

MACRO anim_end
  db $AE
ENDM

anim_bonus:
  anim_sprite 0,0,0,13,0,0
  anim_sprite 1,8,0,14,0,0
  anim_sprite 2,14,0,15,0,0
  anim_sprite 3,22,0,16,0,0
  anim_sprite 4,26,0,17,0,0
  anim_sprite 5,34,0,18,0,0
  anim_sprite 6,40,0,19,0,0
  anim_sprite 7,48,0,20,0,0
  anim_sprite 8,53,0,21,0,0
  anim_sprite 9,61,0,22,0,0
  anim_frame_end

  anim_sprite 0,2,0,13,0,0
  anim_sprite 1,10,0,14,0,0
  anim_sprite 2,15,0,15,0,0
  anim_sprite 3,23,0,16,0,0
  anim_sprite 4,26,0,17,0,0
  anim_sprite 5,34,0,18,0,0
  anim_sprite 6,39,0,19,0,0
  anim_sprite 7,47,0,20,0,0
  anim_sprite 8,51,0,21,0,0
  anim_sprite 9,59,0,22,0,0
  anim_frame_end

  anim_sprite 0,7,0,13,0,0
  anim_sprite 1,15,0,14,0,0
  anim_sprite 2,18,0,15,0,0
  anim_sprite 3,26,0,16,0,0
  anim_sprite 4,26,0,17,0,0
  anim_sprite 5,34,0,18,0,0
  anim_sprite 6,36,0,19,0,0
  anim_sprite 7,44,0,20,0,0
  anim_sprite 8,46,0,21,0,0
  anim_sprite 9,54,0,22,0,0
  anim_frame_end

  anim_sprite 0,16,0,13,0,0
  anim_sprite 1,24,0,14,0,0
  anim_sprite 2,22,0,15,0,0
  anim_sprite 3,30,0,16,0,0
  anim_sprite 4,26,0,17,0,0
  anim_sprite 5,34,0,18,0,0
  anim_sprite 6,32,0,19,0,0
  anim_sprite 7,40,0,20,0,0
  anim_sprite 8,36,0,21,0,0
  anim_sprite 9,44,0,22,0,0
  anim_frame_end

  anim_sprite 0,24,0,13,0,0
  anim_sprite 1,33,0,14,0,0
  anim_sprite 2,26,0,15,0,0
  anim_sprite 3,34,0,16,0,0
  anim_sprite 4,26,0,17,0,0
  anim_sprite 5,34,0,18,0,0
  anim_sprite 6,26,0,19,0,0
  anim_sprite 7,34,0,20,0,0
  anim_sprite 8,26,0,21,0,0
  anim_sprite 9,34,0,22,0,0
  anim_end
  ret

anim_explosion:
  anim_sprite 0,0,0,8,0,0
  anim_sprite 1,7,0,8,1,0
  anim_frame_end

  anim_sprite 0,0,0,9,0,0
  anim_sprite 1,7,0,9,1,0
  anim_frame_end

  anim_sprite 0,0,0,10,0,0
  anim_sprite 1,7,0,10,1,0
  anim_frame_end

  anim_sprite 0,0,0,11,0,0
  anim_sprite 1,7,0,11,1,0
  anim_frame_end

  anim_sprite 0,0,0,12,0,0
  anim_sprite 1,7,0,12,1,0
  anim_end
  ret

anim_match_appear:
  anim_sprite 0,0,0,0,0,0
  anim_sprite 1,12,0,0,1,0
  anim_sprite 2,4,-14,1,0,1
  anim_sprite 3,0,4,0,0,1
  anim_sprite 4,12,4,0,1,1
  anim_sprite 5,6,18,1,0,0
  anim_frame_end

  anim_sprite 0,1,1,0,0,0
  anim_sprite 1,11,1,0,1,0
  anim_sprite 2,5,-13,1,0,1
  anim_sprite 3,1,3,0,0,1
  anim_sprite 4,11,3,0,1,1
  anim_sprite 5,7,17,1,0,0
  anim_frame_end

  anim_sprite 0,2,2,2,0,0
  anim_sprite 1,10,2,3,0,0
  anim_sprite 2,-8,-120,0,0,0
  anim_sprite 3,-8,-120,0,0,0
  anim_sprite 4,-8,-120,0,0,0
  anim_sprite 5,-8,-120,0,0,0
  anim_frame_end

  anim_sprite 0,2,2,4,0,0
  anim_sprite 1,10,2,5,0,0
  anim_frame_end

  anim_sprite 0,2,2,6,0,0
  anim_sprite 1,10,2,7,0,0
  anim_frame_end

  anim_sprite 0,2,2,6,0,0
  anim_sprite 1,10,2,6,0,0
  anim_end
  ;; The animation is over: give the four tiles the solid "marked" graphic so
  ;; they look like the last frame of the anim.
  ;;
  ;; A lot can happen to those cells while the animation plays (the radar can
  ;; sweep them, a bomb can spread onto them, the square can be destroyed or
  ;; fall), so only touch a tile that is still marked, isn't a bomb, and still
  ;; has the normal graphic. Anything else is left exactly as it is.
  ld de, 6
  add hl, de
  ld l, [hl]
  ld h, HIGH(board)

  set_marked_graphic ; top right
  dec l
  set_marked_graphic ; top left

  ; move down one row
  ld a, l
  add ROW
  ld l, a

  set_marked_graphic ; bottom left
  inc l
  set_marked_graphic ; bottom right

  ret

SECTION "Bomb flood fill", ROM0
DEF FLOOD_MASK = %10011011 ; solid, not a bomb, not falling, not swept, color

;;; Flags every tile that has nothing solid under it as falling. Called after a
;;; destroy, so that the falling bit is right before anything else looks at it.
;;; Destroy: AF BC HL
flag_floating_tiles:
  ld hl, board.end-1 - ROW
  ld bc, board.end-1
.loop:
  ld a, [hl]
  or a
  jr z, .next
  ld a, [bc]
  and %10001000
  cp %10000000
  jr z, .next
  set 3, [hl]
.next:
  dec c
  dec l
  jr nz, .loop
  ret

;;; Runs the bomb check on every settled bomb on the board. Only needed when
;;; a board is loaded with bombs already on it.
;;; Destroy: AF HL
bomb_scan_all::
  ld hl, board.end-1
.loop:
  ld a, [hl]
  and %00011000
  cp %00010000
  call z, bomb_check_landed
  dec l
  jr nz, .loop
  ret

;;; Param: HL = pointer into board. Does nothing if that tile is still falling.
bomb_check_if_settled:
  bit 3, [hl]
  ret nz
  ;; fallthrough

;;; A tile has just come to rest at HL. If it is a bomb, spread it. If it is a
;;; normal tile touching a settled bomb of its color, the bomb spreads onto it
;;; (and through it).
;;; Param: HL = pointer into board of the tile
;;; Destroy: AF
bomb_check_landed:
  push bc
  push de
  ld a, [hl]
  bit 4, a
  jr nz, .is_bomb
  ld c, a
  ld d, %10011001
  ld e, l
  ;; up
  ld a, l
  sub ROW
  ld l, a
  ld a, [hl]
  xor c
  and d
  cp %00010000
  jr z, .found
  ;; down
  ld a, e
  add ROW
  ld l, a
  ld a, [hl]
  xor c
  and d
  cp %00010000
  jr z, .found
  ;; left, unless we're in column 0
  ld l, e
  inc h
  ld a, [hl]
  dec h
  or a
  jr z, .no_left
  dec l
  ld a, [hl]
  xor c
  and d
  cp %00010000
  jr z, .found
.no_left:
  ;; right, unless the next cell is column 0 of the next row
  ld l, e
  inc l
  inc h
  ld a, [hl]
  dec h
  or a
  jr z, .none
  ld a, [hl]
  xor c
  and d
  cp %00010000
  jr z, .found
.none:
  ld l, e
  pop de
  pop bc
  ret
.found:
  ld b, [hl]
  ld l, e
  res 2, b ; spread it as an unswept bomb, even if that one has been swept
  call flood_bomb
  pop de
  pop bc
  ret
.is_bomb:
  ld b, a
  res 2, b
  call flood_from
  pop de
  pop bc
  ret

;;; Turns the tile at HL, and every settled same-color tile connected to it,
;;; into the bomb B.
;;; Param: HL = pointer into board of the first tile (must be eligible)
;;; Param: B = the bomb value to write
;;; Destroy: AF
flood_bomb:
  ld [hl], b
  ;; fallthrough

;;; Same, but the tile at HL is already a bomb and is left as it is.
flood_from:
  push bc
  push de
  push hl
  ;; The fill keeps its to-do list on the stack: one entry per tile it turns,
  ;; so up to 2 bytes per board cell. STACK_SIZE allows for that.
  ld a, b
  and %10000001
  ld c, a ; what an eligible tile looks like under FLOOD_MASK
  ld d, FLOOD_MASK
  ld e, l
  ld l, 0
  push hl ; sentinel
  ld l, e
  push hl
.next:
  pop hl
  ld a, l
  or a
  jr z, .done
  ;; left, unless we're in column 0
  inc h
  ld a, [hl]
  dec h
  or a
  jr z, .no_left
  dec l
  ld a, [hl]
  xor c
  and d
  jr nz, .no_left2
  ld [hl], b
  push hl
.no_left2:
  inc l
.no_left:
  ;; right, unless the next cell is column 0 of the next row
  inc l
  inc h
  ld a, [hl]
  dec h
  or a
  jr z, .no_right
  ld a, [hl]
  xor c
  and d
  jr nz, .no_right
  ld [hl], b
  push hl
.no_right:
  ;; up
  ld a, l
  sub ROW+1
  ld l, a
  ld a, [hl]
  xor c
  and d
  jr nz, .no_up
  ld [hl], b
  push hl
.no_up:
  ;; down
  ld a, l
  add ROW*2
  ld l, a
  ld a, [hl]
  xor c
  and d
  jr nz, .next
  ld [hl], b
  push hl
  jr .next
.done:
  pop hl
  pop de
  pop bc
  ret
