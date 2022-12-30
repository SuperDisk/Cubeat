include "defines.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug toggles to make development easier
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; DEF DBG_BLOCK = $81
DEF DBG_DONTFALL = 1
; DEF DBG_DONTANIMATE = 1
DEF SELECT_PAUSES_RADAR = 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DEF DPAD_HOLD_FRAMES EQU 7

DEF BOARD_W EQU 18
DEF BOARD_H EQU 11
DEF ROW EQU 18

DEF NUM_ANIMS = 11

DEF EMPTY = 0
DEF WHITE = $80
DEF BLACK = $81
DEF WHITEBOMB = $E2
DEF BLACKBOMB = $E3
; there are more...

MACRO update_sprite2  ; which sprite, x, y, tile
  ld a, \3+16
  ld [wShadowOAM2+(4*\1)], a
  ld a, \2+8
  ld [wShadowOAM2+(4*\1)+1], a
  ld a, \4
  ld [wShadowOAM2+(4*\1)+2], a
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

MACRO add_a_to_r16
  add \2
  ld \2, a
  adc \1
  sub \2
  ld \1, a
ENDM

;; Thanks PinoBatch!
MACRO sub_from_r16 ;; (high, low, value)
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

MACRO add_a_to_bc
  add_a_to_r16 b, c
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

SECTION "Levels", ROM0

levels:
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

SECTION "Other constant data", ROM0

initial_free_sprites:
FOR SPR, $4C, $9C+4, 4
  db SPR
ENDR
.end:

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

; Maze
; db $00,$80,$80,$80,$80,$80,$00,$00,$00,$00,$00,$00,$00,$80,$80,$80,$80,$80
; db $00,$80,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$80
; db $00,$80,$00,$00,$00,$80,$00,$80,$80,$00,$00,$00,$00,$80,$00,$00,$00,$80
; db $00,$80,$00,$00,$00,$80,$80,$80,$80,$00,$00,$80,$80,$80,$00,$00,$00,$80
; db $00,$80,$00,$00,$00,$00,$00,$00,$80,$80,$00,$80,$00,$00,$00,$00,$00,$80
; db $00,$80,$00,$00,$00,$00,$00,$00,$00,$80,$00,$80,$00,$00,$00,$00,$00,$80
; db $00,$80,$80,$80,$80,$00,$00,$00,$00,$80,$00,$80,$00,$00,$00,$00,$00,$80
; db $00,$80,$80,$00,$80,$00,$80,$00,$80,$80,$00,$80,$00,$00,$00,$00,$00,$80
; db $00,$80,$80,$00,$80,$00,$80,$00,$80,$00,$00,$80,$00,$00,$00,$00,$00,$80
; db $00,$00,$00,$00,$80,$00,$80,$00,$80,$00,$00,$82,$00,$00,$00,$00,$00,$80
; db $00,$00,$00,$80,$80,$00,$80,$00,$80,$80,$80,$82,$80,$80,$80,$00,$00,$00

; V
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80
; db $80,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00
; db $00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00
; db $00,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00
; db $00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00
; db $00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$80,$80,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$80,$82,$80,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$82,$00,$00,$00,$00,$00,$00,$00,$00,$00

; X
; db $00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00
; db $00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$80,$80,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$80,$80,$80,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$00,$82,$00,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$00,$80,$80,$80,$00,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$00,$80,$80,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00
; db $00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00,$00
; db $00,$00,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$80,$80,$00,$00,$00,$00

;; Bomb in middle of all white
; db $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
; db $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
; db $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
; db $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
; db $81,$81,$81,$81,$81,$81,$81,$81,$E3,$81,$81,$81,$81,$81,$81,$81,$81,$81
; db $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
; db $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
; db $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
; db $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
; db $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
; db $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81

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
; REPT 11
; db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
; ENDR

;; All black
; REPT 11
; db $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81
; ENDR

SECTION "Board edge array", ROM0, ALIGN[8]

edge_array:
db 0
REPT 11
db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
ENDR

SECTION "Board edge array 2", ROM0, ALIGN[8]
edge_array2:
db 0
REPT 11
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
ENDR


SECTION "Game vars", WRAM0
score: ds 4 ; 7 digits
score_counter: dw
score_curve: dw ; needed score to advance to the next level (3 digits)

level_num: db

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
dw ; animation
db ; palette
db ; info
db ; how many sprites used
ds 8 ; sprites to use
ENDR
anim_end_sentinel: db

anim_x_temp: db
anim_y_temp: db
anim_palette_temp: db

anim_sprites_needed: db

free_sprites: ds initial_free_sprites.end - initial_free_sprites
free_sprites_count: db

bomb_row1: db
bomb_row2: db

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
IF DEF(SELECT_PAUSES_RADAR)
  ld [radar_paused], a
ENDC

  ; A = 0 here
  ld hl, animations
  ld c, 16*NUM_ANIMS
  rst MemsetSmall

  ld de, __test_board
  ld hl, board
  ld c, board.end - board
  rst MemcpySmall

  ld a, initial_free_sprites.end - initial_free_sprites
  ld [free_sprites_count], a
  ld hl, free_sprites
  ld de, initial_free_sprites
  ld c, a
  rst MemcpySmall

  ld a, LOW(board.end)-1
  ld [bomb_row1], a
  ld a, LOW(board) + (ROW-1)
  ld [bomb_row2], a

  ld a, $F0
  ld [anim_end_sentinel], a

  ld a, $FF
  ld [board_start_sentinel], a
  ld [board_end_sentinel], a

  ; Simulate out of sprites condition
  ; ld a, 6
  ; ld [free_sprites_count], a

  ld hl, levels
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
load_level:
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

  ld a, 10
  ld [anim_sprites_needed], a

  ld e, 7
  ld d, 75

  xor a
  ld [anim_y_temp], a
  ld [anim_x_temp], a

  ld bc, anim_bonus
  call create_animation

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
  ; a = 0
  ld [num_destroyed], a
  ld h, a
  ld l, a

  inc a
  ; a = 1
  ld [need_to_destroy], a
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
  inc a
  ld [dpad_frames], a
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
  ld a, b
  ld [drop_pos], a
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

  ld de, $20

  ld a, IEF_STAT
  ldh [rIE], a
  ld a, STATF_MODE00
  ldh [rSTAT], a

  xor a
  ldh [rIF], a
  halt
  nop

  ld a, [block+0]
  ld [bc], a
  inc bc
  ld [hl+], a
  ld a, [block+1]
  ld [bc], a
  inc bc
  ld [hl+], a

  add hl, de
  dec hl
  dec hl

  ld a, h
  xor %00000100
  ld b, a
  ld c, l

  xor a
  ldh [rIF], a
  halt
  nop

  ld a, [block+2]
  ld [bc], a
  inc bc
  ld [hl+], a
  ld a, [block+3]
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
  ld a, [hl]
  or %110 ; turn it into the destroyable block
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

  ld hl, score_curve+1
  ld a, [score+1]
  sub [hl]
  dec hl
  ld a, [score+2]
  sbc [hl]

  jr c, .curve_not_reached

.curve_reached:
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
  and 1
  add $38
  spriteTile1 10+OFS
ENDR

FOR OFS, 4
  ld a, [next_block2+OFS]
  and 1
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
  ld e, a

  and $F
  add 8
  cp 9
  spriteTile1 3
  jr nz, .normal_num

  ld a, 19+(7*1)+8-1
  spriteX1 3

.normal_num:
  ld a, e
  and $F0
  swap a
  add 8
  spriteTile1 2

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Process running animations
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Only run animations every other frame
  ld a, [frame_counter]
  and 1
  jp z, playfield_update

.animation_stuff:

FOR OFS, 0, NUM_ANIMS*16, 16
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

  push hl

  ld b, h
  ld c, l

  inc bc
  inc bc ; get past animation var

  ; now pointing at palette
  ld a, [bc]
  ld [anim_palette_temp], a

  inc bc ; info
  inc bc ; num sprites used
  inc bc ; sprites

  ld a, [hl+]
  ld h, [hl]
  ld l, a

.process_anim:
  ld a, [hl+]

  cp $AE ; Animation End
  jr nz, .anim_continue

  rst CallHL

  ;; animation is done
  ld hl, -8
  add hl, bc
  ld [hl], 3 ; cleanup flag
  pop af
  ret

.anim_continue:
  cp $FE ; Frame End
  jr nz, .anim_continue2
  ;; move to next frame
  ld b, h
  ld c, l
  pop hl
  ld [hl], c
  inc hl
  ld [hl], b
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
  ld a, [hl+] ; flip attrs and palette
  push hl
  ld hl, anim_palette_temp
  or [hl]
  pop hl
  IF !DEF(DBG_DONTANIMATE)
  ld [de], a
  ENDC

  ld a, [hl+]
  jr .anim_continue

playfield_update::
  ld hl, board
  include "playfield_update.inc"

game_step_done:
  ret

game_step2::

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Spread bombs to matching tiles
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [bomb_row1]
  ; call bomb_scan
  ld a, [bomb_row1]
  sub ROW
  jr nz, .no_reset_row1
  ld a, LOW(board.end)-1
.no_reset_row1:
  ld [bomb_row1], a

  ld a, [bomb_row2]
  ; call bomb_scan
  ld a, [bomb_row2]
  add ROW
  cp LOW(board.end-1)
  jr c, .no_reset_row2
  ld a, LOW(board) + (ROW-1)
.no_reset_row2:
  ld [bomb_row2], a

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Make blocks fall and try to find matches
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld bc, board.end-1 - ROW
  ld hl, board.end-1
  jr .fall_loop

.failed_match:
  ld a, c
  or a
  jp z, .perform_destroy ; TODO: make JR
.fall_loop:
  ld a, [hl]
  or a
  jr nz, .try_find_match

  ld a, [bc]
  ld [hl], a
  xor a
  ld [bc], a

.no_take:
  dec l
  dec c
  jr nz, .fall_loop ; TODO: make JR (reorder this code in general to save double comparisons)
  jp .perform_destroy

.try_find_match:
  ld h, HIGH(edge_array)
  bit 0, [hl]
  ld h, HIGH(board)
  jr nz, .no_take

  ld d, 0

  and 1
  ld e, a

  ld a, [bc]
  bit 7, a
  jr z, .no_take
  bit 6, a
  jr z, .top_right_not_marked
  set 6, d
.top_right_not_marked:
  and 1
  cp e
  jr nz, .no_take

  ;; First column matches
  dec c
  dec l

  ld a, [bc]
  bit 7, a
  jr z, .failed_match
  bit 6, a
  jr z, .top_left_not_marked
  bit 6, d
  jr nz, .failed_match
.top_left_not_marked:
  and 1
  cp e
  jr nz, .failed_match

  ld a, [hl]
  bit 7, a
  jr z, .failed_match
  and 1
  cp e
  jr nz, .failed_match

  ;; Second column matches

  or $80 | (1 << 6) ; TODO: Make this use the actual block, not replace it with the standard
  ld [hl+], a
  ld [bc], a
  inc c
  ld [hl], a
  ld [bc], a

  ld a, c
  ld [anim_x_temp], a
  ld a, e
  ld [anim_y_temp], a

  ;; Split out XY coords from board pos
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

  ld a, 6
  ld [anim_sprites_needed], a
  ld bc, anim_match_appear
  call create_animation

  pop bc
  pop hl

  jp .no_take

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Perform a destroy if necessary
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.perform_destroy:
  ld a, [need_to_destroy]
  or a
  jr z, update_graphics2

  ld hl, board.end-1
  ld bc, board.end-1-ROW-1

.destroy_loop:
  ld a, [hl]
  and %110
  cp %110
  jr nz, .no_destroy

  ld a, [bc]
  cp [hl]
  ld [hl], 0
  jr nz, .no_destroy

  dec l
  cp [hl]
  jr nz, .no_destroy2

  ;; Split out XY coords from board pos
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
  add 47-8-1-8
  add 11
  ld d, a

  push hl
  push bc

  ld a, 2
  ld [anim_sprites_needed], a
  ld bc, anim_explosion
  xor a
  ld [anim_y_temp], a

  call create_animation

  ld hl, num_destroyed
  inc [hl]

  pop bc
  pop hl

.no_destroy:
  dec l
.no_destroy2:
  dec c
  jr nz, .destroy_loop
.done_destroy_loop:
  xor a
  ld [need_to_destroy], a

update_graphics2:
  ld a, [drop_pos]
  add 2
  add a
  add a
  add a

  add 16

  spriteX 9
  spriteX 10
  spriteX 11
  spriteX 12
  spriteX 13
  spriteX 14

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Clean up any animations that need it
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FOR OFS, 0, (NUM_ANIMS)*16, 16
  ld hl, animations+OFS
  call .cleanup_anim
ENDR

  ret

.cleanup_anim:
  bit 1, [hl]
  ret z

  ld [hl], 0

  ld de, 7
  add hl, de

  ld b, HIGH(wShadowOAM2)
  xor a

  ld d, [hl]
  ld e, d
  inc hl
.hide_spr:
  ld c, [hl]
  inc hl
  ld [bc], a

  dec d
  jr nz, .hide_spr

  ld c, d ; c = d = 0
  ld b, e

  ;; Put the claimed sprites back in the free sprites list
  ld de, free_sprites
  ld a, [free_sprites_count]
  add_a_to_de

  dec hl
.free_sprite_loop:
  ld a, [hl-]
  cp $A0
  jr z, .skipA0 ; don't put dummy sprites back in the pool
  ld [de], a
  inc de
  inc c
.skipA0:
  dec b
  jr nz, .free_sprite_loop

  ld a, [free_sprites_count]
  add c
  ld [free_sprites_count], a

  ret

;;; Creates an animation.
;;; Param: E = X
;;; Param: D = Y
;;; Param: C = LOW(the anim)
;;; Param: B = HIGH(the anim)
;;; Destroy: AF DE BC HL
create_animation:
  push de

  ;; Find an empty animation slot
  ld hl, animations-16
  ld de, 16
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
  ld [hl], c ; LOW(the anim)
  inc hl
  ld [hl], b ; HIGH(the anim)
  inc hl

  ld a, [anim_y_temp]
  ld b, a
  swap b
  or b
  ld [hl+], a ; palette
  ld a, [anim_x_temp]
  ld [hl+], a ; info

  ;; find free sprites
  ld a, [anim_sprites_needed]
  ld d, a
  ld [hl+], a

  ld a, [free_sprites_count]
  sub d

  jr nc, .enough_sprites

  cpl
  inc a

.fetch_zero:
  ld [hl], $A0
  inc hl
  dec d
  dec a
  jr nz, .fetch_zero
  ld [free_sprites_count], a ; a = 0

  ld a, d
  or d
  ret z ; no need to even get real sprites

  ld bc, free_sprites
  jr .fetch_sprites

.enough_sprites:
  ld [free_sprites_count], a
  ld bc, free_sprites
  add_a_to_bc

.fetch_sprites:
  ld a, [bc]
  inc bc
  ld [hl+], a
  dec d
  jr nz, .fetch_sprites
  ret


;;; Performs a scan on a single row of the game board
;;; for bombs, and propagates them left, right, up and down.

;;; Param: A = Board index to start from (should be the far right side of the board)
;;; Destroy: AF BC DE HL
bomb_scan:
  ld h, HIGH(board)
  ld l, a
  ld c, BOARD_W-1
.bomb_loop:
  ld a, [hl]
  bit 5, a
  jr z, .continue_scan

  ;; Put bomb
  ld d, a
  and %10000001
  ld e, a

  dec l
  ld a, [hl]
  cp e
  jr nz, .no_left
  ld [hl], d

  inc l
.continue_scan:
  dec l
.no_left:
  dec c
  jr nz, .bomb_loop

part2:
  inc l
  ld c, BOARD_W
.bomb_loop:
  ld a, [hl]
  bit 5, a
  jr z, .continue_scan

  ;; put bomb
  ld h, HIGH(edge_array2)
  bit 0, [hl]
  ld h, HIGH(board)
  jr nz, .skip_right

  ld d, a
  and %10000001
  ld e, a

  inc l
  ld a, [hl]
  cp e
  jr nz, .no_right
  ld [hl], d
.no_right:
  dec l
.skip_right:

  ld a, l
  sub ROW
  ld l, a
  jr z, .no_up
  jr c, .no_up

  ld a, [hl]
  cp e
  jr nz, .no_up
  ld [hl], d

.no_up:
  ld a, l
  add ROW*2
  ld l, a

  cp LOW(board.end-1)
  jr nc, .no_down

  ld a, [hl]
  cp e
  jr nz, .no_down
  ld [hl], d

.no_down:
  ld a, -ROW
  add l
  ld l, a

.continue_scan:
  inc l
  dec c
  jr nz, .bomb_loop

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
  ld a, c
  and 1
  add $80
  IF DEF(DBG_BLOCK)
  ld a, DBG_BLOCK
  ENDC
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
  db (\4*2)+$40      ; tile
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
  anim_frame_end

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
  anim_frame_end

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
  anim_frame_end

  anim_end
  push bc
  push hl

  dec bc
  dec bc

  ld h, HIGH(board)

  ld a, [bc]
  ld l, a

  ld a, [hl]
  bit 6, a
  jr z, .fail ; TODO: is this right?
  or %100 ; make them the solid color (so it looks like the anim)
  ld [hl-], a

  ld a, [hl]
  bit 6, a
  jr z, .fail ; TODO: is this right?
  or %100 ; make them the solid color (so it looks like the anim)
  ld [hl+], a

  ; move down one row
  ld a, l
  add ROW
  ld l, a

  ld a, [hl]
  bit 6, a
  jr z, .fail ; TODO: is this right?
  or %100 ; make them the solid color (so it looks like the anim)
  ld [hl-], a

  ld a, [hl]
  bit 6, a
  jr z, .fail ; TODO: is this right?
  or %100 ; make them the solid color (so it looks like the anim)
  ld [hl], a

.fail:
  pop hl
  pop bc
  ret