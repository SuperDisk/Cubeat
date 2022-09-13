include "defines.asm"

macro update_sprite2  ; which sprite, x, y, tile
  ld a, \3+16
  ld [wShadowOAM2+(4*\1)], a
  ld a, \2+8
  ld [wShadowOAM2+(4*\1)+1], a
  ld a, \4
  ld [wShadowOAM2+(4*\1)+2], a
endm

MACRO spriteX ; which sprite
  ld [wShadowOAM2+(4*\1)+1], a
ENDM

MACRO spriteY ; which sprite
  ld [wShadowOAM2+(4*\1)], a
ENDM

MACRO spriteTile1 ; tile
  ld [wShadowOAM+(4*\1)+2], a
endm

MACRO spriteTile2 ; tile
  ld [wShadowOAM2+(4*\1)+2], a
endm

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

SECTION "Board", WRAM0, ALIGN[8]

db

;; Board is 18 wide, 11 high (including the two tiles on top that are out of bounds)
board: ds (18*11)
.end:

SECTION "Game vars", WRAM0
drop_pos: db

frame_counter: db
radar_pos: db

falling_block_rate: db
falling_block_timer: db
falling_block_y: db

dpad_frames: db

next_block1: ds 4
next_block2: ds 4
block: ds 4

animations:
db ; running
db ; xoff
db ; yoff
dw ; animation
ds 8 ; sprites to use

anim_x_temp: db
anim_y_temp: db

SECTION "Engine code", ROM0

DEF DPAD_HOLD_FRAMES EQU 7
DEF BOARD_W EQU 18
DEF BOARD_H EQU 11

DEF ROW EQU 18

init_game::
  xor a
  ld [frame_counter], a
  ld [radar_pos], a
  ld [drop_pos], a
  ld [falling_block_y], a
  ld [animations], a

  ld hl, board
  ld bc, board.end - board
  call Memset

  ld a, 9
  ld [falling_block_rate], a
  ld [falling_block_timer], a

  ld a, DPAD_HOLD_FRAMES
  ld [dpad_frames], a

  ld a, $81
  ld [block+0], a
  ld a, $81
  ld [block+1], a
  ld a, $80
  ld [block+2], a
  ld a, $80
  ld [block+3], a

  ret

game_step::
  call poll_joystick

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Increment frame counter
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [frame_counter]
  inc a
  ld [frame_counter], a


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Move radar right
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [radar_pos]
  inc a
  cp 153
  jr nz, .no_reset_radar
  ld a, 8
.no_reset_radar:
  ld [radar_pos], a

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Rotate piece
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ld a, [hHeldKeys]
  bit PADB_DOWN, a
  jr nz, .do_fall

.no_down:

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Update falling block Y pos
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [falling_block_timer]
  dec a
  jr nz, .no_fall

.do_fall:

  ;; Block needs to fall
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

  ; If we're sitting on the bottom row, don't check for collision
  cp BOARD_H-2
  jp z, .no_collide_other_block

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

  di

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

  ;; Generate random new block (TODO: queue)
  ld a, [rDIV]
  ld hl, randstate
  ld [hl+], a
  ld [hl+], a
  ld a, [frame_counter]
  ld [hl+], a
  ld [hl], a
  call rand

  ld a, c
  and 1
  add $80
  ld [block+0], a
  rrc c

  ld a, c
  and 1
  add $80
  ld [block+1], a
  rrc c

  ld a, c
  and 1
  add $80
  ld [block+2], a
  rrc c

  ld a, c
  and 1
  add $80
  ld [block+3], a
  rrc c

.no_collide_other_block:

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Make blocks fall
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld bc, board+((BOARD_W*BOARD_H)-ROW)-1
  ld h, 0
  ld l, ROW
  add hl, bc
  jr .fall_loop

.failed_match:
  ld a, c
  or a
  jr z, update_graphics
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
  jr z, update_graphics
.no_dec:
  jr .fall_loop

.try_find_match:
  ld d, 0

  res 6, a
  ld e, a

  ld a, [bc]
  bit 6, a
  jr z, .top_right_not_marked
  set 6, d
  res 6, a
.top_right_not_marked:
  cp e
  jr nz, .no_take

  ;; First column matches
  dec c
  dec l

  ld a, [bc]
  bit 6, a
  jr z, .top_left_not_marked
  bit 6, d
  res 6, a
  jr nz, .failed_match
.top_left_not_marked:
  cp e
  jr nz, .failed_match

  ld a, [hl]
  res 6, a
  cp e
  jr nz, .failed_match

  ;; Second column matches

  set 6, a
  ld [hl+], a
  ld [bc], a
  inc c
  ld [hl], a
  ld [bc], a

  ;; Split out XY coords from board pos
  ld d, 0
  ld a, l
.div_loop:
  inc d
  sub 18
  jr nc, .div_loop

  add 17
  add a
  add a
  add a
  dec a
  dec a
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
  ld hl, animations
  ld a, 1
  ld [hl+], a

  ld a, e
  ld [hl+], a ; x
  ld a, d
  ld [hl+], a ; y
  ld a, LOW(anim_match_appear)
  ld [hl+], a
  ld a, HIGH(anim_match_appear)
  ld [hl+], a

  ld a, 19
  ld [hl+], a
  ld a, 20
  ld [hl+], a
  ld a, 21
  ld [hl+], a
  ld a, 22
  ld [hl+], a
  ld a, 23
  ld [hl+], a
  ld a, 24
  ld [hl+], a
  pop hl

  jr .no_take

update_graphics:
  ;; Dropping block tiles
  ld a, [block+0]
  sub $80
  add a
  add $38
  spriteTile2 15

  ld a, [block+1]
  sub $80
  add a
  add $38
  spriteTile2 16

  ld a, [block+2]
  sub $80
  add a
  add $38
  spriteTile2 17

  ld a, [block+3]
  sub $80
  add a
  add $38
  spriteTile2 18

  ;; Radar position

  ld a, [radar_pos]
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

  ; spriteX 15
  ; spriteX 16
  ; spriteX 17
  ; spriteX 18
  ; spriteX 19
  ; spriteX 20

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
  add 48
  spriteY 16
  spriteY 15
  add 8
  spriteY 17
  spriteY 18

  ld a, [frame_counter]
  and 1
  jr z, .playfield_update


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Process running animations
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld hl, animations
  ld a, [hl+]
  or a
  jr nz, .animate

  ; TODO: go to the next one

  jr .playfield_update

.animate:
  ld a, [hl+]
  ld [anim_x_temp], a
  ld a, [hl+]
  ld [anim_y_temp], a

  push hl

  ld b, h
  ld c, l
  inc bc
  inc bc ; sprites

  ld a, [hl+]
  ld h, [hl]
  ld l, a

.process_anim:
  ld a, [hl+]

  cp $AE ; Animation End
  jr nz, .anim_continue
  ;; animation is done
  ld hl, -5
  add hl, bc
  ld [hl], 0
  pop af
  jr .playfield_update

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
  jr .playfield_update

.anim_continue2:
  cp $C0 ; Code block
  jr nz, .anim_continue3

.anim_continue3:
  ld a, [bc] ; sprite ID
  inc bc
  add a
  add a
  ld de, wShadowOAM2
  add_a_to_de

  ld a, [anim_y_temp]
  add [hl] ; Y
  inc hl
  ld [de], a
  inc de

  ld a, [anim_x_temp]
  add [hl] ; X
  inc hl
  ld [de], a
  inc de

  ld a, [hl+] ; tile
  ld [de], a
  inc de
  ld a, [hl+] ; flip attrs
  ld [de], a

  ld a, [hl+]
  jr .anim_continue

.playfield_update:
  ld hl, board
  include "playfield_update.inc"

game_step_done:
  ret

game_step2::
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

  ret

goto_xy_pos_with_vram:
;;; Sets some pointers to a block position in the board.
;;; Param: C = Y position on board
;;; Param: B = X position on board
;;; Return: BC = Pointer into VRAM of coord
;;; Return: HL = Pointer into board of coord
;;; Destroy: AF DE
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

goto_xy_pos:
;;; Sets some pointers to a block position in the board.
;;; Param: C = Y position on board
;;; Param: B = X position on board
;;; Return: HL = Pointer into board of coord
;;; Destroy: AF BC
  ld a, c
  or a
  ld a, b
  jr z, .done
  xor a

.mult:
  add ROW
  dec c
  jr nz, .mult

  add b

.done:
  ld hl, board
  add_a_to_hl

  ret

poll_joystick:
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
  db (\4*2)+$3f+1      ; tile
  db (\5 << 5) | (\6 << 6) ; flip flags
ENDM

MACRO anim_frame_end
  db $FE
ENDM

MACRO anim_end
  db $AE
ENDM

MACRO anim_code
  db $C0
ENDM

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

  ; anim_code
  ; ld a, 1


  anim_end