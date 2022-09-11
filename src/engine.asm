include "defines.asm"

update_sprite2: macro ; which sprite, x, y, tile
  ld a, \3+16
  ld [wShadowOAM2+(4*\1)], a
  ld a, \2+8
  ld [wShadowOAM2+(4*\1)+1], a
  ld a, \4
  ld [wShadowOAM2+(4*\1)+2], a
endm

spriteX: MACRO ; which sprite
  ld [wShadowOAM2+(4*\1)+1], a
ENDM

spriteY: MACRO ; which sprite
  ld [wShadowOAM2+(4*\1)], a
ENDM

previewTile: macro ; which sprite, x, y, tile
  ld [wShadowOAM+(4*\1)+2], a
endm

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

add_a_to_bc: MACRO
    add_a_to_r16 b, c
ENDM

SECTION "Game vars", WRAM0
drop_pos: db

frame_counter: db
radar_pos: db

falling_block_rate: db
falling_block_timer: db
falling_block_y: db

dpad_frames: db

block: ds 4

;; Board is 18 wide, 11 high (including the two tiles on top that are out of bounds)
board: ds (18*11)
.end:

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
  ;; Move the drop position around
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld a, [drop_pos]
  ld b, a
  ld a, [falling_block_y]
  sub 2
  ld c, a
  call goto_xy_pos
  ld de, ROW

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

  jr z, .no_collide_other_block

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

.no_collide_other_block:

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Make blocks fall
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ld c, BOARD_W
  push bc

  ld b, 0
  ld c, BOARD_H-1
  call goto_xy_pos

  ld de, -ROW

.walk_right_row:
  ld c, 9

.walk_up_column:
  ld a, [hl]
  or a
  jr nz, .cant_take

  push hl
  add hl, de
  ld a, [hl]
  ld [hl], 0
  pop hl
  ld [hl], a
.cant_take:
  add hl, de
  dec c
  jr nz, .walk_up_column

  pop bc
  dec c
  jr z, .walk_done
  push bc

  inc hl
  ld a, ROW*9
  add_a_to_hl
  jr .walk_right_row

.walk_done:

update_graphics:
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

  spriteX 15
  spriteX 16
  spriteX 17
  spriteX 18
  spriteX 19
  spriteX 20

  ;; Falling block
  sub 16
  spriteX 21
  add 8
  spriteX 22

  ld a, [falling_block_y]
  add a
  add a
  add a
  add 48
  spriteY 22
  spriteY 21

.playfield_update:
  ld hl, board
  include "playfield_update.inc"

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
  xor a

.mult:
  add ROW
  dec c
  jr nz, .mult

  add b

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
