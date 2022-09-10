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

SECTION "Game vars", WRAM0
drop_pos: db

frame_counter: db
radar_pos: db

falling_block_rate: db
falling_block_timer: db
falling_block_y: db

dpad_frames: db

;; Board is 18 wide, 11 high (including the two tiles on top that are out of bounds)
board: ds (18*11)
.end:

SECTION "Engine code", ROM0

DEF DPAD_HOLD_FRAMES EQU 7

init_game::
  xor a
  ld [frame_counter], a
  ld [radar_pos], a
  ld [drop_pos], a
  ld [falling_block_y], a

  ld hl, board
  ld bc, board.end - board
  call Memset

  ld a, 18
  ld [falling_block_rate], a
  ld [falling_block_timer], a

  ld a, DPAD_HOLD_FRAMES
  ld [dpad_frames], a

  ret

game_step::
  call poll_joystick

  ;; Increment frame counter
  ld a, [frame_counter]
  inc a
  ld [frame_counter], a

  ;; Move radar right
  ld a, [radar_pos]
  inc a
  cp 153
  jr nz, .no_reset_radar
  ld a, 8
.no_reset_radar:
  ld [radar_pos], a

  ld a, [hHeldKeys]
  and %00110000
  jr nz, .did_hold_key

.no_hold_key:
  ld a, DPAD_HOLD_FRAMES
  ld [dpad_frames], a
  jr .done

.did_hold_key:
  ;; User is pressing a direction key, prepare to
  ;; rapidly move the block
  ld a, [dpad_frames]
  dec a
  jr nz, .no_slide
  inc a
  ld [dpad_frames], a
  ld a, [hHeldKeys]
  jr .do_slide

.no_slide:
  ld [dpad_frames], a

  ;; Move the drop position around
  ld a, [hPressedKeys]
.do_slide:
  ld h, a
  ld a, [drop_pos]

  bit 4, h
  jr z, .no_right

  ;; Player pressed right
  inc a
  cp 17
  jr nz, .save
  dec a

.no_right:
  bit 5, h
  jr z, .done

  ;; Player pressed left
  dec a
  bit 7, a
  jr z, .save
  inc a

.save:
  ld [drop_pos], a
.done:

  ;; Update falling block position
  ld a, [falling_block_timer]
  dec a
  jr nz, .no_fall

  ;; Block needs to fall
  ld a, [falling_block_y]
  inc a
  ld [falling_block_y], a

  ld a, [falling_block_rate]

.no_fall:
  ld [falling_block_timer], a

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
