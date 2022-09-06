include "defines.asm"

SECTION "Game vars", WRAM0
drop_pos: db

frame_counter: db
radar_pos: db

;; Board is 18 wide, 13 high (including the two tiles on top that are out of bounds)
board: ds (18*13)

;; Next piece

SECTION "Engine code", ROM0

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

spriteX: MACRO ; which sprite
  ld [wShadowOAM2+(4*\1)+1], a
ENDM

init_game::
  xor a
  ld [frame_counter], a
  ld [radar_pos], a
  ld [drop_pos], a

game_step::
  call poll_joystick

  ld a, [frame_counter]
  inc a
  ld [frame_counter], a

  ld a, [radar_pos]
  inc a
  cp 153
  jr nz, .no_reset_radar
  ld a, 8
.no_reset_radar:

  ld [radar_pos], a

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


  ld a, [hPressedKeys]
  ld h, a
  ld a, [drop_pos]

  bit 4, h
  jr z, .no_right
  add 8
.no_right:
  bit 5, h
  jr z, .no_left
  sub 8
.no_left:
  ld [drop_pos], a
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

.no_press:
  ret