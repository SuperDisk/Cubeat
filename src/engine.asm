include "defines.asm"

SECTION "Game vars", WRAM0
drop_pos: db

frame_counter: db
radar_pos: db

;; Board is 18 wide, 13 high (including the two tiles on top that are out of bounds)
board: ds (18*13)

;; Next piece

SECTION "Engine code", ROM0

spriteX: MACRO ; which sprite
  ld [wShadowOAM2+(4*\1)+1], a
ENDM

init_game::
  xor a
  ld [frame_counter], a
  ld [radar_pos], a

game_step::
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

  ret