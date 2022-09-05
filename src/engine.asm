include "defines.asm"

SECTION "Game vars", WRAM0
drop_pos: db

frame_counter: db
radar_pos: db

;; Board is 18 wide, 13 high (including the two tiles on top that are out of bounds)
board: ds (18*13)

;; Next piece

SECTION "Engine code", ROM0

game_step::
  ld hl, frame_counter
  inc [hl]
  inc hl
  assert frame_counter+1 == radar_pos
  inc [hl]



  ret