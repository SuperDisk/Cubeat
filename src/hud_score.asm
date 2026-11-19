include "defines.asm"

;;; The in-game score display.
;;;
;;; The HUD row is at the 10 sprites per scanline limit, so the score can't
;;; have one sprite per digit. Instead its 8 digits are drawn, at a pitch of 6
;;; pixels, into a strip of SCORE_HUD_TILES tiles that 6 fixed sprites show.
;;;
;;; Each kernel loop `score_hud_prepare` works out which tiles of the strip
;;; changed and composes up to SCORE_HUD_BUDGET of them into a queue in RAM,
;;; and `score_hud_blit` copies the queue into VRAM during HBlanks.

DEF SCORE_HUD_TILES = 6
DEF SCORE_HUD_BUDGET = 3 ; tiles redrawn per kernel loop, at most
DEF SCORE_HUD_ROWS = 12  ; bytes per tile: the glyphs only use rows 1 to 6

assert SCORE_HUD_TILE % 16 == 0

SECTION "Score HUD RAM", WRAM0

score_shown: ds 8 ; the digits the strip shows (or is about to), leftmost first
score_dirty: db   ; bit N set = tile N of the strip needs to be redrawn
score_next: db    ; the tile to look at first next time...
score_next_mask: db ; ...and its bit in score_dirty

;; Tiles composed by score_hud_prepare, waiting for score_hud_blit. Each entry
;; is the low byte of the address in VRAM, then the SCORE_HUD_ROWS bytes.
score_queue_count: db
score_queue: ds SCORE_HUD_BUDGET * (1 + SCORE_HUD_ROWS)

SECTION "Score HUD", ROM0

;; Rows 1 to 6 of each digit. The glyphs are in the 5 rightmost columns.
score_font:
FOR I, 10
  incbin "res/numbers_8x8_only.2bppu", I * 16 + 2, SCORE_HUD_ROWS
ENDR

;;; Draws the whole score. Call with the LCD off.
;;; Destroy: AF BC DE HL
score_hud_init::
  xor a
  ld [score_dirty], a
  ld [score_queue_count], a
  ld hl, $8000 + SCORE_HUD_TILE * 16
  ld c, SCORE_HUD_TILES * 16
  rst MemsetSmall

  ;; No digit is $FF, so this makes every tile dirty
  ld a, $FF
  ld hl, score_shown
  ld c, 8
  rst MemsetSmall

  ld a, SCORE_HUD_TILES - 1
  ld [score_next], a
  ld a, 1 << (SCORE_HUD_TILES - 1)
  ld [score_next_mask], a

REPT SCORE_HUD_TILES / SCORE_HUD_BUDGET
  call score_hud_prepare
  call .copy
ENDR
  ret

.copy:
  ld a, [score_queue_count]
  or a
  ret z
  ld b, a
  ld hl, score_queue
.copy_tile:
  ld a, [hl+]
  ld e, a
  ld d, HIGH($8000 + SCORE_HUD_TILE * 16)
  ld c, SCORE_HUD_ROWS
.copy_row:
  ld a, [hl+]
  ld [de], a
  inc e
  dec c
  jr nz, .copy_row
  dec b
  jr nz, .copy_tile
  xor a
  ld [score_queue_count], a
  ret

;;; Copies the tiles queued by score_hud_prepare into VRAM, half a tile per
;;; HBlank. Call with only the mode 0 STAT interrupt selected in IE, and with
;;; interrupts disabled.
;;; Destroy: AF BC DE HL
score_hud_blit::
  ld a, [score_queue_count]
  or a
  ret z
  ld b, a
  ld hl, score_queue
.tile:
  ld a, [hl+]
  ld e, a
  ld d, HIGH($8000 + SCORE_HUD_TILE * 16)
REPT 2
  xor a
  ldh [rIF], a
  halt
  nop
  REPT SCORE_HUD_ROWS / 2
  ld a, [hl+]
  ld [de], a
  inc e
  ENDR
ENDR
  dec b
  jr nz, .tile
  xor a
  ld [score_queue_count], a
  ret

;;; Finds the tiles of the strip that `score` has changed and queues up to
;;; SCORE_HUD_BUDGET of them. Doesn't touch VRAM.
;;; Destroy: AF BC DE HL
score_hud_prepare::
  ld a, [score_dirty]
  ld b, a

  ;; Digit I starts at column 6*I+1 of the strip, so most digits are split
  ;; across two tiles.
FOR I, 8
  ld a, [score + 3 - I / 2]
  IF I % 2 == 0
  swap a
  ENDC
  and $F
  ld hl, score_shown + I
  cp [hl]
  jr z, :+
  ld [hl], a
  ld a, b
  or (1 << ((6 * I + 1) / 8)) | (1 << ((6 * I + 5) / 8))
  ld b, a
:
ENDR

  ;; Go through the tiles from right to left, starting where the last call
  ;; stopped so that a score that keeps counting up can't starve the tiles on
  ;; the left.
  ;; B = dirty tiles, C = budget left, D = bit of tile E, HL = queue
  ld hl, score_queue
  ld c, SCORE_HUD_BUDGET
  ld a, [score_next]
  ld e, a
  ld a, [score_next_mask]
  ld d, a

.scan:
  ld a, b
  or a
  jr z, .all_clean
  ld a, c
  or a
  jr z, .done
  ld a, b
  and d
  jr z, .next

  xor b
  ld b, a
  dec c

  ld a, e
  swap a
  add 2 ; skip row 0
  ld [hl+], a

  push bc
  push de
  push hl
  ld a, e
  call score_compose_tile
  pop hl
  ld a, l
  add SCORE_HUD_ROWS
  ld l, a
  adc h
  sub l
  ld h, a
  pop de
  pop bc

.next:
  dec e
  srl d
  jr nz, .scan
.all_clean:
  ld e, SCORE_HUD_TILES - 1
  ld d, 1 << (SCORE_HUD_TILES - 1)
  ld a, b
  or a
  jr nz, .scan

.done:
  ld a, e
  ld [score_next], a
  ld a, d
  ld [score_next_mask], a
  ld a, b
  ld [score_dirty], a
  ld a, SCORE_HUD_BUDGET
  sub c
  ld [score_queue_count], a
  ret

;;; Composes a tile of the strip from the digits in score_shown.
;;;
;;; The strip repeats every 3 tiles / 4 digits, and each tile is made of two
;;; digits: one gives the columns on the left and the other the ones on the
;;; right.
;;;
;;;   tile 0: digit 0 in columns 1-5, the first column of digit 1 in column 7
;;;   tile 1: the rest of digit 1 in columns 0-3, 3 columns of digit 2 in 5-7
;;;   tile 2: the rest of digit 2 in columns 0-1, digit 3 in columns 3-7
;;;
;;; Param: A = which tile (0 to SCORE_HUD_TILES-1)
;;; Param: HL = where to put the SCORE_HUD_ROWS bytes
;;; Destroy: AF BC DE HL
score_compose_tile:
  ld c, a
  add a
  add LOW(.digits)
  ld e, a
  adc HIGH(.digits)
  sub e
  ld d, a

  ld a, [de]
  ld b, a
  inc de
  ld a, [de]
  push af ; the digit on the right

  ;; C = which of the 3 kinds of tile
  ld a, c
  cp 3
  jr c, :+
  sub 3
:
  ld c, a

  ld a, b
  call .glyph
  push hl
  ld a, c
  or a
  jr z, .left0
  dec a
  jr z, .left1
  call .shl6
  jr .right
.left0:
  call .shl2
  jr .right
.left1:
  call .shl4

.right:
  pop hl
  pop af
  call .glyph
  ld a, c
  or a
  jr z, .or_shr4
  dec a
  jr z, .or_shr2
  ;; fallthrough

;; The digit as it is, on top of what is there
  ld b, SCORE_HUD_ROWS
:
  ld a, [de]
  inc de
  or [hl]
  ld [hl+], a
  dec b
  jr nz, :-
  ret

.or_shr4:
  ld b, SCORE_HUD_ROWS
:
  ld a, [de]
  inc de
  swap a
  and %00000001
  or [hl]
  ld [hl+], a
  dec b
  jr nz, :-
  ret

.or_shr2:
  ld b, SCORE_HUD_ROWS
:
  ld a, [de]
  inc de
  rrca
  rrca
  and %00000111
  or [hl]
  ld [hl+], a
  dec b
  jr nz, :-
  ret

.shl2:
  ld b, SCORE_HUD_ROWS
:
  ld a, [de]
  inc de
  add a
  add a
  ld [hl+], a
  dec b
  jr nz, :-
  ret

.shl4:
  ld b, SCORE_HUD_ROWS
:
  ld a, [de]
  inc de
  swap a
  and %11110000
  ld [hl+], a
  dec b
  jr nz, :-
  ret

.shl6:
  ld b, SCORE_HUD_ROWS
:
  ld a, [de]
  inc de
  rrca
  rrca
  and %11000000
  ld [hl+], a
  dec b
  jr nz, :-
  ret

;; Param: A = which digit of the score (0 to 7)
;; Return: DE = its rows in score_font
;; Destroy: AF
.glyph:
  add LOW(score_shown)
  ld e, a
  adc HIGH(score_shown)
  sub e
  ld d, a
  ld a, [de]

  ld e, a
  add a
  add e
  add a
  add a ; *= SCORE_HUD_ROWS
  add LOW(score_font)
  ld e, a
  adc HIGH(score_font)
  sub e
  ld d, a
  ret

;; The digits on the left and on the right of each tile
.digits:
  db 0, 1
  db 1, 2
  db 2, 3
  db 4, 5
  db 5, 6
  db 6, 7
