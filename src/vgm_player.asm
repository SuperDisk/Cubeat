include "defines.asm"

SECTION "Music vars", WRAM0
music_bank:: db
music_pointer:: dw

SECTION "Music Jumptable actions", ROM0, ALIGN[8]
jumptable:
  jr vgm_literals_end
  jr vgm_literals
  jr switch_bank
  ; jr switch_port0 -- not needed since switch_port0 immediately follows

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