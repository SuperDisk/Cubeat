include "defines.asm"

SECTION "Music Playback Vars", WRAM0
vgm_offset: dw
bank: db

SECTION "Music Playback", ROM0

mus_step:
  ld hl, vgm_offset
  ld a, [hl+]
  ld h, [hl]
  ld l, a

  ld a, [bank]
  ld [rROMB0], a
  ld b, a

.step:
  ld a, [hl+]
  cp $5E
  jr z, .port0
  cp $5F
  jr z, .port1
  cp $61
  jr z, .wait
  cp $FF
  jr z, .bankswitch

  ;; couldnt find command
  jr .step

.bankswitch:
  ld a, [bank]
  inc a
  ld [bank], a
  ld [rROMB0], a
  ld hl, 0
  jr .step
.port1:
  ld a, [hl+]
  ld [$0003], a
  ld a, [hl+]
  ld [$0004], a
  jr .step
.port0:
  ld a, [hl+]
  ld [$0001], a
  ld a, [hl+]
  ld [$0002], a
  jr .step
.wait:
  ld a, [hl+]
  ld [rTMA], a
  inc hl
  ret