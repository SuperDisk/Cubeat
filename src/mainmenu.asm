include "defines.asm"

MACRO add_a_to_r16
  add \2
  ld \2, a
  adc \1
  sub \2
  ld \1, a
ENDM

MACRO add_a_to_hl
  add_a_to_r16 h, l
ENDM

MACRO update_sprite  ; which sprite, x, y, tile
  ld a, \3+16
  ld [wShadowOAM+(4*\1)], a
  ld a, \2+8
  ld [wShadowOAM+(4*\1)+1], a
  ld a, \4
  ld [wShadowOAM+(4*\1)+2], a
  ld a, \5
  ld [wShadowOAM+(4*\1)+3], a
ENDM

MACRO getspriteX ; which sprite
  ld a, [wShadowOAM+(4*(\1))+1]
ENDM

MACRO spriteX ; which sprite
  ld [wShadowOAM+(4*(\1))+1], a
ENDM

MACRO getspriteY ; which sprite
  ld a, [wShadowOAM+(4*(\1))]
ENDM

MACRO spriteY ; which sprite
  ld [wShadowOAM+(4*(\1))], a
ENDM

SECTION "Main Menu Graphics", ROMX

main_menu_bg:
incbin "res/menu/main_menu.2bppu"
.end:

main_menu_map:
incbin "res/menu/main_menu.tilemapu"
.end:

cursor_sprite:
incbin "res/menu/cursor.2bpp"
.end:

SECTION "Tweening vars", WRAM0
menu_frame_counter: db

tween_start_coords:

tween_startx1: db
tween_startx2: db
tween_startx3: db
tween_startx4: db

tween_starty1: db
tween_starty2: db
tween_starty3: db
tween_starty4: db

tween_end_coords:

tween_endx1: db
tween_endx2: db
tween_endx3: db
tween_endx4: db

tween_endy1: db
tween_endy2: db
tween_endy3: db
tween_endy4: db

coords:
x1: db
x2: db
x3: db
x4: db

y1: db
y2: db
y3: db
y4: db

tween_dist: db

tween_step: db
tweening: db

bleh: db

SECTION "Main Menu", ROM0

play_coords:
.x: db 15, 82, 15, 82
.y: db 39, 39, 74, 74
.end:

select_level_coords:
.x: db 88, 131, 88, 131
.y: db 48, 48, 74, 74

credits_coords:
.x: db 87, 121, 88, 121
.y: db 80, 80, 91, 91

music_player_coords:
.x:
.y:


MainMenu::
	xor a
	ld [hLCDC], a
.wait_lcdc_off:
  ld a, [rLCDC]
  and %10000000
  jr nz, .wait_lcdc_off

  di

  ;; Setup tweening
  xor a
  ld [menu_frame_counter], a
  ld [tween_step], a
  ld [tweening], a
  ld [bleh], a

  ld de, play_coords
  ld hl, coords
  ld c, (play_coords.end - play_coords)
  rst MemcpySmall

  ;; Load BG area
  ld a, BANK(main_menu_bg)
  ld [rROMB0], a
  ld de, main_menu_bg
  ld hl, $9000
  ld bc, (main_menu_bg.end - main_menu_bg)
  call Memcpy

  lb bc, SCRN_X_B, SCRN_Y_B
  ld de, $9800
  ld hl, main_menu_map
  call MapRegion

  ;; Load sprite area
  ld de, cursor_sprite
  ld hl, $8000
  ld c, (cursor_sprite.end - cursor_sprite)
  rst MemcpySmall

  ;; Setup sprites
  update_sprite 0, 15, 39, 0, 0
  update_sprite 1, 82, 39, 0, OAMF_XFLIP
  update_sprite 2, 15, 74, 0, OAMF_YFLIP
  update_sprite 3, 82, 74, 0, OAMF_XFLIP|OAMF_YFLIP

  ld a, HIGH(wShadowOAM)
  call hOAMDMA

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800 | LCDCF_OBJON
	ld [rLCDC], a

.stuff:
  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt ; wait for VBlank
  nop

  ld hl, menu_frame_counter
  inc [hl]

  ld a, HIGH(wShadowOAM)
  call hOAMDMA

  ld a, [tweening]
  or a
  jp z, .no_tween

  ld a, [tween_step]
  cp 64
  jr c, .continue
  xor a
  ld [tweening], a
  inc a
  ld [bleh], a

  jp .no_tween
.continue:
  inc a
  inc a
  ld [tween_step], a
  dec a
  dec a
  add a

  ld hl, tweening_table
  add_a_to_hl

  ld c, [hl]
  inc hl
  ld b, [hl]

  ld a, [tween_endx1]
  ld hl, tween_startx1
  call tween
  ld [x1], a

  ld a, [tween_endy1]
  ld hl, tween_starty1
  call tween
  ld [y1], a

  ld a, [tween_endx2]
  ld hl, tween_startx2
  call tween
  ld [x2], a

  ld a, [tween_endy2]
  ld hl, tween_starty2
  call tween
  ld [y2], a

  ld a, [tween_endx3]
  ld hl, tween_startx3
  call tween
  ld [x3], a

  ld a, [tween_endy3]
  ld hl, tween_starty3
  call tween
  ld [y3], a

  ld a, [tween_endx4]
  ld hl, tween_startx4
  call tween
  ld [x4], a

  ld a, [tween_endy4]
  ld hl, tween_starty4
  call tween
  ld [y4], a

.no_tween:
  call poll_joystick

  ld a, [menu_frame_counter]
  and %00010000
  swap a
  ld c, a

  ld hl, coords

  ld a, [hl+]
  add c
  add 8
  spriteX 0
  ld a, [hl+]
  sub c
  add 8
  spriteX 1
  ld a, [hl+]
  add c
  add 8
  spriteX 2
  ld a, [hl+]
  sub c
  add 8
  spriteX 3
  ld a, [hl+]
  add c
  add 16
  spriteY 0
  ld a, [hl+]
  add c
  add 16
  spriteY 1
  ld a, [hl+]
  sub c
  add 16
  spriteY 2
  ld a, [hl+]
  sub c
  add 16
  spriteY 3

  ld a, [hPressedKeys]
  bit PADB_START, a
  jp z, .stuff

  ld a, [bleh]
  or a
  jr z, .nobleh

  ld de, credits_coords
  jr .yeah

.nobleh:
  ld de, select_level_coords

.yeah:
  call start_tween

  jp .stuff
  ret

start_tween:
  push de

  ld de, coords
  ld hl, tween_start_coords
  ld c, (play_coords.end - play_coords)
  rst MemcpySmall

  ld hl, tween_start_coords
  ld bc, tween_end_coords

  pop de

REPT 8
  ld a, [de]
  sub [hl]
  ld [bc], a
  inc hl
  inc de
  inc bc
ENDR

  xor a
  ld [tween_step], a
  inc a
  ld [tweening], a

  ret

;;; A = tween distance
;;; HL = tween start pos
tween:
  push af

  bit 7, a
  jr z, .positive_dist
  cpl
  inc a

.positive_dist:

  push hl
  ld d, a
  ld e, 0
  call mult_de_bc
  ld a, e
  pop hl

  pop de
  bit 7, d
  jr z, .no_negatize
  cpl
  inc a

.no_negatize:
  add [hl]

  ret

mult_de_bc:
  ld hl, 0

  sla e   ; optimised 1st iteration
  rl d
  jr nc, @+4
  ld h, b
  ld l, c

  ld a, 15
.loop:
  add hl, hl
  rl e
  rl d
  jr nc, @+6
  add hl, bc
  jr nc, @+3
  inc de

  dec a
  jr nz, .loop

  ret

MapRegion::
  push bc
.copy:
	ld a, [hli]
	ld [de], a
	inc de
	dec b
	jr nz, .copy
	pop af
	dec c
	ret z
	push af
	ld b, a
	ld a, SCRN_VX_B
	sub a, b
	add a, e
	ld e, a
	adc a, d
	sub a, e
	ld d, a
	jr .copy

tweening_table:
; dw 0
; dw 18
; dw 36
; dw 53
; dw 69
; dw 84
; dw 99
; dw 113
; dw 126
; dw 139
; dw 151
; dw 162
; dw 172
; dw 182
; dw 192
; dw 201
; dw 209
; dw 217
; dw 224
; dw 230
; dw 237
; dw 242
; dw 248
; dw 252
; dw 257
; dw 260
; dw 264
; dw 267
; dw 270
; dw 272
; dw 274
; dw 276
; dw 277
; dw 278
; dw 279
; dw 280
; dw 280
; dw 280
; dw 280
; dw 280
; dw 279
; dw 278
; dw 278
; dw 277
; dw 275
; dw 274
; dw 273
; dw 272
; dw 270
; dw 269
; dw 267
; dw 266
; dw 265
; dw 263
; dw 262
; dw 261
; dw 259
; dw 258
; dw 257
; dw 256
; dw 256
; dw 255
; dw 255
; dw 255
; dw 255


dw 0
dw 26
dw 50
dw 71
dw 90
dw 107
dw 122
dw 136
dw 148
dw 159
dw 169
dw 178
dw 186
dw 193
dw 199
dw 205
dw 210
dw 215
dw 219
dw 222
dw 226
dw 229
dw 231
dw 234
dw 236
dw 238
dw 240
dw 241
dw 243
dw 244
dw 245
dw 246
dw 247
dw 248
dw 248
dw 249
dw 249
dw 250
dw 250
dw 251
dw 251
dw 252
dw 252
dw 252
dw 252
dw 253
dw 253
dw 253
dw 253
dw 253
dw 253
dw 254
dw 254
dw 254
dw 254
dw 254
dw 254
dw 254
dw 254
dw 254
dw 254
dw 254
dw 254
dw 254
dw 255