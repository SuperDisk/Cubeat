include "defines.asm"

DEF BUTTON_PLAY = %00
DEF BUTTON_SELECT = %01
DEF BUTTON_MUSIC = %10
DEF BUTTON_CREDITS = %11

MACRO spriteAttr  ; which sprite, x, y, tile
  ld a, \2
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

include "res/menu/bgmenu.menu.asm"

SECTION "Main Menu Graphics", ROMX

main_menu_buttons_gfx:
incbin "res/menu/main_menu_buttons.2bppu"
.end:

main_menu_buttons_map:
incbin "res/menu/main_menu_buttons.tilemapu"
.end:

move_select_gfx:
incbin "res/menu/move_select.2bppu"
.end:

move_select_map:
incbin "res/menu/move_select.tilemapu"
.end:

back_gfx:
incbin "res/menu/back.2bppu"
.end:

back_map:
incbin "res/menu/back.tilemapu"
.end:

levels_gfx:
incbin "res/menu/levels.2bppu"
.end:

levels_map:
incbin "res/menu/levels.tilemapu"
.end:

text_select_level_gfx:
incbin "res/menu/text_select_level.2bppu"
.end:

text_select_level_map:
incbin "res/menu/text_select_level.tilemapu"
.end:

text_credits_gfx:
incbin "res/menu/text_credits.2bppu"
.end:

text_credits_map:
incbin "res/menu/text_credits.tilemapu"
.end:

text_pause_gfx:
incbin "res/menu/text_pause.2bppu"
.end:

text_pause_map:
incbin "res/menu/text_pause.tilemapu"
.end:

text_lost_progress_gfx:
incbin "res/menu/text_lost_progress.2bppu"
.end:

text_lost_progress_map:
incbin "res/menu/text_lost_progress.tilemapu"
.end:

cursor_sprite:
incbin "res/menu/cursor.2bpp"
.end:

song_buttons_gfx:
incbin "res/menu/song_buttons.2bppu"
.end:

song_buttons_map:
incbin "res/menu/song_buttons.tilemapu"
.end:

bg_scrolled_gfx:
incbin "res/menu/bg_scrolled.2bpp"
.end:

bg_scrolled_leftbar_gfx:
incbin "res/menu/bg_scrolled_leftbar.2bpp"
.end:

bg_scrolled_topbar_gfx:
incbin "res/menu/bg_scrolled_topbar.2bpp"
.end:

credits_scroll_gfx:
incbin "res/menu/credits_scroll.credits.2bpp"
.end:

credits_scroll_data:
include "res/menu/credits_scroll.credits.asm"
.end:

pause_buttons_gfx:
incbin "res/menu/pause_buttons.2bppu"
.end:

pause_buttons_map:
incbin "res/menu/pause_buttons.tilemapu"
.end:

SECTION "Menu vars", WRAM0
menu_ui_ptr: dw
menu_init_ptr: dw

menu_frame_counter: db
selected_button: db

selected_level: db
true_x: db
true_y: db

scroll_amount: dw

credits_scroll_amount: db
credits_row_offset: db

main_menu_inited:: db

SECTION "Tweening vars", WRAM0
tween_dist: db
tween_step: db
tweening: db

tween_dist2: db
tween_step2: db
tweening2: db

dest_coords_ptr: dw

scroll_x1: db
x1highbit: db

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

SECTION "Main Menu", ROM0

button_coords:

play_coords:
.x: db 15, 82, 15, 82
.y: db 39, 39, 74, 74
.end:

select_level_coords:
.x: db 87, 130, 87, 130
.y: db 47, 47, 74, 74

music_player_coords:
.x: db 39, 82, 39, 82
.y: db 79, 79, 106, 106

credits_coords:
.x: db 87, 122, 87, 122
.y: db 79, 79, 90, 90

CreditsMenu::
  ld hl, menu_ui_ptr
  ld [hl], LOW(credits_ui)
  inc hl
  ld [hl], HIGH(credits_ui)
  inc hl
  ld [hl], LOW(credits_init)
  inc hl
  ld [hl], HIGH(credits_init)
  jr Menu.no_sgb


PauseMenu::
  ld hl, menu_ui_ptr
  ld [hl], LOW(pause_ui)
  inc hl
  ld [hl], HIGH(pause_ui)
  inc hl
  ld [hl], LOW(pause_init)
  inc hl
  ld [hl], HIGH(pause_init)
  jr Menu.no_sgb

LevelsMenu::
  ld hl, menu_ui_ptr
  ld [hl], LOW(levels_ui)
  inc hl
  ld [hl], HIGH(levels_ui)
  inc hl
  ld [hl], LOW(levels_init)
  inc hl
  ld [hl], HIGH(levels_init)
  jr Menu.no_sgb

MusicPlayerMenu::
  ld hl, menu_ui_ptr
  ld [hl], LOW(music_player_ui)
  inc hl
  ld [hl], HIGH(music_player_ui)
  inc hl
  ld [hl], LOW(music_player_init)
  inc hl
  ld [hl], HIGH(music_player_init)
  jr Menu.no_sgb

MainMenu::
  ld hl, menu_ui_ptr
  ld [hl], LOW(main_menu_ui)
  inc hl
  ld [hl], HIGH(main_menu_ui)
  inc hl
  ld [hl], LOW(main_menu_init)
  inc hl
  ld [hl], HIGH(main_menu_init)
  jr c, Menu.no_sgb

Menu:
  ld hl, skin_menus
  call colorize
  call safe_turn_off_lcd

.no_sgb:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  call init_playfield_buffer

  ld de, static_ram_code
  ld hl, ram_code
  ld c, static_ram_code.end - static_ram_code
  rst MemcpySmall

  ld a, $31 ; ld sp, xxxx
  ld [update_bg_done], a

  ld a, $C3 ; jp xxxx
  ld [update_playfield_buffer], a

  ;; Copy initial tile data
  ld a, BANK(bgmenu_gfx_init)
  ld [rROMB0], a

  ld a, LOW(bgmenu_gfx_init)
  ld [ptr_next_update_bg], a
  ld a, HIGH(bgmenu_gfx_init)
  ld [ptr_next_update_bg+1], a
  call update_bg

  ld a, BANK(bgmenu_map_init)
  ld [next_map_bank], a
  ld a, LOW(bgmenu_map_init)
  ld [update_playfield_buffer+1], a
  ld a, HIGH(bgmenu_map_init)
  ld [update_playfield_buffer+2], a
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Setup tweening
  xor a
  ld [menu_frame_counter], a
  ld [tween_step], a
  ld [tweening], a
  ld [tween_step2], a
  ld [tweening2], a
  ;; Set up other vars
  ld [level_num], a
  ld [locked_level], a

  ld a, [main_menu_inited]
  or a
  jr nz, .no_init_selected_button
  xor a
  ld [selected_button], a
  inc a
  ld [main_menu_inited], a
.no_init_selected_button:

  ;; Load BG area
  ld a, BANK(move_select_gfx)
  ld [rROMB0], a
  ld hl, $9640
  ld de, move_select_gfx
  ld bc, (move_select_gfx.end - move_select_gfx)
  call Memcpy

  ld de, back_gfx
  ld bc, (back_gfx.end - back_gfx)
  call Memcpy

  ;; Load sprite area
  ld de, cursor_sprite
  ld hl, $8000
  ld c, (cursor_sprite.end - cursor_sprite)
  rst MemcpySmall

  ;; Setup sprites
  call clear_oam
  spriteAttr 0, 0
  spriteAttr 1, OAMF_XFLIP
  spriteAttr 2, OAMF_YFLIP
  spriteAttr 3, OAMF_XFLIP|OAMF_YFLIP

  ;; Initialize tile map
  ld a, [next_map_bank]
  ld [rROMB0], a
  call update_playfield_buffer

  ld a, BANK(main_menu_buttons_gfx)
  ld [rROMB0], a
  ld hl, menu_init_ptr
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  rst CallHL

  ld a, HIGH(wShadowOAM)
  call hOAMDMA

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800 | LCDCF_OBJON
  ld [rLCDC], a

  call UnfreezeScreen

  call FadeInit
  ld hl, KnownRet
  call FadeIn

menu_loop:
  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt

  call FadeStep
  call tick_sfx

  ld a, HIGH(wShadowOAM)
  call hOAMDMA

  ld a, [next_map_bank]
  ld [rROMB0], a
  call update_playfield_buffer

  ld hl, menu_ui_ptr
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  rst CallHL

  jr menu_loop

credits_init:
  ld de, text_credits_gfx
  ld hl, $8800
  ld c, (text_credits_gfx.end - text_credits_gfx)
  rst MemcpySmall

  ld de, credits_scroll_gfx
  ld hl, $8000
  ld bc, (credits_scroll_gfx.end - credits_scroll_gfx)
  call Memcpy

  lb bc, 4, 2
  ld de, $9A01
  ld hl, back_map
  call MapRegion

  lb bc, 5, 1
  ld de, $9843
  ld hl, text_credits_map
  call MapRegion

  spriteAttr 0, 0
  spriteAttr 1, 0
  spriteAttr 2, 0
  spriteAttr 3, 0

  ld a, 120+16
  ld [credits_scroll_amount], a
  xor a
  ld [credits_row_offset], a

  ret

credits_ui:
  call poll_joystick

  ld a, [hPressedKeys]
  bit PADB_B, a
  jr z, .wait_for_split

  ld hl, sfx_ui_back
  call play_sfx

  ld hl, goto_mainmenu
  call FadeOut

.wait_for_split:
  ld a, [rLY]
  cp 33
  jr nz, .wait_for_split

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800 | LCDCF_OBJON | LCDCF_OBJ16
  ld [rLCDC], a

.wait_for_split2:
  ld a, [rLY]
  cp 120
  jr nz, .wait_for_split2

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800 | LCDCF_OBJOFF | LCDCF_OBJ16
  ld [rLCDC], a

  ld hl, wShadowOAM
  ld c, NB_SPRITES * 4
  xor a
  rst MemsetSmall

  ;; Update scrolling sprites
  ld a, BANK(credits_scroll_data)
  ld [rROMB0], a

  ld hl, credits_scroll_data
  ld a, [credits_row_offset]
  or a
  jr z, .scan_done
  ld c, a
.scan_zeros:
  ld a, [hl+]
  or a
  jr nz, .scan_zeros
  inc hl
  dec c
  jr nz, .scan_zeros

.scan_done:
  push hl
  ld d, h
  ld e, l

  ld hl, wShadowOAM

  ld a, [credits_scroll_amount]
  ld c, a
  ld b, 116-92+8

REPT 6
:
  ld a, c
  ld [hl+], a
  ld a, b
  ld [hl+], a
  add 8
  ld b, a
  ld a, [de]
  or a
  jr z, :+
  dec a
  add a
  ld [hl+], a
  inc de
  inc hl
  jr :-
:
  dec hl
  dec hl
  inc de
  ld a, [de]
  add c
  add 16
  ld c, a
  ld b, 116-92+8
  inc de
ENDR

  xor a
  ld [hl-], a
  ld [hl-], a

  ld a, [hHeldKeys]
  and ~PADF_B
  jr nz, .held_a

  ld a, [menu_frame_counter]
  cp 9
  jr nz, .not_equal

.held_a:
  ld hl, credits_scroll_amount
  dec [hl]

  xor a
.not_equal:
  inc a
  ld [menu_frame_counter], a

  ld a, [credits_scroll_amount]
  cp $21
  jr z, .change_scroll
  pop af
  ret

.change_scroll:
  pop hl

.scan_zeros2:
  ld a, [hl+]
  or a
  jr nz, .scan_zeros2

  ld a, [credits_scroll_amount]
  add [hl]
  add 16
  ld [credits_scroll_amount], a

  ld a, [credits_row_offset]
  inc a
  cp $9
  jr nz, .no_reset_credits

  ld a, 120+16
  ld [credits_scroll_amount], a
  xor a
  ld [credits_row_offset], a
  ret

.no_reset_credits:
  ld [credits_row_offset], a

  ret

main_menu_init:
  ld a, [selected_button]
  ld de, button_coords
  add a
  add a
  add a
  add_a_to_de

  ld hl, coords
  ld c, (play_coords.end - play_coords)
  rst MemcpySmall

  ld de, main_menu_buttons_gfx
  ld hl, $9000
  ld bc, (main_menu_buttons_gfx.end - main_menu_buttons_gfx)
  call Memcpy
  jp main_menu_ui

pause_init:
  ld de, text_pause_gfx
  ld hl, $9000
  ld bc, (text_pause_gfx.end - text_pause_gfx)
  call Memcpy

  ld de, pause_buttons_gfx
  ld bc, (pause_buttons_gfx.end - pause_buttons_gfx)
  call Memcpy

  ld de, text_lost_progress_gfx
  ld bc, (text_lost_progress_gfx.end - text_lost_progress_gfx)
  call Memcpy

  xor a
  ld [selected_level], a

  ld a, 29
  ld [true_x], a
  ld [x1], a
  ld [x3], a

  ld a, 123
  ld [x2], a
  ld [x4], a

  ld a, 45
  ld [true_y], a
  ld [y1], a
  ld [y2], a

  ld a, 60
  ld [y3], a
  ld [y4], a
  call update_cursor_pos

  jp pause_ui

pause_ui:
  ld hl, menu_frame_counter
  inc [hl]

  ld a, [tweening]
  or a
  jp z, .no_tween

  ld a, [tween_step]
  cp 40
  jr c, .continue
  xor a
  ld [tweening], a

  ld a, [true_x]
  ld [x1], a
  ld a, [true_y]
  ld [y1], a

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

.no_tween:
  ld a, [x1]
  ld [x3], a
  add 94
  ld [x2], a
  ld [x4], a

  ld a, [y1]
  ld [y2], a
  add 15
  ld [y3], a
  ld [y4], a

  call poll_joystick
  call update_cursor_pos

  ld a, [hPressedKeys]
  bit PADB_A, a
  jr z, .no_a

  ld hl, sfx_alt_radar_destroy_echo
  call play_sfx

  ld hl, goto_mainmenu
  ld a, [selected_level]
  dec a
  call z, FadeOut

  ld hl, goto_gameplay_restore
  call FadeOut

.no_a:
  and PADF_UP|PADF_DOWN
  jr z, .no_begin_tween

  ld d, a
  bit PADB_DOWN, d
  jr z, .no_down

  ;; pressed down

  ld a, [selected_level]
  or a
  jr nz, .no_begin_tween
  inc a
  ld [selected_level], a

  ld a, [x1]
  ld [tween_startx1], a
  ld [true_x], a
  xor a
  ld [tween_endx1], a

  ld a, [y1]
  ld [tween_starty1], a
  ld b, a

  ld a, [true_y]
  add 15
  ld [true_y], a

  sub b
  ld [tween_endy1], a

  jr .did_tween

.no_down:
  bit PADB_UP, d
  jr z, .no_up

  ;; pressed up

  ld a, [selected_level]
  or a
  jr z, .no_begin_tween
  dec a
  ld [selected_level], a

  ld a, [x1]
  ld [tween_startx1], a
  ld [true_x], a
  xor a
  ld [tween_endx1], a

  ld a, [y1]
  ld [tween_starty1], a
  ld b, a

  ld a, [true_y]
  sub 15
  ld [true_y], a

  sub b
  ld [tween_endy1], a

.no_up:

.did_tween:
  ld hl, sfx_ui_move
  call play_sfx

  xor a
  ld [tween_step], a
  inc a
  ld [tweening], a

.no_begin_tween:
  ld a, BANK(text_pause_gfx)
  ld [rROMB0], a

  lb bc, 4, 1
  ld de, $9864
  ld hl, text_pause_map
  call MapRegion

  lb bc, 13, 3
  ld de, $99E1
  ld hl, move_select_map
  call MapRegion

  lb bc, 12, 4
  ld de, $98C4
  ld hl, pause_buttons_map
  call MapRegion

  ld a, [selected_level]
  or a
  jr nz, .draw_warning

  lb bc, 8, 2
  ld de, $9965
  ld hl, text_lost_progress_map + (8*2)
  ;; tail call
  jp MapRegion

.draw_warning:
  lb bc, 8, 2
  ld de, $9965
  ld hl, text_lost_progress_map
  ;; tail call
  jp MapRegion

levels_init:
  xor a
  ld [selected_level], a

  ld a, 23
  ld [x1], a
  ld a, 31
  ld [y1], a

  ld de, levels_gfx
  ld hl, $9000
  ld bc, (levels_gfx.end - levels_gfx)
  call Memcpy

  ld hl, $9790
  ld de, text_select_level_gfx
  ld bc, (text_select_level_gfx.end - text_select_level_gfx)
  call Memcpy
  jp levels_ui

paint_music_buttons_part1:
  ld a, [scroll_x1]
  ld l, a
  ld a, [x1highbit]
  rrca
  srl l
  or l
  srl a
  srl a

  ld hl, $9880
  ld de, song_buttons_map
  add_a_to_de

  ld b, 5
.loop0:
REPT 21
  ld a, [de]
  ld [hl+], a
  inc de
ENDR

  ld a, $20-21
  add_a_to_hl

  ld a, 65-21
  add_a_to_de

  dec b
  jr nz, .loop0

  ret

paint_music_buttons_part2:
  ld b, 6
.loop0:
REPT 21
  wait_vram
  ld a, [de]
  ld [hl+], a
  inc de
ENDR

  ld a, $20-21
  add_a_to_hl

  ld a, 65-21
  add_a_to_de

  dec b
  jp nz, .loop0

  ret

music_player_init:
  xor a
  ld [scroll_amount], a
  ld [scroll_amount+1], a
  ld [scroll_x1], a
  ld [x1highbit], a

  ld a, 24+8-1
  ld [x1], a
  ld [x3], a
  add 89-6
  ld [x2], a
  ld [x4], a

  ld a, 32-1
  ld [y1], a
  ld [true_y], a
  ld [y2], a
  add 17-6
  ld [y3], a
  ld [y4], a

  call update_cursor_pos

  ld de, song_buttons_gfx
  ld hl, $8900
  ld bc, (song_buttons_gfx.end - song_buttons_gfx)
  call Memcpy

  ld hl, $9790
  ld de, text_select_level_gfx
  ld bc, (text_select_level_gfx.end - text_select_level_gfx)
  call Memcpy

  lb bc, 7, 1
  ld de, $9843
  ld hl, text_select_level_map
  call MapRegion

  lb bc, 13, 3
  ld de, $99E1
  ld hl, move_select_map
  call MapRegion

  lb bc, 4, 2
  ld de, $9A0E
  ld hl, back_map
  call MapRegion

  call paint_music_buttons_part1
  call paint_music_buttons_part2

.upload_scrolled_gfx:
  ld a, [scroll_x1]
  and %111
  swap a ; *= 16
  push af
  push af

  ld de, bg_scrolled_gfx
  add_a_to_de
  ld hl, $9000
  ld bc, 16
  call LCDMemcpy

  pop af
  ld de, bg_scrolled_leftbar_gfx
  add_a_to_de
  ld hl, $90E0
  ld bc, 16
  call LCDMemcpy

  pop af
  ld de, bg_scrolled_topbar_gfx
  add_a_to_de
  ld hl, $9310
  ld bc, 16
  jp LCDMemcpy ; tail call

music_player_ui:
  ld a, BANK(song_buttons_gfx)
  ld [rROMB0], a

  call paint_music_buttons_part1
  push de
  push hl

  call music_player_init.upload_scrolled_gfx

  ld hl, menu_frame_counter
  inc [hl]

.wait_for_split:
  ld a, [rLY]
  cp 31
  jr nz, .wait_for_split

  wait_vram
  ld a, [scroll_x1]
  and %111
  ld [rSCX], a

  pop hl
  pop de
  call paint_music_buttons_part2

.wait_for_split2
  ld a, [rLY]
  cp 111+8
  jr c, .wait_for_split2

  wait_vram
  xor a
  ld [rSCX], a

  ld a, [tweening]
  or a
  jp z, .no_tween

  ld a, [tween_step]
  cp 63
  jr c, .continue
  xor a
  ld [tweening], a

  ld a, [scroll_amount]
  ld [scroll_x1], a

  jp .no_tween

.continue:
  inc a
  ; inc a
  ld [tween_step], a
  ; dec a
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
  ld hl, scroll_x1
  ld c, [hl]
  ld [hl], a

  ld a, [tween_endx1]
  bit 7, a
  jr z, .scrolling_right

  ld a, [hl]
  cp c
  jr c, .no_tween
  jr z, .no_tween
  ld hl, x1highbit
  ld [hl], 0

  jr .no_tween

.scrolling_right:
  ld a, [hl]
  cp c
  jr nc, .no_tween
  ld hl, x1highbit
  ld [hl], 1

.no_tween:
  ld a, [tweening2]
  or a
  jp z, .no_tween2

  ld a, [tween_step2]
  cp 40
  jr c, .continue2
  xor a
  ld [tweening2], a

  ld a, [true_y]
  ld [y1], a

  jp .no_tween2
.continue2:
  inc a
  inc a
  ld [tween_step2], a
  dec a
  dec a
  add a

  ld hl, tweening_table
  add_a_to_hl

  ld c, [hl]
  inc hl
  ld b, [hl]

  ld a, [tween_endy1]
  ld hl, tween_starty1
  call tween
  ld [y1], a

.no_tween2:
  ld a, [x1]
  ld [x3], a
  add 89-6
  ld [x2], a
  ld [x4], a

  ld a, [y1]
  ld [y2], a
  add 17-6
  ld [y3], a
  ld [y4], a

  call poll_joystick
  call update_cursor_pos

  ld a, [hPressedKeys]
  or a
  ret z

  bit PADB_RIGHT, a
  jr z, .no_right

  ld hl, scroll_amount
  ld a, [hl+]
  cp $60
  jr z, .no_right

  ld [tween_startx1], a
  ld [scroll_x1], a

  ld h, [hl]
  ld l, a

  ld a, h
  ld [x1highbit], a

  ld de, 88
  add hl, de

  ld a, l
  ld [scroll_amount], a
  ld a, h
  ld [scroll_amount+1], a

  ld a, 88
  ld [tween_endx1], a

  xor a
  ld [tween_step], a
  inc a
  ld [tweening], a

  ld hl, sfx_ui_move
  call play_sfx

.no_right:
  ld a, [hPressedKeys]
  bit PADB_LEFT, a
  jr z, .no_left

  ld hl, scroll_amount
  ld a, [hl+]
  or a
  jr z, .no_left

  ld [tween_startx1], a
  ld [scroll_x1], a

  ld h, [hl]
  ld l, a

  ld a, h
  ld [x1highbit], a

  ld de, -88
  add hl, de

  ld a, l
  ld [scroll_amount], a
  ld a, h
  ld [scroll_amount+1], a

  ld a, -88
  ld [tween_endx1], a

  xor a
  ld [tween_step], a
  inc a
  ld [tweening], a

  ld hl, sfx_ui_move
  call play_sfx

.no_left:
  ld a, [hPressedKeys]
  bit PADB_DOWN, a
  jr z, .no_down

  ld a, [true_y]
  cp $5F
  jr z, .no_up
  ld [tween_starty1], a
  add 16
  ld [true_y], a
  ld a, 16
  ld [tween_endy1], a

  xor a
  ld [tween_step2], a
  inc a
  ld [tweening2], a

  ld hl, sfx_ui_move
  call play_sfx

.no_down:
  ld a, [hPressedKeys]
  bit PADB_UP, a
  jr z, .no_up

  ld a, [true_y]
  cp $1F
  jr z, .no_up
  ld [tween_starty1], a
  sub 16
  ld [true_y], a
  ld a, -16
  ld [tween_endy1], a

  xor a
  ld [tween_step2], a
  inc a
  ld [tweening2], a

  ld hl, sfx_ui_move
  call play_sfx

.no_up:
  ld a, [hPressedKeys]
  bit PADB_B, a
  ret z

  ld hl, sfx_ui_back
  call play_sfx

  ld hl, goto_mainmenu
  call FadeOut

  ret

levels_ui:
  ld a, BANK(levels_gfx)
  ld [rROMB0], a
  lb bc, 15, 9
  ld de, $9883
  ld hl, levels_map
  call MapRegion

  lb bc, 13, 3
  ld de, $99E1
  ld hl, move_select_map
  call MapRegion

  lb bc, 4, 2
  ld de, $9A0E
  ld hl, back_map
  call MapRegion

  lb bc, 7, 1
  ld de, $9843
  ld hl, text_select_level_map
  call MapRegion

  ld hl, menu_frame_counter
  inc [hl]

  ld a, [tweening]
  or a
  jp z, .no_tween

  ld a, [tween_step]
  cp 40
  jr c, .continue
  xor a
  ld [tweening], a

  ld a, [true_x]
  ld [x1], a
  ld a, [true_y]
  ld [y1], a

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

.no_tween:
  ld a, [x1]
  ld [x3], a
  add 11
  ld [x2], a
  ld [x4], a

  ld a, [y1]
  ld [y2], a
  add 11
  ld [y3], a
  ld [y4], a

  call poll_joystick
  call update_cursor_pos

  ld a, [hPressedKeys]
  bit PADB_A, a
  jr z, .no_a

  ld a, [selected_level]
  add a
  add a
  ld [level_num], a
  inc a ; set a to nonzero (level num is never 255)
  ld [locked_level], a
  ld hl, goto_gameplay
  call FadeOut

.no_a:
  bit PADB_B, a
  jr z, .no_b

  ld hl, sfx_ui_back
  call play_sfx

  ld hl, goto_mainmenu
  call FadeOut

.no_b:
  ld a, [hPressedKeys]
  and PADF_LEFT|PADF_RIGHT|PADF_UP|PADF_DOWN
  ret z

  ld d, a
  ld a, [selected_level]

  bit PADB_LEFT, d
  jr z, .no_left
  sub 1
  jr nc, .no_left
  inc a
.no_left:
  bit PADB_RIGHT, d
  jr z, .no_right
  inc a
  cp 25
  jr nz, .no_right
  dec a
.no_right:
  bit PADB_UP, d
  jr z, .no_up
  sub 7
  jr nc, .no_up
  add 7
.no_up:
  bit PADB_DOWN, d
  jr z, .no_down
  add 7
  cp 25
  jr c, .no_down
  sub 7
.no_down:
  ld hl, selected_level
  cp [hl]
  ld [hl], a
  push af

  ld b, 0
.div_loop:
  inc b
  sub 7
  jr nc, .div_loop
  add 7

  swap a
  add 24-1
  ld d, a
  ld [true_x], a

  ld a, b
  dec a
  swap a
  add 32-1
  ld b, a
  ld [true_y], a

  ld a, [x1]
  ld [tween_startx1], a
  ld e, a
  ld a, d
  sub e
  ld [tween_endx1], a

  ld a, [y1]
  ld [tween_starty1], a
  ld e, a
  ld a, b
  sub e
  ld [tween_endy1], a

  xor a
  ld [tween_step], a
  inc a
  ld [tweening], a

  pop af
  ld hl, sfx_ui_move
  call nz, play_sfx

  ret

main_menu_ui:
  ld a, BANK(main_menu_buttons_gfx)
  ld [rROMB0], a
  lb bc, 16, 10
  ld de, $98A2
  ld hl, main_menu_buttons_map
  call MapRegion

  lb bc, 13, 3
  ld de, $99E1
  ld hl, move_select_map
  call MapRegion

  ld hl, menu_frame_counter
  inc [hl]

  ld a, [tweening]
  or a
  jp z, .no_tween

  ld a, [tween_step]
  cp 40
  jr c, .continue
  xor a
  ld [tweening], a

  ld hl, dest_coords_ptr
  ld a, [hl+]
  ld d, [hl]
  ld e, a
  ld hl, coords
  ld c, 8
  rst MemcpySmall

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
  call update_cursor_pos

  ld a, [hPressedKeys]
  ld d, a
  bit PADB_B, d
  jr z, .no_b

  ld hl, sfx_ui_back
  call play_sfx

  ld hl, goto_titlescreen
  call FadeOut

.no_b:
  bit PADB_A, d
  jr z, .no_a

  ld hl, sfx_alt_radar_destroy_echo
  call play_sfx

  ld a, [selected_button]
  add a
  ld hl, .jump
  add_a_to_hl
  ld a, [hl+]
  ld h, [hl]
  ld l, a
  call FadeOut

  jr .no_a
.jump:
  dw goto_gameplay
  dw goto_levelsmenu
  dw goto_musicplayermenu
  dw goto_creditsmenu
.no_a:
  ld a, [hPressedKeys]
  and PADF_LEFT|PADF_RIGHT|PADF_UP|PADF_DOWN
  ret z

  ld d, a
  ld a, [selected_button]

  bit PADB_LEFT, d
  jr z, .no_left
  res 0, a
.no_left:
  bit PADB_RIGHT, d
  jr z, .no_right
  set 0, a
.no_right:
  bit PADB_UP, d
  jr z, .no_up
  res 1, a
.no_up:
  bit PADB_DOWN, d
  jp z, .no_down
  set 1, a
.no_down:
  ld hl, selected_button
  cp [hl]
  ld [hl], a
  push af

  ld de, button_coords
  add a
  add a
  add a
  add_a_to_de

  pop af
  ld hl, sfx_ui_move
  call nz, play_sfx

  ;; fallthrough

start_tween:
  push de
  ld hl, dest_coords_ptr
  ld [hl], e
  inc hl
  ld [hl], d

  ld de, coords
  ld hl, tween_start_coords
  ld c, (play_coords.end - play_coords)
  rst MemcpySmall

  pop de
  ld hl, tween_start_coords
  ld bc, tween_end_coords

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


update_cursor_pos:
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

; @param b: Width
; @param c: Height
; @param de: VRAM destination
; @param hl: Source map
MapRegion::
  push bc
.copy:
.waitVram:
  ldh a, [rSTAT]
  and a, STATF_BUSY
  jr nz, .waitVram
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
