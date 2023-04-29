include "defines.asm"

include "res/anarkade_logo.nocolon.asm"

SECTION "Splash Screen", ROM0

SplashScreen::
  ;; Blank the background layer
  ld a, $88
  ld c, 0
  ld hl, $9800
  rst MemsetSmall
  rst MemsetSmall
  rst MemsetSmall

  ;; Clear out OAM
  ld hl, $FE00
  ld c, $9F
  xor a
  rst MemsetSmall

  ld de, playfield_buffer_rom
  ld hl, playfield_buffer
  ld bc, playfield_buffer_rom.end - playfield_buffer_rom
  call Memcpy

  ld de, static_ram_code
  ld hl, ram_code
  ld c, static_ram_code.end - static_ram_code
  rst MemcpySmall

  ld a, $31 ; ld sp, xxxx
  ld [update_bg_done], a

  ld a, $C3 ; jp xxxx
  ld [update_playfield_buffer], a

  ld a, $98
  ld [current_bg], a

  ;; Copy initial tile data
  ld a, BANK(anarkade_logo_gfx_init)
  ld [rROMB0], a

  ld a, LOW(anarkade_logo_gfx_init)
  ld [ptr_next_update_bg], a
  ld a, HIGH(anarkade_logo_gfx_init)
  ld [ptr_next_update_bg+1], a
  call update_bg

  ld a, BANK(anarkade_logo_map0)
  ld [next_map_bank], a
  ld a, LOW(anarkade_logo_map0)
  ld [update_playfield_buffer+1], a
  ld a, HIGH(anarkade_logo_map0)
  ld [update_playfield_buffer+2], a

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8800 | LCDCF_OBJON
  ld [rLCDC], a

  ld a, [wBGP]
  ld [rBGP], a

splash_loop:
  ld a, [next_map_bank]
  ld [rROMB0], a

  call update_playfield_buffer

  ld a, [current_bg]
  xor %00000100
  ld [current_bg], a
  ld h, a
  ld l, 0

  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt ; wait for VBlank
  nop

  assert IEF_VBLANK + 1 == IEF_STAT
  ld a, IEF_STAT
  ldh [rIE], a
  ld a, STATF_MODE00
  ldh [rSTAT], a ; Careful, this may make the STAT int pending

  call playfield_buffer

  ld a, [next_gfx_bank]
  ld [rROMB0], a

  ld a, IEF_VBLANK
  ldh [rIE], a
  xor a
  ld [rIF], a
  halt ; wait for VBlank
  nop

  ; swap map
  ldh a, [rLCDC]
  xor %00001000
  ldh [rLCDC], a

  ld a, IEF_STAT
  ldh [rIE], a
  ld a, STATF_MODE00
  ldh [rSTAT], a ; Careful, this may make the STAT int pending

  call update_bg

  jr splash_loop