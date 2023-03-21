INCLUDE "defines.asm"

SECTION "Header", ROM0[$100]

  ; This is your ROM's entry point
  ; You have 4 bytes of code to do... something
  sub $11 ; This helps check if we're on CGB more efficiently
  jr EntryPoint

  ; Make sure to allocate some space for the header, so no important
  ; code gets put there and later overwritten by RGBFIX.
  ; RGBFIX is designed to operate over a zero-filled header, so make
  ; sure to put zeros regardless of the padding value. (This feature
  ; was introduced in RGBDS 0.4.0, but the -MG etc flags were also
  ; introduced in that version.)
  ds $150 - @, 0

EntryPoint:
  ldh [hConsoleType], a

  xor a
  ldh [hIsSGB], a

  ld a, $14
  cp c
  jr nz, .not_sgb

  inc a
  ldh [hIsSGB], a

  call DisableManualPaletting

.not_sgb:
  ; ld hl, skin0.border_bank
  ; call colorize

Reset::
  ; Enable sound globally
  ld a, $80
  ld [rAUDENA], a
  ; Enable all channels in stereo
  ld a, $FF
  ld [rAUDTERM], a
  ; Set volume
  ld a, $FF
  ld [rAUDVOL], a

  ; Wait for VBlank and turn LCD off
.waitVBlank
  ldh a, [rLY]
  cp SCRN_Y
  jr c, .waitVBlank
  xor a
  ldh [rLCDC], a
  ; Goal now: set up the minimum required to turn the LCD on again
  ; A big chunk of it is to make sure the VBlank handler doesn't crash

  ld sp, wStackBottom

  ld a, BANK(OAMDMA)
  ; No need to write bank number to HRAM, interrupts aren't active
  ld [rROMB0], a
  ld hl, OAMDMA
  lb bc, OAMDMA.end - OAMDMA, LOW(hOAMDMA)
.copyOAMDMA
  ld a, [hli]
  ldh [c], a
  inc c
  dec b
  jr nz, .copyOAMDMA

  ; CGB palettes maybe, DMG ones always
  ; xor a
  ; ld [rBGP], a
  ; ld [rOBP0], a
  ; ld [rOBP1], a
  ld a, %00_10_01_11
  ld [wBGP], a
  ld a, %11_10_00_01
  ld [wOBP0], a
  ld a, %00_01_11_10
  ld [wOBP1], a


  ; You will also need to reset your handlers' variables below
  ; I recommend reading through, understanding, and customizing this file
  ; in its entirety anyways. This whole file is the "global" game init,
  ; so it's strongly tied to your own game.
  ; I don't recommend clearing large amounts of RAM, nor to init things
  ; here that can be initialized later.

  ; Reset variables necessary for the VBlank handler to function correctly
  ; But only those for now
  xor a
  ldh [hVBlankFlag], a
  ldh [hOAMHigh], a
  ldh [hCanSoftReset], a
  dec a ; ld a, $FF
  ldh [hHeldKeys], a

  ; Clear OAM, so it doesn't display garbage
  ; This will get committed to hardware OAM after the end of the first
  ; frame, but the hardware doesn't display it, so that's fine.
  ld hl, wShadowOAM
  ld c, NB_SPRITES * 4
  xor a
  rst MemsetSmall
  ld a, h ; ld a, HIGH(wShadowOAM)
  call hOAMDMA

  ld hl, wShadowOAM2
  ld c, NB_SPRITES * 4
  xor a
  rst MemsetSmall

  ; jp Intro
  ; jp MainMenu
  jp TitleScreen

SECTION "OAM DMA routine", ROMX

; OAM DMA prevents access to most memory, but never HRAM.
; This routine starts an OAM DMA transfer, then waits for it to complete.
; It gets copied to HRAM and is called there from the VBlank handler
OAMDMA:
  ldh [rDMA], a
  ld a, NB_SPRITES
.wait
  dec a
  jr nz, .wait
  ret
.end

SECTION "Global vars", HRAM

; 0 if CGB (including DMG mode and GBA), non-zero for other models
hConsoleType:: db
hIsSGB:: db

;; Temporary stack used inside of graphic update data
;; Has enough room to call hOAMDMA but that's it
hTempStackTop:
ds 2
hTempStack:: ds 2

; Copy of the currently-loaded ROM bank, so the handlers can restore it
; Make sure to always write to it before writing to ROMB0
; (Mind that if using ROMB1, you will run into problems)
hCurROMBank:: db


SECTION "OAM DMA", HRAM

hOAMDMA::
  ds OAMDMA.end - OAMDMA


SECTION UNION "Shadow OAM", WRAM0,ALIGN[8]

;; HUD, preview blocks (top part of screen)
wShadowOAM::
  ds NB_SPRITES * 4

SECTION UNION "Shadow OAM 2", WRAM0,ALIGN[8]

;; Radar, drop highlights, animations (bottom part of screen)
wShadowOAM2::
  ds NB_SPRITES * 4

;; this is a dummy "sprite" that is used when we're out
;; of real sprites. It doesn't appear.
ds 4

SECTION "Stack", WRAM0

wStack:
  ds STACK_SIZE
wStackBottom:
