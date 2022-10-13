;; This file stolen from https://github.com/ISSOtm/motherboard-gb

include "defines.asm"

MACRO defborder
db BANK(\1)
dw \1, \2
ENDM

SECTION "Border table", ROM0
border_table::
  defborder frame_01_tiles, frame_01_attrs
  defborder frame_02_tiles, frame_02_attrs
  defborder frame_03_tiles, frame_03_attrs
  defborder frame_04_tiles, frame_04_attrs
  defborder frame_05_tiles, frame_05_attrs
  defborder frame_06_tiles, frame_06_attrs
  defborder frame_07_tiles, frame_07_attrs
  defborder frame_08_tiles, frame_08_attrs
  defborder frame_09_tiles, frame_09_attrs
  defborder frame_10_tiles, frame_10_attrs
  defborder frame_11_tiles, frame_11_attrs
  defborder frame_12_tiles, frame_12_attrs
  defborder frame_13_tiles, frame_13_attrs
  defborder frame_22_tiles, frame_22_attrs

SECTION "frame_01", ROMX
frame_01_tiles:
INCBIN "res/borders/SuperGameBoyFrame_01_Clean.borderchr.pb16"
frame_01_attrs:
INCBIN "res/borders/SuperGameBoyFrame_01_Clean.borderattr.pb16"
SECTION "frame_02", ROMX
frame_02_tiles:
INCBIN "res/borders/SuperGameBoyFrame_02_Clean.borderchr.pb16"
frame_02_attrs:
INCBIN "res/borders/SuperGameBoyFrame_02_Clean.borderattr.pb16"
SECTION "frame_03", ROMX
frame_03_tiles:
; INCBIN "res/borders/SuperGameBoyFrame_03_Clean.borderchr.pb16"
frame_03_attrs:
; INCBIN "res/borders/SuperGameBoyFrame_03_Clean.borderattr.pb16"
SECTION "frame_04", ROMX
frame_04_tiles:
; INCBIN "res/borders/SuperGameBoyFrame_04_Clean.borderchr.pb16"
frame_04_attrs:
; INCBIN "res/borders/SuperGameBoyFrame_04_Clean.borderattr.pb16"
SECTION "frame_05", ROMX
frame_05_tiles:
INCBIN "res/borders/SuperGameBoyFrame_05_Clean.borderchr.pb16"
frame_05_attrs:
INCBIN "res/borders/SuperGameBoyFrame_05_Clean.borderattr.pb16"
SECTION "frame_06", ROMX
frame_06_tiles:
INCBIN "res/borders/SuperGameBoyFrame_06_Clean.borderchr.pb16"
frame_06_attrs:
INCBIN "res/borders/SuperGameBoyFrame_06_Clean.borderattr.pb16"
SECTION "frame_07", ROMX
frame_07_tiles:
INCBIN "res/borders/SuperGameBoyFrame_07_Clean.borderchr.pb16"
frame_07_attrs:
INCBIN "res/borders/SuperGameBoyFrame_07_Clean.borderattr.pb16"
SECTION "frame_08", ROMX
frame_08_tiles:
INCBIN "res/borders/SuperGameBoyFrame_08_Clean.borderchr.pb16"
frame_08_attrs:
INCBIN "res/borders/SuperGameBoyFrame_08_Clean.borderattr.pb16"
SECTION "frame_09", ROMX
frame_09_tiles:
INCBIN "res/borders/SuperGameBoyFrame_09_Clean.borderchr.pb16"
frame_09_attrs:
INCBIN "res/borders/SuperGameBoyFrame_09_Clean.borderattr.pb16"
SECTION "frame_10", ROMX
frame_10_tiles:
INCBIN "res/borders/SuperGameBoyFrame_10_Clean.borderchr.pb16"
frame_10_attrs:
INCBIN "res/borders/SuperGameBoyFrame_10_Clean.borderattr.pb16"
SECTION "frame_11", ROMX
frame_11_tiles:
; INCBIN "res/borders/SuperGameBoyFrame_11_Clean.borderchr.pb16"
frame_11_attrs:
; INCBIN "res/borders/SuperGameBoyFrame_11_Clean.borderattr.pb16"
SECTION "frame_12", ROMX
frame_12_tiles:
; INCBIN "res/borders/SuperGameBoyFrame_12_Clean.borderchr.pb16"
frame_12_attrs:
; INCBIN "res/borders/SuperGameBoyFrame_12_Clean.borderattr.pb16"
SECTION "frame_13", ROMX
frame_13_tiles:
INCBIN "res/borders/SuperGameBoyFrame_13_Clean.borderchr.pb16"
frame_13_attrs:
INCBIN "res/borders/SuperGameBoyFrame_13_Clean.borderattr.pb16"
SECTION "frame_22", ROMX
frame_22_tiles:
; INCBIN "res/borders/SuperGameBoyFrame_22_Clean.borderchr.pb16"
frame_22_attrs:
; INCBIN "res/borders/SuperGameBoyFrame_22_Clean.borderattr.pb16"


SECTION "SGB setup", ROM0

TwoPlayersPacket:
    sgb_packet MLT_REQ, 1, 1
OnePlayerPacket:
    sgb_packet MLT_REQ, 1, 0

DisablePalettesPacket:
    sgb_packet ICON_EN, 1, %001 ; Disable palettes, keep other two

TransferBorderTilesPacket:
    sgb_packet CHR_TRN, 1, %00 ; BG tiles, $00-7F
TransferBorderTilesPacket2:
    sgb_packet CHR_TRN, 1, %01 ; BG tiles, $80-FF

BORDER_ATTRIBUTE_SIZE = $880
TransferBorderAttributesPacket:
    sgb_packet PCT_TRN, 1

; hack
vSGBTransferArea EQU $8000

MACRO sgb_pal_packet
  db (PAL01 << 3) | 1
  dw (\1 & $F8) << 7 | (\1 & $F800) >> 6 | (\1 & $F80000) >> 19
  dw (\2 & $F8) << 7 | (\2 & $F800) >> 6 | (\2 & $F80000) >> 19
  dw (\3 & $F8) << 7 | (\3 & $F800) >> 6 | (\3 & $F80000) >> 19
  dw (\4 & $F8) << 7 | (\4 & $F800) >> 6 | (\4 & $F80000) >> 19
ENDM

TestPalette:
    sgb_pal_packet $FFFFFF, $00639C, $104A84, $FF9C00

ChangeSGBBorder::
    push hl
    push de

    ld a, 1
    ldh [hIsSGB], a

    ; Freeze the screen for the upcoming transfers
    call FreezeSGBScreen
    call SGBDelay
    call .waitVBlank ; Wait an extra frame to make up for the SGB delay (can be removed if decompression takes long enough)
    ; Shut the LCD down to decompress directly to VRAM

    call .turnLCDOff

    ; Now, send the border while the static screen is being shown
    pop de
    ; ld de, CompressedBorderTiles
    ld hl, vSGBTransferArea
    ld b, 0 ; $1000 bytes
    call pb16_unpack_block
    push de
    call FillScreenWithSGBMap ; Also re-enables display and sets up render params
    ; Render params are written to the HRAM shadow regs, not the actual hardware regs.
    ; This would normally not be a problem, since the VBlank handler takes care of applying the copy,
    ; but said handler is disabled when this function is run.
    ; Set up the screen position and palette manually.
    xor a
    ldh [rSCY], a
    ldh [rSCX], a
    ld a, %11100100
    ldh [rBGP], a
    call .waitVBlank
    ld hl, TransferBorderTilesPacket
    call SendPackets
    call .turnLCDOff
    pop de
    ld hl, vSGBTransferArea
    ld b, 0 ; FIXME: hardcoded for the time being
    call pb16_unpack_block
    call SetupSGBLCDC
    call .waitVBlank
    ld hl, TransferBorderTilesPacket2
    call SendPackets
    call .turnLCDOff
    pop de
    ; ld de, CompressedBorderAttributes
    ld hl, vSGBTransferArea
    ld b, BORDER_ATTRIBUTE_SIZE / 16
    call pb16_unpack_block
    call SetupSGBLCDC
    call .waitVBlank
    ld hl, TransferBorderAttributesPacket
    call SendPackets

    ; Clear the garbage we transmitted by blanking the palette
    ; Thought of disabling the LCD, but it appears this doesn't blank the screen on SGB!
    xor a
    ldh [hBGP], a

    ; Disable manual paletting
    ld hl, DisablePalettesPacket
    call SendPackets

    ; TODO: parameterize this and make it happen when changing skins
    ld hl, TestPalette
    call SendPackets

    ; Unfreeze the screen
    ld hl, UnfreezeScreenPacket
    jp SendPacketNoDelay ; Tail call


;; This function is run while interrupts are disabled, so we're
;; just doing it the dirty way.
.turnLCDOff:
    ldh a, [rLY]
    cp SCRN_Y
    jr c, .turnLCDOff
    xor a
    ldh [rLCDC], a
    ret

.waitVBlank:
    ldh a, [rLY]
    cp SCRN_Y
    jr c, .waitVBlank
    ret
