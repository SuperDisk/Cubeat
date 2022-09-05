;; This file stolen from https://github.com/ISSOtm/motherboard-gb

include "defines.asm"

SECTION "SGB setup", ROMX

TwoPlayersPacket:
    sgb_packet MLT_REQ, 1, 1
OnePlayerPacket:
    sgb_packet MLT_REQ, 1, 0

DisablePalettesPacket:
    sgb_packet ICON_EN, 1, %001 ; Disable palettes, keep other two

CompressedBorderTiles:
INCBIN "res/borders/SuperGameBoyFrame.borderchr.pb16"
TransferBorderTilesPacket:
    sgb_packet CHR_TRN, 1, %00 ; BG tiles, $00-7F
TransferBorderTilesPacket2:
    sgb_packet CHR_TRN, 1, %01 ; BG tiles, $80-FF

BORDER_ATTRIBUTE_SIZE = $880
CompressedBorderAttributes:
INCBIN "res/borders/SuperGameBoyFrame.borderattr.pb16"
TransferBorderAttributesPacket:
    sgb_packet PCT_TRN, 1

; hack
vSGBTransferArea EQU $8000

; Sets up a ton of SGB-related stuff
; The stuff in question takes a bunch of time, but we need to do it ASAP, basically
; Of course on a non-SGB system basically nothing will happen :D
DoSGBSetup::
    ld a, 1
    ldh [hIsSGB], a

    ; Freeze the screen for the upcoming transfers
    call FreezeSGBScreen
    call SGBDelay
    call .waitVBlank ; Wait an extra frame to make up for the SGB delay (can be removed if decompression takes long enough)
    ; Shut the LCD down to decompress directly to VRAM

    call .turnLCDOff

    ; Now, send the border while the static screen is being shown
    ld de, CompressedBorderTiles
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
    ld de, CompressedBorderAttributes
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
