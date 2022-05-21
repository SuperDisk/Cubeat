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
    ; Request multiplayer mode, which a non-SGB device will ignore
    ld hl, TwoPlayersPacket
    call SendPackets
    lb bc, 2, LOW(rP1)
.tryAgain
    ; Now poll buttons (these are the only thing a real SGB cares about)...
    ld a, $10
    ld [$ff00+c], a
    ; ...and say we're over.
    ld a, $30
    ld [$ff00+c], a
    ; A non-SGB won't have cared the slightest about our little ballet here
    ; But a SGB will know that we have polled one joypad, and will switch to the next one
    ; And we can read that out!
    ld a, [$ff00+c]
    and $03
    cp $03 ; Value returned when Player 1 is in effect ($XF)
    jr nz, .isSGB
    ; Okay, but maybe we just genuinely polled Player 1. Let's try again.
    dec b
    jr nz, .tryAgain
    ret


.isSGB
    ; Freeze the screen for the upcoming transfers
    call FreezeSGBScreen
    rst WaitVBlank ; Wait an extra frame to make up for the SGB delay (can be removed if decompression takes long enough)
    ; Shut the LCD down to decompress directly to VRAM
    xor a
    ldh [hLCDC], a
    inc a ; ld a, 1
    ldh [hIsSGB], a
    rst WaitVBlank

    ; Now, send the border while the static screen is being shown
    ld de, CompressedBorderTiles
    ld hl, vSGBTransferArea
    ld b, 0 ; $1000 bytes
    call pb16_unpack_block
    push de
    call FillScreenWithSGBMap ; Also re-enables display and sets up render params
    rst WaitVBlank ; Wait for the first blank frame to display; the transfer will start at the end of the following frame
    ld hl, TransferBorderTilesPacket
    call SendPackets
    xor a
    ldh [hLCDC], a
    rst WaitVBlank
    pop de
    ld hl, vSGBTransferArea
    ld b, 0 ; FIXME: hardcoded for the time being
    call pb16_unpack_block
    call SetupSGBLCDC
    rst WaitVBlank
    ld hl, TransferBorderTilesPacket2
    call SendPackets
    xor a
    ldh [hLCDC], a
    rst WaitVBlank
    ld de, CompressedBorderAttributes
    ld hl, vSGBTransferArea
    ld b, BORDER_ATTRIBUTE_SIZE / 16
    call pb16_unpack_block
    call SetupSGBLCDC
    rst WaitVBlank
    ld hl, TransferBorderAttributesPacket
    call SendPackets

    ; Now send the attribute files...
    xor a
    ldh [hLCDC], a
    rst WaitVBlank

    ; Clear the garbage we transmitted by blanking the palette
    ; Thought of disabling the LCD, but it appears this doesn't blank the screen on SGB!
    xor a
    ldh [hBGP], a
    ; Disable manual paletting
    ld hl, DisablePalettesPacket
    call SendPackets

    ; We don't unfreeze the screen, this will be done by the next PAL_SET packet

    ld hl, OnePlayerPacket ; We're not gonna use multiplayer capabilities, though.
    jp SendPacketNoDelay ; Tail call
