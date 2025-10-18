include "defines.asm"

; DEF NUM_SKINS = 2
DEF NUM_SKINS = 25

MACRO pal_packet
  .sgb::
  db (PAL01 << 3) | 1
  dw (\1 & $F8) << 7 | (\1 & $F800) >> 6 | (\1 & $F80000) >> 19
  dw (\2 & $F8) << 7 | (\2 & $F800) >> 6 | (\2 & $F80000) >> 19
  dw (\3 & $F8) << 7 | (\3 & $F800) >> 6 | (\3 & $F80000) >> 19
  dw (\4 & $F8) << 7 | (\4 & $F800) >> 6 | (\4 & $F80000) >> 19

  .cgb::
  ;; BGP
  db (\4 & $00FF00)>>8, (\4 & $FF0000)>>16, (\4 & $FF)
  db (\2 & $00FF00)>>8, (\2 & $FF0000)>>16, (\2 & $FF)
  db (\3 & $00FF00)>>8, (\3 & $FF0000)>>16, (\3 & $FF)
  db (\1 & $00FF00)>>8, (\1 & $FF0000)>>16, (\1 & $FF)

  ;; OBP1
  ; db (\2 & $00FF00)>>8, (\2 & $FF0000)>>16, (\2 & $FF)
  db (\1 & $00FF00)>>8, (\1 & $FF0000)>>16, (\1 & $FF)
  db (\3 & $00FF00)>>8, (\3 & $FF0000)>>16, (\3 & $FF)
  db (\4 & $00FF00)>>8, (\4 & $FF0000)>>16, (\4 & $FF)

  ;; OBP2
  ; db (\3 & $00FF00)>>8, (\3 & $FF0000)>>16, (\3 & $FF)
  db (\4 & $00FF00)>>8, (\4 & $FF0000)>>16, (\4 & $FF)
  db (\2 & $00FF00)>>8, (\2 & $FF0000)>>16, (\2 & $FF)
  db (\1 & $00FF00)>>8, (\1 & $FF0000)>>16, (\1 & $FF)
ENDM

SECTION "Title border", ROMX
frame_title_tiles:
INCBIN "res/borders/SplashScreen_Clean.borderchr.pb16"
frame_title_attrs:
INCBIN "res/borders/SplashScreen_Clean.borderattr.pb16"

SECTION "Menus border", ROMX
frame_menus_tiles:
INCBIN "res/borders/menus_Clean.borderchr.pb16"
frame_menus_attrs:
INCBIN "res/borders/menus_Clean.borderattr.pb16"

FOR I, 1, NUM_SKINS+1
  include "res/backgrounds/bg{02d:I}.asm"
ENDR

FOR I, 1, NUM_SKINS+1
  SECTION "frame_{02d:I}", ROMX
  frame_{02d:I}_tiles:
  INCBIN "res/borders/SuperGameBoyFrame_{02d:I}_Clean.borderchr.pb16"
  frame_{02d:I}_attrs:
  INCBIN "res/borders/SuperGameBoyFrame_{02d:I}_Clean.borderattr.pb16"
ENDR

MACRO defskin ; id, gfx_init, map0, block0, block1
  .blockset_bank: db BANK(\4) ; bank of blockset
  .block0: dw \4
  .block1: dw \5 ; block0 and block1

  .block_gfx_offset: dw \1*64 ; offset of block gfx

  .gfx_init_bank: db BANK(\2) ; bank of gfx_init
  .gfx_init: dw \2 ; gfx_init

  .map0_bank: db BANK(\3) ; bank of map0
  .map0: dw \3 ; map0

  .border_bank:: db BANK(\6) ; bank of border tiles
  .border_data: dw \6, \7 ; border tiles and border attrs
  .palette: dw \8 ; sgb palette
ENDM

;; Defines its own sections
include "res/sprite_block_gfx.sep1.2bpp.asm"

SECTION "Skins", ROMX

pal_splash:: pal_packet $faffff, $e68484, $000000, $d61313
pal_title:: pal_packet $fff9f4, $6d94b1, $516a95, $29334d
pal0:: pal_packet $ffffff, $00649e, $124882, $ff9d00
pal1:  pal_packet $0036b2, $ffc4a2, $ff766e, $f4ff00
pal2:  pal_packet $ded40f, $fffded, $57dbf1, $5a55ae
pal3:  pal_packet $ffee00, $8763cc, $763eb2, $07b9f1
pal4:  pal_packet $9c00cd, $fdc540, $e99418, $ffffff
pal5:  pal_packet $ffffff, $ffd300, $ff9800, $c44a1d
pal6:  pal_packet $e7e312, $347648, $4c5161, $5cae23
pal7:  pal_packet $0036b2, $ffc4a2, $ff766e, $f4ff00
pal8:  pal_packet $ded40f, $fffded, $57dbf1, $5a55ae
pal9:  pal_packet $9c00cd, $fdc540, $e99418, $ffffff
pal10: pal_packet $ded40f, $fffded, $57dbf1, $5a55ae
pal11: pal_packet $0036b2, $ffc4a2, $ff766e, $f4ff00
pal12: pal_packet $ffffff, $9fa395, $b0b6a1, $e10019
pal13: pal_packet $4c5161, $cdd912, $8dc112, $ffffff
pal14: pal_packet $ffee00, $8763cc, $763eb2, $07b9f1
pal15: pal_packet $ffffff, $ffd300, $ff9800, $c44a1d
pal16: pal_packet $9c00cd, $fdc540, $e99418, $ffffff
pal17: pal_packet $ded40f, $fffded, $57dbf1, $5a55ae
pal18: pal_packet $fffa00, $0c5c70, $00362a, $00bcd8
pal19: pal_packet $0036b2, $ffc4a2, $ff766e, $f4ff00
pal20: pal_packet $ffffff, $0008f7, $00108c, $f708ff
pal21: pal_packet $ffffff, $9fa395, $b0b6a1, $e10019
pal22: pal_packet $c3db7b, $8aa346, $617b38, $003d2e
pal23: pal_packet $ffff00, $000091, $000000, $ff0000
pal24: pal_packet $9c00cd, $fdc540, $e99418, $ffffff

skin_title::
db BANK(frame_title_tiles)
dw frame_title_tiles, frame_title_attrs
dw pal_title

skin_menus::
db BANK(frame_menus_tiles)
dw frame_menus_tiles, frame_menus_attrs
dw pal_title

skins::

skin0:: defskin 0, bg01_gfx_init, bg01_map0, blockset_0_0, blockset_0_1, frame_01_tiles, frame_01_attrs, pal0
skin1:: defskin 1, bg02_gfx_init, bg02_map0, blockset_1_0, blockset_1_1, frame_02_tiles, frame_02_attrs, pal1
skin2:: defskin 2, bg03_gfx_init, bg03_map0, blockset_2_0, blockset_2_1, frame_03_tiles, frame_03_attrs, pal2
skin3:: defskin 3, bg04_gfx_init, bg04_map0, blockset_3_0, blockset_3_1, frame_04_tiles, frame_04_attrs, pal3
skin4:: defskin 4, bg05_gfx_init, bg05_map0, blockset_4_0, blockset_4_1, frame_05_tiles, frame_05_attrs, pal4
skin5:: defskin 5, bg06_gfx_init, bg06_map0, blockset_5_0, blockset_5_1, frame_06_tiles, frame_06_attrs, pal5
skin6:: defskin 6, bg07_gfx_init, bg07_map0, blockset_6_0, blockset_6_1, frame_07_tiles, frame_07_attrs, pal6
skin7:: defskin 7, bg08_gfx_init, bg08_map0, blockset_7_0, blockset_7_1, frame_08_tiles, frame_08_attrs, pal7
skin8:: defskin 8, bg09_gfx_init, bg09_map0, blockset_8_0, blockset_8_1, frame_09_tiles, frame_09_attrs, pal8
skin9:: defskin 9, bg10_gfx_init, bg10_map0, blockset_9_0, blockset_9_1, frame_10_tiles, frame_10_attrs, pal9
skin10:: defskin 10, bg11_gfx_init, bg11_map0, blockset_10_0, blockset_10_1, frame_11_tiles, frame_11_attrs, pal10
skin11:: defskin 11, bg12_gfx_init, bg12_map0, blockset_11_0, blockset_11_1, frame_12_tiles, frame_12_attrs, pal11
skin12:: defskin 12, bg13_gfx_init, bg13_map0, blockset_12_0, blockset_12_1, frame_13_tiles, frame_13_attrs, pal12
skin13:: defskin 13, bg14_gfx_init, bg14_map0, blockset_13_0, blockset_13_1, frame_14_tiles, frame_14_attrs, pal13
skin14:: defskin 14, bg15_gfx_init, bg15_map0, blockset_14_0, blockset_14_1, frame_15_tiles, frame_15_attrs, pal14
skin15:: defskin 15, bg16_gfx_init, bg16_map0, blockset_15_0, blockset_15_1, frame_16_tiles, frame_16_attrs, pal15
skin16:: defskin 16, bg17_gfx_init, bg17_map0, blockset_16_0, blockset_16_1, frame_17_tiles, frame_17_attrs, pal16
skin17:: defskin 17, bg18_gfx_init, bg18_map0, blockset_17_0, blockset_17_1, frame_18_tiles, frame_18_attrs, pal17
skin18:: defskin 18, bg19_gfx_init, bg19_map0, blockset_18_0, blockset_18_1, frame_19_tiles, frame_19_attrs, pal18
skin19:: defskin 19, bg20_gfx_init, bg20_map0, blockset_19_0, blockset_19_1, frame_20_tiles, frame_20_attrs, pal19
skin20:: defskin 20, bg21_gfx_init, bg21_map0, blockset_20_0, blockset_20_1, frame_21_tiles, frame_21_attrs, pal20
skin21:: defskin 21, bg22_gfx_init, bg22_map0, blockset_21_0, blockset_21_1, frame_22_tiles, frame_22_attrs, pal21
skin22:: defskin 22, bg23_gfx_init, bg23_map0, blockset_22_0, blockset_22_1, frame_23_tiles, frame_23_attrs, pal22
skin23:: defskin 23, bg24_gfx_init, bg24_map0, blockset_23_0, blockset_23_1, frame_24_tiles, frame_24_attrs, pal23
skin24:: defskin 24, bg25_gfx_init, bg25_map0, blockset_23_0, blockset_23_1, frame_25_tiles, frame_25_attrs, pal24

skin_table::
FOR I, NUM_SKINS
  dw skin{d:I}
ENDR