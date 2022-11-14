include "defines.asm"

; include "res/cosmic.asm"
; include "res/sxtnt.asm"
include "res/asof.asm"
; include "res/zen.asm"
; include "res/sxtnt3.asm"
; include "res/sxtnt4.asm"
; include "res/pocket.asm"
; include "res/onlyopl2.asm"

include "res/backgrounds/bg01.asm"
; include "res/backgrounds/bg02.asm"
; include "res/backgrounds/bg03.asm"
; include "res/backgrounds/bg04.asm"
; include "res/backgrounds/bg05.asm"
; include "res/backgrounds/bg06.asm"
; include "res/backgrounds/bg07.asm"
; include "res/backgrounds/bg08.asm"
; include "res/backgrounds/bg09.asm"
; include "res/backgrounds/bg10.asm"
; include "res/backgrounds/bg11.asm"
; include "res/backgrounds/bg12.asm"
; include "res/backgrounds/bg13.asm"
; include "res/backgrounds/bg14.asm"
; include "res/backgrounds/bg15.asm"
; include "res/backgrounds/bg16.asm"
; include "res/backgrounds/bg17.asm"
; include "res/backgrounds/bg18.asm"
; include "res/backgrounds/bg19.asm"
; include "res/backgrounds/bg20.asm"
; include "res/backgrounds/bg21.asm"
; include "res/backgrounds/bg22.asm"
; include "res/backgrounds/bg23.asm"
; include "res/backgrounds/bg24.asm"
; include "res/backgrounds/bg25.asm"

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

EXPORT bg01_gfx_init
EXPORT bg01_map0

MACRO defskin ; id, gfx_init, map0, block0, block1
db BANK(\4) ; bank of blockset
dw \4, \5 ; block0 and block1

db \1*64 ; offset of block gfx

db BANK(\2) ; bank of gfx_init
dw \2 ; gfx_init

db BANK(\3) ; bank of map0
dw \3 ; map0

db BANK(\6) ; bank of border tiles
dw \6, \7 ; border tiles and border attrs
ENDM

;; Defines its own sections
include "res/sprite_block_gfx.sep1.2bpp.asm"

SECTION "Skins", ROM0

skins::
skin0:: defskin 0, bg01_gfx_init, bg01_map0, blockset_0_0, blockset_0_1, frame_01_tiles, frame_01_attrs
; skin1:: defskin 3, bg04_gfx_init, bg04_map0, blockset_3_0, blockset_3_1
;  defskin 2, bg03_gfx_init, bg03_map0, blockset3_0, blockset3_1
;  defskin 3, bg04_gfx_init, bg04_map0, blockset4_0, blockset4_1
;  defskin 4, bg05_gfx_init, bg05_map0, blockset5_0, blockset5_1
;  defskin 5, bg06_gfx_init, bg06_map0, blockset6_0, blockset6_1
;  defskin 6, bg07_gfx_init, bg07_map0, blockset7_0, blockset7_1
;  defskin 7, bg08_gfx_init, bg08_map0, blockset8_0, blockset8_1
;  defskin 8, bg09_gfx_init, bg09_map0, blockset9_0, blockset9_1
;  defskin 9, bg10_gfx_init, bg10_map0, blockset10_0, blockset10_1
;  defskin 10, bg11_gfx_init, bg11_map0, blockset11_0, blockset11_1
;  defskin 11, bg12_gfx_init, bg12_map0, blockset12_0, blockset12_1
;  defskin 12, bg13_gfx_init, bg13_map0, blockset13_0, blockset13_1
;  defskin 13, bg14_gfx_init, bg14_map0, blockset14_0, blockset14_1
;  defskin 14, bg15_gfx_init, bg15_map0, blockset15_0, blockset15_1
;  defskin 15, bg16_gfx_init, bg16_map0, blockset16_0, blockset16_1
;  defskin 16, bg17_gfx_init, bg17_map0, blockset17_0, blockset17_1
;  defskin 17, bg18_gfx_init, bg18_map0, blockset18_0, blockset18_1
;  defskin 18, bg19_gfx_init, bg19_map0, blockset19_0, blockset19_1
;  defskin 19, bg20_gfx_init, bg20_map0, blockset20_0, blockset20_1
;  defskin 20, bg21_gfx_init, bg21_map0, blockset21_0, blockset21_1
;  defskin 21, bg22_gfx_init, bg22_map0, blockset22_0, blockset22_1
;  defskin 22, bg23_gfx_init, bg23_map0, blockset23_0, blockset23_1
;  defskin 23, bg24_gfx_init, bg24_map0, blockset24_0, blockset24_1

skin_table:
dw skin0
; dw skin1
; dw skin2
; dw skin3
; dw skin4
; dw skin5
; dw skin6
; dw skin7
; dw skin8
; dw skin9
; dw skin10
; dw skin11
; dw skin12
; dw skin13
; dw skin14
; dw skin15
; dw skin16
; dw skin17
; dw skin18
; dw skin19
; dw skin20
; dw skin21
; dw skin22
; dw skin23
; dw skin24