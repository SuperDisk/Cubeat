DEF f_vgm_literals_end = 0
DEF f_vgm_literals = 1
DEF f_empty_frame = 2
DEF f_switch_port = 3

MACRO dwbe
  db HIGH(\1), LOW(\1)
ENDM