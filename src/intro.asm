
SECTION "Intro", ROMX

Intro::
; Remove this line
	rst $38

; Put your code here!
	jr @

incbin "res/title.2bpp"