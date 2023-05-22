;; This file defines a section that consumes the entire 256th bank. Due to the
;; way the Cubeat mapper works and for PCB space reasons, the uppermost bit of
;; the selected bank is always passed through to the memory chip, even if the GB
;; is accessing the "non-movable" bank ($0000-$3FFF). This means that if we set
;; rROMB1 to 1, then bank *zero* changes as well. We can work around this by
;; duplicating the contents of bank zero into bank 256. RGBDS doesn't provide an
;; easy way of doing it, so we just define this empty region and then copy it in
;; at build time with `dd`.

SECTION "Bank 256 padding", ROMX, BANK[256]
ds 1024*16