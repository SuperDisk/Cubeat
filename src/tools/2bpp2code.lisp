(defun conv-2bpp (filename)
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (loop repeat 24
          for blockset = (loop repeat 64 collect (read-byte in))
          for bs-count from 0 do
            (format t "SECTION \"Blockset ~a\",ROMX~%" bs-count)
            (loop for i from 0 below 64 by 16
                  for block = (subseq blockset i (+ i 16)) do
                    (format t "blockset_~a_~a::~%" bs-count (floor i 16))
                    (format t "xor a~%")
                    (format t "ldh [rIF], a~%")
                    (format t "halt~%")
                    (format t "nop~%")
                    (loop for b in block
                          for bytecnt from 0 do
                            (when (and (not (zerop bytecnt))
                                       (zerop (mod bytecnt 12)))
                              (format t "xor a~%")
                              (format t "ldh [rIF], a~%")
                              (format t "halt~%"))
                            (format t "ld a, $~x~%" b)
                            (format t "ld [hl+], a~%"))
                    (format t "ret~%")))))

(defun main ()
  (apply #'conv-2bpp (cdr sb-ext:*posix-argv*))
  (exit))
