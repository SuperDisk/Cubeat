(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria) :silent t))

(defun generate (gfx asm)
  (let* ((bytes (coerce (alexandria:read-stream-content-into-byte-vector *standard-input*) 'list))
         (tiles (loop for bs = bytes then (nthcdr 16 bs)
                      while bs
                      collect (subseq bs 0 16)))
         (rows (loop for rs = tiles then (nthcdr 10 rs)
                     while rs
                     collect (cons :no-break (subseq rs 0 10))))
         (rows2 (loop with rs = rows
                      with out
                      finally (return (reverse out))
                      while rs do
                        (if (apply #'= (apply #'append (cdar rs)))
                            (progn
                              (setf rs (cdr rs))
                              (setf (caadr out) :break))
                            (progn
                              (push (car rs) out)
                              (push (cadr rs) out)
                              (setf rs (cddr rs))))))
         (rowpairs (loop for (a b) on rows2 by #'cddr
                         collect (cons a b)))
         (trimmed-rows (loop for (a . b) in rowpairs
                             collect
                             (cons (car a)
                                   (loop for t1 in (cdr a)
                                         for t2 in (cdr b)
                                         for x = (append t1 t2)
                                         while (not (apply #'= x))
                                         collect x)))))
    (loop for row in trimmed-rows
          for (broken . tiles) = row
          with idx = 1 do
            (write-sequence (apply #'append tiles) gfx)
            (format asm "db ~{~a,~}0,~a,$~x~%"
                    (loop for i from idx
                          repeat (length tiles)
                          collect (1+ (rem (1- i) 64)))
                    (if (eq broken :break) 8 0)
                    (ceiling (* (length tiles) 16) 4))
            (format asm "dw credits_scroll_gfx + $~x, $8000 + $~x~%"
                    (* (1- idx) 16 2) ; source
                    (* (rem (1- idx) 64) 16 2) ; dest
                    )
            (incf idx (length tiles)))
    (format asm ";;~%")
    (loop for row in trimmed-rows
          for (broken . tiles) = row
          repeat 6
          with idx = 1 do
            (format asm "db 0,~a,$~x~%"
                    (if (eq broken :break) 8 0)
                    (ceiling (* (length tiles) 16) 4))
            (format asm "dw credits_scroll_gfx + $~x, $8000 + $~x~%"
                    (* (1- idx) 16 2) ; source
                    (* (rem (1- idx) 64) 16 2) ; dest
                    )
            (incf idx (length tiles)))))

(defun main ()
  (destructuring-bind (gfx-file asm-file) (cdr sb-ext:*posix-argv*)
    (with-open-file (gfx gfx-file
                         :if-exists :supersede
                         :direction :output
                         :element-type '(unsigned-byte 8))
      (with-open-file (asm asm-file
                           :if-exists :supersede
                           :direction :output)
        (generate gfx asm)))
    (exit)))
