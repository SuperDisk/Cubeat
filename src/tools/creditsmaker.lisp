(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria) :silent t))

(defun generate (gfx asm)
  (let* ((bytes (coerce (alexandria:read-stream-content-into-byte-vector *standard-input*) 'list))
         (tiles (loop for bs = bytes then (nthcdr 16 bs)
                      while bs
                      collect (subseq bs 0 16)))
         (rows (loop for rs = tiles then (nthcdr 10 rs)
                     while rs
                     collect (subseq rs 0 10)))
         (rows2 (loop with rs = rows
                      with out
                      finally (return (reverse out))
                      while rs do
                        (if (apply #'= (apply #'append (car rs)))
                            (setf rs (cdr rs))
                            (progn
                              (push (car rs) out)
                              (push (cadr rs) out)
                              (setf rs (cddr rs))))))
         (rowpairs (loop for (a b) on rows2 by #'cddr
                         collect (cons a b)))
         (trimmed-rows (loop for (a . b) in rowpairs
                             collect
                             (loop for t1 in a
                                   for t2 in b
                                   for x = (append t1 t2)
                                   while (not (apply #'= x))
                                   collect x))))
    (loop for row in trimmed-rows
          with idx = 1 do
            (write-sequence (apply #'append row) gfx)
            (format asm "db ~{~a,~}0~%" (loop for i from idx
                                              repeat (length row)
                                              collect i
                                              finally (setf idx i))))))

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
