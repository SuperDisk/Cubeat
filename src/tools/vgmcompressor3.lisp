(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:yason) :silent t))

(defconstant +min-length+ 3)               ; minimum reference size
(defconstant +max-length+ #x7F)            ; maximum literals / reference size
(defconstant +window-size+ #xFFF)          ; sliding window size
(defconstant +flag-ltrls+ 0)               ; indicates literals
(defconstant +flag-pair+ #x80)             ; indicates length,offset pair
(defconstant +flag-eof+ 0)                 ; end of compressed data
(defconstant +flag-megaframe+ #x80)
(defconstant +flag-reset-decompressor+ #x81)

(defstruct lz-seq
  offset
  length)

(defun file-stem (path)
  (let* ((pathname (pathname path))
         (file-name (file-namestring pathname))
         (dot-position (position #\. file-name :from-end t)))
    (if dot-position
        (subseq file-name 0 dot-position)
        file-name)))

(defun lz-find (data offset size maxlen)
  (let ((match (make-lz-seq :offset 0 :length 0))
        (length 0)
        (window 0))
    (if (< offset +window-size+)
        (setf window 0)
        (setf window (- offset +window-size+)))
    (loop while (< window offset) do
      (setf length 0)
      (when (= (aref data window) (aref data offset))
        (incf length)
        (loop while (= (aref data (+ window length))
                       (aref data (+ offset length)))
              do
                 (incf length)
                 (when (= length +max-length+)
                   (return))
                 (when (= length maxlen)
                   (return))
                 (when (> (+ window length) size)
                   (return))))
      (when (and (<= length maxlen)
                 (>= length +min-length+)
                 (> length (lz-seq-length match)))
        (setf match (make-lz-seq :offset window :length length)))
      (incf window))
    match))

(defun compress ()
  (let* ((outfile (cadr sb-ext:*posix-argv*))
         (loopframe (yason:parse (read-line)))
         (frames (yason:parse (read-line)))
         (framelens (mapcar (lambda (x) (1- (length x))) frames))
         (framedata (coerce (apply #'append (mapcar #'butlast frames)) '(vector (unsigned-byte 8))))
         (d 0)
         (b 0)
         (buffer '())
         (data framedata)
         (size (length data))
         (frame-need 0)
         (this-bank 0)
         (out-banks '())
         (cur-bank '())
         (megaframes '())
         (loopbank nil)
         (loopbyte nil)
         (fprocessed -1))
    (format t "Original file size ~a bytes~%" size)
    (labels ((writebytes (b)
               (incf this-bank (length b))
               (setf cur-bank (append b cur-bank)))
             (write-megaframe-ref (mf)
               (incf this-bank 2)
               (push (list mf) cur-bank))
             (flush-buffer ()
               (writebytes (list (logior +flag-ltrls+ b)))
               (writebytes buffer)
               (setf buffer '())
               (setf b 0)))
      (loop while (< d size) do
        (block continue
          (when (= frame-need 0)
            (incf fprocessed)
            (when buffer
              (flush-buffer))

            (when (not framelens)
              (return))

            (when (and cur-bank (not (listp (car cur-bank))))
              (writebytes (list +flag-eof+)))

            (setf frame-need (pop framelens))

            (when (>= (+ this-bank frame-need) (- #x4000 20))
              (format t "splitting bank at ~x~%" this-bank)
              (setf this-bank 0)
              (push +flag-eof+ cur-bank)
              (push cur-bank out-banks)
              (setf cur-bank '()))

            (when (= fprocessed loopframe)
              (format t "resetting compressor~%")
              (setf data (subseq data d))
              (setf size (- size d))
              (setf d 0)
              (writebytes (list +flag-reset-decompressor+))
              (setf loopbank (length out-banks))
              (setf loopbyte (1- (length cur-bank))))

            (cond
              ((string= (car (last (elt frames fprocessed))) "mega")

               (format t "Making megaframe ~a~%" frame-need)
               (writebytes (list +flag-megaframe+))
               (push (subseq data d (+ d frame-need)) megaframes)
               (setf data (concatenate '(vector (unsigned-byte 8))
                                       (subseq data 0 d)
                                       (subseq data (+ d frame-need))))
               (decf size frame-need)
               (write-megaframe-ref (1- (length megaframes)))
               (incf this-bank (length (car megaframes)))
               (setf frame-need 0)
               (return-from continue))
              ((string= (car (last (elt frames fprocessed))) "regular")
                                        ; pass
               )
              (t (format t "seriously wrong~%")))
            )

          (let ((best (lz-find data d size frame-need)))
            (if (> (lz-seq-length best) 0)
                (progn
                  (when buffer
                    (flush-buffer))
                  (writebytes (list (logior +flag-pair+ (lz-seq-length best))))
                  (writebytes (list
                               (ash (logand (- (lz-seq-offset best) d) #xFF00) -8)
                               (logand (- (lz-seq-offset best) d) #xFF)))
                  (incf d (lz-seq-length best))
                  (decf frame-need (lz-seq-length best)))
                (progn
                  (push (aref data d) buffer)
                  (incf d)
                  (incf b)
                  (decf frame-need)
                  (when (or (= b +max-length+) (= d size))
                    (flush-buffer)))))))

      (when (not (listp (car cur-bank)))
        (push +flag-eof+ cur-bank))
      (push +flag-eof+ cur-bank)
      (push cur-bank out-banks)

      (setf megaframes (reverse megaframes))

      (flet ((asm (el)
               (if (listp el)
                   (format nil "LOW(megaframe~a),HIGH(megaframe~:*~a)" (car el))
                   (format nil "~a" el))))
        (with-open-file (out outfile
                             :direction :output
                             :if-exists :supersede)
          (let ((stem (file-stem outfile)))
            (loop for (bank . banks) on (reverse out-banks)
                  for rbank = (reverse bank)
                  for idx from 0 do
                    (format out "SECTION \"music__~a~a\", ROMX[$4000]~%" stem idx)
                    (format out "~a~a::~%" stem idx)
                    (format out "db ~{~a~^,~}~%" (mapcar #'asm rbank))
                    (format out "db LOW(BANK(~a~a))~%" stem (if banks (1+ idx) loopbank))
                    (format out "dw ~a~%" (if banks #x4000 (+ #x4000 loopbyte)))

                    (loop for el in rbank when (listp el) do
                      (format out "megaframe~a:~%" (car el))
                      (format out "db ~{~a~^,~}~%"
                              (coerce (elt megaframes (car el)) 'list)))))))
      (format t "Compressed to ~a bytes~%"
              (+ (apply #'+ (mapcar #'length megaframes))
                 (apply #'+ (mapcar #'length out-banks))))
      (exit))))
