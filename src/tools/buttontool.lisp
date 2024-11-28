(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:skippy :unix-opts) :silent t))

(opts:define-opts
  (:name :input
   :short #\i
   :long "input"
   :arg-parser #'identity)
  (:name :map
   :short #\m
   :long "map"
   :arg-parser #'identity)
  (:name :code
   :short #\c
   :long "code"
   :arg-parser #'identity)
  (:name :graphics
   :short #\g
   :long "graphics"
   :arg-parser #'identity))

(defun dbg (&rest args)
  (format t "~{~a ~}~%" args)
  (car args))

(defun gif-data= (i1 i2)
  (equalp (skippy:image-data i1) (skippy:image-data i2)))

(defun sxhash-gif-data (f)
  (sxhash (skippy:image-data f)))

(define-hash-table-test gif-data= sxhash-gif-data)

(defun format-slice (slice)
  (format nil "[~{~a~:*-_~a~^, ~}]" slice))

(defun format-slices (slices)
  (format nil "[~{~a~^, ~}]" slices))

(defun format-input (tiles slices)
  (format nil "[~a, ~a].~%"
          (format-slices (mapcar #'format-slice slices))
          (format-slice tiles)))

(defun solve (all-tiles slices)
  (let ((data (format-input (loop for k being each hash-key of all-tiles
                                  collect (gethash k all-tiles))
                            slices))
        (process (uiop:launch-program
                  (list "swipl" "/home/npfaro/projects/Cubeat/src/tools/buttontool.pl")
                  :input :stream
                  :output :stream)))
    (unwind-protect
         (let ((input-stream (uiop:process-info-input process))
               (output-stream (uiop:process-info-output process)))

           (write-string data input-stream)
           (finish-output input-stream)

           (loop for form = (ignore-errors (read output-stream nil))
                 while form collect form))
      (uiop:close-streams process))))

(defun int->bits (int)
  (cond
    ((eql int 0) (values 0 0))
    ((eql int 1) (values 0 1))
    ((eql int 2) (values 1 0))
    ((eql int 3) (values 1 1))))

(defun tile->2bpp (img)
  (let ((data nil)
        (current-byte1 0)
        (current-byte2 0)
        (byte-index 7))
    (loop for idx across (skippy:image-data img) do
      (multiple-value-bind (b1 b2) (int->bits idx)
        (setf (ldb (byte 1 byte-index) current-byte1) b1)
        (setf (ldb (byte 1 byte-index) current-byte2) b2)
        (decf byte-index)
        (when (< byte-index 0)
          (push current-byte1 data)
          (push current-byte2 data)
          (setf byte-index 7))))
    (reverse data)))

(defun write-tiles-2bpp (all-tiles stream)
  (let* ((pairs
           (loop for key being each hash-key of all-tiles
                 collect (cons key (gethash key all-tiles))))
         (sorted-pairs
           (sort pairs #'< :key #'cdr))
         (sorted-tiles (mapcar #'car sorted-pairs)))
    (loop for tile in sorted-tiles do
      (write-sequence (tile->2bpp tile) stream))))

(defun write-slice-code (slice name assignments stream)
  (format stream "slice~a:~%" name)
  (let ((slice (remove-duplicates slice :test #'=))
        (bytes 1))
    (loop for tile in slice
          for index = (cdr (assoc tile assignments))
          for upcounter from 0
          for counter downfrom (1- (length slice)) do
            (format stream "ld de, song_buttons_gfx+(~a*16)~%" tile)
            (format stream "ld hl, $8900+(~a*16)~%" index)

            (if (>= upcounter 6)
                (progn
                  (format stream "ld c, 16~%")
                  (if (zerop counter)
                      (format stream "jp LCDMemcpySmall~%")
                      (format stream "call LCDMemcpySmall~%")))
                (if (zerop counter)
                    (format stream "jp MemcpyTile~%")
                    (format stream "rst MemcpyTile~%")))
            (incf bytes 5))
    bytes))

(defun main ()
  (let* ((cmd (opts:get-opts))
         (input-file (getf cmd :input))
         (map-file (getf cmd :map))
         (code-file (getf cmd :code))
         (graphics-file (getf cmd :graphics))
         (data-stream (skippy:load-data-stream input-file))
         (img (aref (skippy:images data-stream) 0))
         (all-tiles (make-hash-table :test #'gif-data=))
         (segmented-image nil))
    (clrhash all-tiles)
    (let ((tile-idx 0))
      (loop for y below (skippy:height img) by 8 do
        (loop for x below (skippy:width img) by 8 do
          (let ((new-tile (skippy:make-image :width 8 :height 8)))
            (skippy:composite img new-tile :sx x :sy y :dx 0 :dy 0)
            (push new-tile segmented-image)
            (when (not (gethash new-tile all-tiles))
              (setf (gethash new-tile all-tiles) tile-idx)
              (incf tile-idx))))))
    (setf segmented-image (reverse segmented-image))
    (let* ((slices
             (loop for x below (skippy:width img) by 8
                   collect
                   (loop for y below (skippy:height img) by 8
                         collect
                         (gethash
                          (skippy:composite img
                                            (skippy:make-canvas :width 8 :height 8)
                                            :sx x :sy y :dx 0 :dy 0)
                          all-tiles))))
           (assignments (solve all-tiles slices))
           (gb-map
             (loop for tile in segmented-image
                   collect (cdr (assoc (gethash tile all-tiles) assignments)))))
      (with-open-file (stream graphics-file
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        (write-tiles-2bpp all-tiles stream))
      (with-open-file (stream map-file
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        (write-sequence (mapcar (lambda (x) (mod (+ x #x90) #x100)) gb-map) stream))
      (with-open-file (stream code-file
                              :direction :output
                              :if-exists :supersede)
        (loop for slice in slices
              for i from 0 do
                (write-slice-code slice i assignments stream))

        (format stream "slice_table::~%")
        (loop for i from 0 below (length slices) do
          (format stream "dw slice~a~%" i)))))
  (exit))

#+nil
(defun write-tile-code (tile name stream)
  (format stream "tile~a:~%" name)
  (let ((data (tile->2bpp tile))
        (bytes 0))
    (loop for b in data
          for i from (length data) downto 0
          with old-value = -1 do
            (cond
              ((= old-value b))
              ((= b (logxor old-value #xFF))
               (format stream "cpl a~%")
               (incf bytes))
              ((zerop b)
               (format stream "xor a~%")
               (incf bytes))
              (t
               (format stream "ld a, $~x~%" b)
               (incf bytes 2)))
            (setf old-value b)
            (when (not (zerop i))
              (format stream "ld [hl+], a~%")
              (incf bytes)))
    (format stream "ret~%")
    bytes))
