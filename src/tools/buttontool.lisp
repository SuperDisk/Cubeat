(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:skippy :unix-opts) :silent t))

(opts:define-opts
  (:name :input
   :short #\i
   :long "input")
  (:name :map
   :short #\m
   :long "map")
  (:name :code
   :short #\c
   :long "code"))

(defun gif-data= (i1 i2)
  (equalp (skippy:image-data i1) (skippy:image-data i2)))

(defun sxhash-gif-data (f)
  (sxhash (skippy:image-data f)))

(define-hash-table-test gif-data= sxhash-gif-data)

(defparameter *data-stream*
  (skippy:load-data-stream "../res/menu/song_buttons.gif"))

(defparameter *img*
  (aref (skippy:images *data-stream*) 0))

(defparameter *color-table*
  (skippy:color-table *data-stream*))

(defun dump (img name)
  (let ((dstream (skippy:make-data-stream :height (skippy:height img)
                                          :width (skippy:width img)
                                          :color-table *color-table*
                                          :initial-images (list img))))
       (skippy:output-data-stream dstream (format nil "/tmp/~a.gif" name))))


(defparameter *all-tiles* (make-hash-table :test #'gif-data=))
(defparameter *segmented-image* nil)

(clrhash *all-tiles*)
(let ((tile-idx 0))
  (loop for y below (skippy:height *img*) by 8 do
    (loop for x below (skippy:width *img*) by 8 do
      (let ((new-tile (skippy:make-image :width 8 :height 8)))
        (skippy:composite *img* new-tile :sx x :sy y :dx 0 :dy 0)
        (push new-tile *segmented-image*)
        (when (not (gethash new-tile *all-tiles*))
          (setf (gethash new-tile *all-tiles*) tile-idx)
          (incf tile-idx))))))
(setf *segmented-image* (reverse *segmented-image*))

(defparameter *slices*
  (loop for x below (skippy:width *img*) by 8
        collect
        (loop for y below (skippy:height *img*) by 8
              collect
              (gethash
               (skippy:composite *img*
                                 (skippy:make-canvas :width 8 :height 8)
                                 :sx x :sy y :dx 0 :dy 0)
               *all-tiles*))))

(defun format-slice (slice)
  (format nil "[~{~a~:*-_~a~^, ~}]" slice))

(defun format-slices (slices)
  (format nil "[~{~a~^, ~}]" slices))

(defun format-input (tiles slices)
  (format nil "[~a, ~a].~%"
          (format-slices (mapcar #'format-slice slices))
          (format-slice tiles)))

(defun solve ()
  (let ((data (format-input (loop for k being each hash-key of *all-tiles*
                                  collect (gethash k *all-tiles*))
                            *slices*))
        (process (uiop:launch-program
                  (list "swipl" "./buttontool.pl")
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

(defvar *assignments* (solve))

(defparameter *gb-map*
  (loop for tile in *segmented-image*
        collect (cdr (assoc (gethash tile *all-tiles*) *assignments*))))

(defparameter *gb-map-idxs*
  (loop for tile in *segmented-image*
        collect (gethash tile *all-tiles*)))

(defun chunk (list n)
  (loop for i below (length list) by n
        collect (subseq list i (+ i n))))

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

(defun dbg (&rest args)
  (format t "~{~a ~}~%" args)
  (car args))

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

(defun write-tiles-2bpp (stream)
  (let* ((pairs
          (loop for key being each hash-key of *all-tiles*
                collect (cons key (gethash key *all-tiles*))))
        (sorted-pairs
          (sort pairs #'< :key #'cdr))
        (sorted-tiles (mapcar #'car sorted-pairs)))
    (loop for tile in sorted-tiles do
      (write-sequence (skippy:image-data tile) stream))))

(defun write-slice-code (slice name stream)
  (format stream "slice~a:~%" name)
  (let ((slice (remove-duplicates slice :test #'=))
        (bytes 1))
    (loop for tile in slice
          for index = (cdr (assoc tile *assignments*)) do
            (format stream "ld de, tiles+(~a*16)~%" tile)
            (format stream "ld hl, $8900+(~a*16)~%" index)
            (format stream "rst MemcpyTile~%")
            (incf bytes 5))
    (format stream "ret~%")
    bytes))

(defun main ()
  (let* ((cmd (opts:get-opts))
         (input-file (getf cmd :input))
         (map-file (getf cmd :map))
         (code-file (getf cmd :code)))
    (with-open-file (stream map-file
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-sequence *gb-map* stream))
    (with-open-file (stream code-file
                            :direction :output
                            :if-exists :supersede)
      (format stream "SECTION \"Slice code\", ROMX~%")
      (loop for slice in *slices*
            for i from 0 do
              (write-slice-code slice i stream))

      (format stream "slice_table::~%")
      (loop for i from 0 below (length *slices*) do
        (format stream "dw slice~a~%" i)))))
