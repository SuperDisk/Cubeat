(ql:quickload 'skippy)

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

(defparameter *gb-map*
  (let ((assignments (solve)))
    (loop for tile in *segmented-image*
          collect (cdr (assoc (gethash tile *all-tiles*) assignments)))))

(defparameter *gb-map-idxs*
  (loop for tile in *segmented-image*
        collect (gethash tile *all-tiles*)))

(defun chunk (list n)
  (loop for i below (length list) by n
        collect (subseq list i (+ i n))))
