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

(defparameter *all-tiles*
  (let ((tile-idx 0)
        (table (make-hash-table :test #'gif-data=)))
    (loop for x below (skippy:width *img*) by 8 do
      (loop for y below (skippy:height *img*) by 8 do
        (let ((new-tile (skippy:make-image :width 8 :height 8)))
          (skippy:composite *img* new-tile :sx x :sy y :dx 0 :dy 0)
          (when (not (gethash new-tile table))
            (setf (gethash new-tile table) tile-idx)
            (incf tile-idx)))))
    table))

(defun dump (img)
  (let ((dstream (skippy:make-data-stream :height (skippy:height img)
                                          :width (skippy:width img)
                                          :color-table *color-table*
                                          :initial-images (list img))))
    (skippy:output-data-stream dstream "/tmp/yeah.gif")))

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

(defun used (window)
  (remove-duplicates (apply #'append window) :test #'=))

(defstruct slice-diff
  incoming
  evicted)

(defun calculate-diffs (slices)
  (let ((slice-window (subseq slices 0 20))
        out)
    (loop for i from 21 below (length slices)
          for oldset = (used slice-window) do
            (pop slice-window)
            (setf slice-window (append slice-window (list (nth i slices))))
            (let ((newset (used slice-window)))
              (push (make-slice-diff
                     :incoming (set-difference newset oldset)
                     :evicted (set-difference oldset newset))
                    out)))
    (reverse out)))

(defparameter *slice-diffs-right*
  (calculate-diffs *slices*))

(defparameter *slice-diffs-left*
  (calculate-diffs (reverse *slices*)))

(defun format-slice-diff (slice-diffs)
  (let ((names (format nil "[~{~a~:*-_~a~^, ~}]" (loop for x being each hash-key of *all-tiles* collect (gethash x *all-tiles*))))
        (free (format nil "[~{~a~^, ~}]" (loop for i below 211 collect i) #+nil(loop for i from #x90 to #x163 collect i)))
        (diffs (format nil "[~{diff(~{[~{~A~:*-_~A~^, ~}], [~{~A~:*-_~A~^, ~}]~})~^, ~}]"
                       (cons
                        (list (used (subseq *slices* 0 20))
                              nil)
                        (loop for diff in slice-diffs
                              collect
                              (list (slice-diff-incoming diff)
                                    (slice-diff-evicted diff)))))))
    (format nil "[~a, ~a, ~a].~%" names diffs free)))

(defun solve (slice-diffs)
  (let ((data (format-slice-diff slice-diffs))
        (process (uiop:launch-program
                  (list "swipl" "./buttontool.pl")
                  :input :stream
                  :output :stream)))
    (unwind-protect
         (let ((input-stream (uiop:process-info-input process))
               (output-stream (uiop:process-info-output process)))

           (write-string data input-stream)
           (finish-output input-stream)

           ;; Read and process forms from subprocess stdout
           (loop for form = (ignore-errors (read output-stream nil))
                 while form do (format t "~a~%" form)))
      ;; Ensure process streams are closed
      (uiop:close-streams process))))
