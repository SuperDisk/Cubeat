(ql:quickload :skippy)

(defun gif-data= (i1 i2)
  (equalp (skippy:image-data i1) (skippy:image-data i2)))

(defun sxhash-gif-data (f)
  (sxhash (skippy:image-data f)))

(define-hash-table-test gif-data= sxhash-gif-data)

(defun gif->tiles (filename &key (dump nil))
  (let* ((in-stream (skippy:load-data-stream filename))
         (frames (skippy:images in-stream))
         (tiles (make-hash-table :test #'gif-data=))
         (tile-id 0))
    (loop for frame across frames do
      (loop for x from 0 to (skippy:width frame) by 8 do
        (loop for y from 0 to (skippy:height frame) by 8 do
          (let ((new-tile (skippy:make-image :width 8 :height 8)))
            (skippy:composite frame new-tile :sx x :sy y :dx 0 :dy 0)
            (when (not (gethash new-tile tiles))
              (setf (gethash new-tile tiles) (hash-table-count tiles))
              (incf tile-id))))))

    (when dump
      (maphash (lambda (k v)
                 (let ((new-data-stream (skippy:make-data-stream :width 8 :height 8)))
                   (skippy:add-image k new-data-stream)
                   (setf (skippy:color-table new-data-stream) (skippy:color-table in-stream))
                   (skippy:output-data-stream new-data-stream (format nil "~a.gif" v))))
               tiles))

    tiles))
