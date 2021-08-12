(ql:quickload :skippy)

(defun gif-data= (i1 i2)
  (equalp (skippy:image-data i1) (skippy:image-data i2)))

(defun sxhash-gif-data (f)
  (sxhash (skippy:image-data f)))

(define-hash-table-test gif-data= sxhash-gif-data)

(defun getimg (fname)
  (elt (skippy:images (skippy:load-data-stream fname)) 0))

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

(defun image->2bpp (img)
  (let ((out nil))
    (loop for y below (skippy:height img) by 8 do
      (loop for x below (skippy:width img) by 8 do
        (let ((new-tile (skippy:make-image :width 8 :height 8)))
          (skippy:composite img new-tile :sx x :sy y :dx 0 :dy 0)
          (setf out (append out (tile->2bpp new-tile))))))
    out))

(defun dump-2bpp (img-2bpp)
  (with-open-file (s "res/temp-bytes"
                     :direction :output
                     :element-type 'unsigned-byte
                     :if-exists :supersede)
    (loop for b in img-2bpp do
      (write-byte b s))))

(defun gif->tiles (filename &key (dump nil))
  (let* ((in-stream (skippy:load-data-stream filename))
         (frames (skippy:images in-stream))
         (tiles (make-hash-table :test #'gif-data=))
         (indexes->tiles (make-hash-table))
         (tile-id 0)
         (diff-set nil)
         (frame-sets nil)
         (index-commonness nil))
    (loop for frame across frames do
      (let ((frame-set (make-hash-table)))
        (loop for y below (skippy:height frame) by 8 do
          (loop for x below (skippy:width frame) by 8 do
            (let ((new-tile (skippy:make-image :width 8 :height 8)))
              (skippy:composite frame new-tile :sx x :sy y :dx 0 :dy 0)
              (when (not (gethash new-tile tiles))
                (setf (gethash new-tile tiles) tile-id)
                (incf tile-id))
              (setf (gethash (gethash new-tile tiles) frame-set) 0))))
        (push frame-set frame-sets)))
    (setf frame-sets (nreverse frame-sets))

    (loop for tile being each hash-key of tiles do
      (let ((idx (gethash tile tiles)))
        (setf (gethash idx indexes->tiles) tile)))

    (let ((commonness-hash (make-hash-table)))
      (loop for frame in frame-sets do
        (loop for idx being each hash-key of frame do
          (setf (gethash idx commonness-hash) (1+ (gethash idx commonness-hash 0)))))

      (loop for idx being each hash-key of commonness-hash do
        (setf index-commonness (acons idx (gethash idx commonness-hash) index-commonness)))
      (setf index-commonness (sort index-commonness #'< :key 'cdr)))

    (when dump
      (maphash (lambda (k v)
                 (let ((new-data-stream (skippy:make-data-stream :width 8 :height 8)))
                   (skippy:add-image k new-data-stream)
                   (setf (skippy:color-table new-data-stream) (skippy:color-table in-stream))
                   (skippy:output-data-stream new-data-stream (format nil "~a.gif" v))))
               tiles))

    (loop for (before after) on frame-sets
          for i from 0 do
      (let ((diff nil))
        (when (and before after)
          (loop for idx being each hash-key of after do
            (when (null (gethash idx before))
              (push idx diff)))

          (when dump
            (ensure-directories-exist (format nil "~ato~a/" i (1+ i)))
            (loop for idx in diff do
              (let ((new-data-stream (skippy:make-data-stream :width 8 :height 8)))
                (skippy:add-image (gethash idx indexes->tiles) new-data-stream)
                (setf (skippy:color-table new-data-stream) (skippy:color-table in-stream))
                (skippy:output-data-stream new-data-stream (format nil "~ato~a/~a.gif" i (1+ i) idx)))))

          (push diff diff-set))))
    (setf diff-set (nreverse diff-set))
    (cons tiles diff-set)))
