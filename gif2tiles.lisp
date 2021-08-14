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

(defun dump-bytes (fname img-2bpp)
  (with-open-file (s fname
                     :direction :output
                     :element-type 'unsigned-byte
                     :if-exists :supersede)
    (loop for b in img-2bpp do
      (write-byte b s))))

(defun splitimg (img)
  (let ((out nil))
    (loop for y below (skippy:height img) by 8 do
      (loop for x below (skippy:width img) by 8 do
        (let ((new-tile (skippy:make-image :width 8 :height 8)))
          (skippy:composite img new-tile :sx x :sy y :dx 0 :dy 0)
          (push new-tile out))))
    (nreverse out)))

(defun wrap-location (loc)
  (let ((remainder (mod loc 20))
        (row (floor loc 20)))
    (+ #x9800 (* row 32) remainder)))

(defun tile-map->code (annotated-tmap)
  (let ((unique-tiles (remove-duplicates (mapcar #'cdr annotated-tmap)))
        #+nil(unique-pairs (remove-duplicates
                       (loop for (a b) on tmap by #'cddr
                             collect (cons (cdr a) (cdr b)))
                       :test #'equalp))
        #+nil(annotated-pairs
          (loop for (a b) on tmap by #'cddr
                for i from 0 by 2
                collect (cons a b))))

    ; a method
    (append
     (loop for tile in unique-tiles
           append
           (list* #x3E tile
                  (loop for (location . __tile) in (remove-if-not (lambda (x) (eql (cdr x) tile)) annotated-tmap)
                        append
                        (list #xEA
                              (ldb (byte 8 0) (wrap-location location))
                              (ldb (byte 8 8) (wrap-location location))))))

     '(#xC9)
     #+nil'(#xC3 #x30 #x40))
    #+nil ; sp method
    (append (loop for pair in unique-pairs
                  append
                  (list* #x31
                         (car pair)
                         (cdr pair)
                         (loop for (loc . _p) in (remove-if-not (lambda (x) (equalp (cdr x) pair)) annotated-pairs)
                               append
                               (list #x08
                                     (ldb (byte 8 0) (wrap-location loc))
                                     (ldb (byte 8 8) (wrap-location loc))))))
            '(#xC3 #x2c #x40))))

(defun image->annotated-tilemap (split-img tiles)
  (loop for tile in split-img
        for i from 0
        collect (cons i (min 255 (gethash tile tiles)))))

(defun dbg (f val)
  (funcall f val)
  val)

(defun gif->tiles (filename &key)
  (let* ((frames (skippy:images (skippy:load-data-stream filename)))
         (tiles (make-hash-table :test #'gif-data=))
         (indexes->tiles (make-hash-table))
         (diff-set nil)
         (frame-sets nil)
         (index-commonness nil))
    (let ((tile-id 0))
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
          (push frame-set frame-sets))))
    (setf frame-sets (reverse frame-sets))

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

    (loop for (before after) on frame-sets
          for i from 0 do
      (let ((diff nil))
        (when (and before after)
          (loop for idx being each hash-key of after do
            (when (null (gethash idx before))
              (push idx diff)))
          (push diff diff-set))))
    (setf diff-set (reverse diff-set))

    (let ((sorted-tiles nil))
      (loop for i from 0 to (1- (hash-table-count indexes->tiles)) do
        (push (cons i (gethash i indexes->tiles)) sorted-tiles))
      (setf sorted-tiles (mapcar #'cdr (sort sorted-tiles #'< :key #'car)))

      (dump-bytes "res/temp-bytes"
                 (loop for tile in sorted-tiles append (tile->2bpp tile))))

    (dump-bytes "res/temp-tilemap"
                (loop for tile in (splitimg (elt frame-set 0))
                      collect (gethash tile tiles)))

    (dump-bytes "res/temp-tilemap-code-initial"
                (tile-map->code (image->annotated-tilemap (splitimg (elt frames 0)) tiles)))
    (loop for (before after) on (cons (car (last (coerce frames 'list))) (coerce frames 'list))
          for i from 0
          when after do
            (let ((before-annotated-tm (image->annotated-tilemap (splitimg before) tiles))
                  (after-annotated-tm (image->annotated-tilemap (splitimg after) tiles)))
              (dump-bytes (format nil "res/temp-tilemap-code~a" i)
                          (tile-map->code
                           (dbg (lambda (x) (format t "~a map updates between frames ~a, ~a~%" (length x) i (1+ i)))
                                (loop for b in before-annotated-tm
                                      for a in after-annotated-tm
                                      when (not (equalp a b))
                                        collect a))))))



    (format t "~%")
    (format t "~a unique tiles~%" (hash-table-count tiles))
    (format t "~%")
    (loop for diff in diff-set
          for i from 0 do
          (format t "~a different tiles between frames ~a, ~a~%" (length diff) i (1+ i)))
    (format t "~%")
    (loop for sp-img in (mapcar #'splitimg (coerce frames 'list))
          for i from 0 do
          (format t "~a unique frames in frame ~a~%" (length (remove-duplicates sp-img :test #'equalp :key #'skippy:image-data)) i))

    (cons tiles diff-set)))

(defun pass () nil)
