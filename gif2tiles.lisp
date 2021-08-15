(ql:quickload :skippy)

(defun gif-data= (i1 i2)
  (equalp (skippy:image-data i1) (skippy:image-data i2)))

(defun sxhash-gif-data (f)
  (sxhash (skippy:image-data f)))

(define-hash-table-test gif-data= sxhash-gif-data)

;; Ripped from rosetta code
(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

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
  (loop for y below (skippy:height img) by 8 do
    (loop for x below (skippy:width img) by 8
          append (let ((new-tile (skippy:make-image :width 8 :height 8)))
                   (skippy:composite img new-tile :sx x :sy y :dx 0 :dy 0)
                   (tile->2bpp new-tile)))))

(defun dump-bytes (fname img-2bpp)
  (with-open-file (s fname
                     :direction :output
                     :element-type 'unsigned-byte
                     :if-exists :supersede)
    (loop for b in img-2bpp do
      (write-byte b s))))

(defun dump-string (fname string)
  (with-open-file (s fname
                     :direction :output
                     :if-exists :supersede)
    (format s string)))

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

(defun tile-map->source (annotated-tmap next-frame)
  (with-output-to-string (stream)
    (let ((unique-tiles (remove-duplicates (mapcar #'cdr annotated-tmap))))
      (loop for tile in unique-tiles do
        (format stream "ld a, ~a~%" tile)
        (loop for (location . __tile) in (remove-if-not (lambda (x) (eql (cdr x) tile)) annotated-tmap) do
          (format stream "ld [$~X], a~%" (wrap-location location))))
      (format stream "ld a, LOW(map~a)~%" next-frame)
      (format stream "ld [jump_next_frame_map+1], a~%")
      (format stream "ld a, HIGH(map~a)~%" next-frame)
      (format stream "ld [jump_next_frame_map+2], a~%")
      (format stream "ret~%"))))

(defun graphics->source (indexes->tiles assignment next-frame)
  (with-output-to-string (stream)
    (loop for (idx . name) in assignment do
      (let ((converted-tile (tile->2bpp (gethash name indexes->tiles))))
        (loop for (b1 b2) on converted-tile by #'cddr
              for i from 0 by 2 do
          (format stream "ld sp, $~X~%" (dpb b2 (byte 8 8) b1))
          (format stream "ld [$~X], sp~%" (+ #x8000 (* idx 16) i)))))

    (format stream "ld a, LOW(gfx~a)~%" next-frame)
    (format stream "ld [jump_next_frame_gfx+1], a~%")
    (format stream "ld a, HIGH(gfx~a)~%" next-frame)
    (format stream "ld [jump_next_frame_gfx+2], a~%")
    (format stream "ld sp, original_sp~%")
    (format stream "pop hl~%")
    (format stream "ld sp, hl~%")
    (format stream "reti~%")))

(defun image->annotated-tilemap (split-img tiles assignment)
  (remove-if #'null
             (loop for tile in split-img
                   for i from 0
                   collect
                   (let* ((tile-name (gethash tile tiles))
                          (tile-idx (car (find tile-name assignment :key #'cdr))))
                     (cons i tile-idx)))
             :key #'cdr))

(defun dbg (f val)
  (funcall f val)
  val)

(defun gif->tiles (filename &key)
  (let* ((frames (coerce (skippy:images (skippy:load-data-stream filename)) 'list))
         (tiles (make-hash-table :test #'gif-data=)) ; map of tile data -> tile name
         (indexes->tiles (make-hash-table)) ; map of tile name -> tile data
         (diff-set nil) ; list of (list of tile names that need to be loaded for a frame)
         (frame-sets nil) ; list of (set of the tile names for each frame)
         (assignments nil) ; list of (list of (idx . name) pairs) for tile map updates
         )

    ;; Build frame-sets
    (let ((tile-name 0))
      (loop for frame in frames do
        (let ((frame-set (make-hash-table)))
          (loop for y below (skippy:height frame) by 8 do
            (loop for x below (skippy:width frame) by 8 do
              (let ((new-tile (skippy:make-image :width 8 :height 8)))
                (skippy:composite frame new-tile :sx x :sy y :dx 0 :dy 0)
                (when (not (gethash new-tile tiles))
                  (setf (gethash new-tile tiles) tile-name)
                  (incf tile-name))
                (setf (gethash (gethash new-tile tiles) frame-set) 0))))
          (push frame-set frame-sets)))
      (setf frame-sets (reverse frame-sets)))

    ;; Build indexes->tiles
    (loop for tile being each hash-key of tiles do
      (let ((idx (gethash tile tiles)))
        (setf (gethash idx indexes->tiles) tile)))

    ;; Build diff-set
    (loop for (before after) on (cons (car (last frame-sets)) frame-sets) do
      (let ((diff nil))
        (when (and before after)
          (loop for idx being each hash-key of after do
            (when (null (gethash idx before))
              (push idx diff)))
          (push diff diff-set))))
    (setf diff-set (reverse diff-set))

    ; An "assignment" is a mapping from tile index -> tile name
    (setf assignments
          (let* ((first-assignment (loop for tname being each hash-key of (car frame-sets)
                                         for i from 0
                                         collect (cons i tname)))
                (current-assignment first-assignment))
            (flet ((make-assignment (diff)
                     (let* ((to-remain (remove-if-not (lambda (a) (find (cdr a) diff)) current-assignment))
                            (need-names (remove-if (lambda (x) (find x to-remain :key #'cadr)) diff))
                            (free-idxs (nshuffle
                                        (loop for i from 0 to 255
                                              when (not (find i to-remain :key #'cadr))
                                                collect i)))
                            (new-assignments (loop for needed-name in need-names
                                                   collect (cons (pop free-idxs) needed-name))))
                       (setf current-assignment (append to-remain new-assignments))
                       new-assignments)))
              (let ((the-rest (loop for diff in (cdr diff-set)
                                       collect (make-assignment diff))))
                (cons (make-assignment (car diff-set)) the-rest)))))

    ;; Dump map initialization code
    (with-open-file (gfx "res/graphics-code"
                                         :direction :output
                                         :if-exists :supersede)
      (with-open-file (map "res/map-code"
                                      :direction :output
                                      :if-exists :supersede)
        (format map "map_initial:~%")
        (format map
                (tile-map->source
                 (image->annotated-tilemap
                  (splitimg (car frames))
                  tiles
                  ;; TODO
                  (loop for tname being each hash-key of (car frame-sets)
                        for i from 0
                        collect (cons i tname)))
                 0))
        (format gfx "gfx_initial:~%")
        (format gfx
                (graphics->source
                 indexes->tiles
                 ;; TODO
                 (loop for tname being each hash-key of (car frame-sets)
                       for i from 0
                       collect (cons i tname))
                 0))

        (loop for frame in frames
              for assignment in assignments
              for i from 0 do
                (format map "map~a:~%" i)
                (format gfx "gfx~a:~%" i)
                (let ((annotated-tm (image->annotated-tilemap (splitimg frame) tiles assignment))
                      (next-frame (mod (1+ i) (length frames))))
                  (format t "~a map updates between frames ~a, ~a~%" (length annotated-tm) i (1+ i))
                  (format map (tile-map->source annotated-tm next-frame))
                  (format gfx (graphics->source indexes->tiles assignment next-frame))))))

    (format t "~%")
    (format t "~a unique tiles~%" (hash-table-count tiles))
    (format t "~%")
    (loop for diff in diff-set
          for i from 0 do
          (format t "~a different tiles between frames ~a, ~a~%" (length diff) i (1+ i)))
    (format t "~%")
    (loop for sp-img in (mapcar #'splitimg frames)
          for i from 0 do
            (let ((ut (length (remove-duplicates sp-img :test #'equalp :key #'skippy:image-data))))
              (when (> ut 256)
                (format t "Frame ~a has too many (~a) unique tiles!" i ut))))

    (cons tiles diff-set)))

(defun pass () nil)
