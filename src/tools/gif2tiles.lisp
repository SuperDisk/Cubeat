(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:skippy) :silent t))

(defparameter *reserved-tiles-count* 8)


(defun gif-data= (i1 i2)
  (equalp (skippy:image-data i1) (skippy:image-data i2)))

(defun sxhash-gif-data (f)
  (sxhash (skippy:image-data f)))

(define-hash-table-test gif-data= sxhash-gif-data)

(defun partition (predicate list)
  (loop for x in list
        if (funcall predicate x) collect x into yes
          else collect x into no
        finally (return (values yes no))))

(defun update-hash (key func default hash)
  (let ((val (gethash key hash default)))
    (setf (gethash key hash) (funcall func val))))

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

(defun splitimg (img)
  (let ((out nil))
    (loop for y below (skippy:height img) by 8 do
      (loop for x below (skippy:width img) by 8 do
        (let ((new-tile (skippy:make-image :width 8 :height 8)))
          (skippy:composite img new-tile :sx x :sy y :dx 0 :dy 0)
          (push new-tile out))))
    (nreverse out)))

(defun generate-playfield-buffer ()
  (flet ((wrap-playfield-location (x y)
           (+ (* y 20) x)))
    (let (idx->buffer-offset)
      (values
       (with-output-to-string (out)
         (let ((current-row-start-pos 0)
               (bytes-generated 0)
               (total-cycles 0)
               (cycle-counter 0))
           (flet ((inc-cycles (amount)
                    (incf total-cycles amount)
                    (when (> total-cycles 1090) (incf cycle-counter amount))
                    (when (> cycle-counter 42)
                      (format out "ld e, a~%")
                      (format out "xor a~%")
                      (format out "ldh [rIF], a~%")
                      (format out "halt~%")
                      (format out "ld a, e~%")
                      (incf bytes-generated 6)
                      (setf cycle-counter 1))))
             (loop for y from 0 below 18 do
               (loop for x from 0 below (1- 20) do
                 (push (cons (wrap-playfield-location x y) (1+ bytes-generated))
                       idx->buffer-offset)
                 (format out "ld a, 0~%")
                 (inc-cycles 2)
                 (format out "ld [hl+], a~%")
                 (inc-cycles 2)
                 (incf bytes-generated 3))

               (push (cons (wrap-playfield-location 19 y) (1+ bytes-generated))
                     idx->buffer-offset)
               (format out "ld [hl], 0~%")
               (inc-cycles 3)
               (incf bytes-generated 2)

               (incf current-row-start-pos #x20)
               (let ((new-l (ldb (byte 8 0) current-row-start-pos))) ; lowermost 8 bits
                 (format out "ld l, ~a~%" new-l)
                 (inc-cycles 2)
                 (incf bytes-generated 2)
                 (when (zerop new-l)
                   (format out "inc h~%")
                   (inc-cycles 1)
                   (incf bytes-generated)))))))
       (reverse idx->buffer-offset)))))

(defparameter *idx->playfield-buffer-offset*
  (nth-value 1 (generate-playfield-buffer)))

(defun generate-playfield-update-code (mapping)
  (flet ((wrap-playfield-location (x y)
           (+ (* (+ 6 y) 20) 1 x)))
    (with-output-to-string (out)
      (format out "ld b, %10000011~%")
      (loop for y from 0 below 11 do
        (loop for x from 0 below 18
              for offs = (cdr (assoc (wrap-playfield-location x y) mapping)) do
                (format out "ld a, [hl+]~%")
                (format out "and a, b~%")
                (format out "jr z, @+5~%")
                (format out "ld [playfield_buffer+~a], a~%" offs))))))

(defun tmap->source (annotated-tmap next-frame prefix)
  (with-output-to-string (stream)
    (let (old-loc)
      (format stream "ld de, 3~%")
      (loop for (location . tile) in annotated-tmap
            for loc = (cdr (assoc location *idx->playfield-buffer-offset*)) do
              (if (and old-loc (= (- loc old-loc) 3))
                  (format stream "add hl, de~%")
                  (format stream "ld hl, playfield_buffer + $~X~%" loc))
              (format stream "ld [hl], ~a~%" (logxor #b10000000 tile))
              (setf old-loc loc)))

    (format stream "ld a, LOW(~a_map~a)~%" prefix next-frame)
    (format stream "ld [update_playfield_buffer+1], a~%")
    (format stream "ld a, HIGH(~a_map~a)~%" prefix next-frame)
    (format stream "ld [update_playfield_buffer+2], a~%")

    (format stream "ld a, BANK(~a_map~a)~%" prefix next-frame)
    (format stream "ld [next_map_bank], a~%")

    (format stream "ret~%")))

(defun gfx->source (annotated-tmap indexes->tiles assignment next-frame &key (include-halts t) (prefix ""))
  (with-output-to-string (stream)
    (let ((total-cycles 0)
          (cycle-counter 0)
          (scanlines 0)
          (done-dma nil))
      (flet ((inc-cycles (amount)
               (when include-halts
                 (incf total-cycles amount)
                 (when (> total-cycles 1075) (incf cycle-counter amount))
                 (when (> cycle-counter 42)
                   (format stream "xor a~%")
                   (format stream "ldh [rIF], a~%")
                   (format stream "halt~%")
                   (setf cycle-counter 0)
                   (incf scanlines)
                   (when (and (= scanlines 31)
                              (not done-dma))
                     (format stream "ld a, [rLCDC]~%")
                     (format stream "set 2, a~%")
                     (format stream "ld [rLCDC], a~%")
                     (format stream "ld a, HIGH(wShadowOAM2)~%")
                     (format stream "ld hl, sp+0~%")
                     (format stream "ld sp, hTempStack~%")
                     (format stream "call hOAMDMA~%")
                     (format stream "ld sp, hl~%")
                     (format stream "xor a~%")
                     (format stream "ldh [rIF], a~%")
                     (format stream "halt~%")
                     (setf done-dma t))))))
        (let ((top-tilemap (remove-if-not (lambda (loc) (<= (car loc) 150)) annotated-tmap)))
          (multiple-value-bind (top-assignment bottom-assignment)
              (partition (lambda (a) (find (car a) top-tilemap :key #'cdr)) assignment)

            ;; top graphics
            (let ((all-writes (make-hash-table)))
              (loop for (idx . name) in top-assignment do
                (let ((converted-tile (tile->2bpp (gethash name indexes->tiles))))
                  (loop for (b1 b2) on converted-tile by #'cddr
                        for i from 0 by 2 do
                          (update-hash (dpb b2 (byte 8 8) b1)
                                       (lambda (x) (cons (+ #x8800 (* idx 16) i) x)) nil all-writes))))
              (loop for data being each hash-key of all-writes do
                (format stream "ld sp, $~X~%" data)
                (inc-cycles 3)
                (loop for address in (gethash data all-writes) do
                  (format stream "ld [$~X], sp~%" address)
                  (inc-cycles 5))))

            ;; bottom graphics
            (let ((all-writes (make-hash-table)))
              (loop for (idx . name) in bottom-assignment do
                (let ((converted-tile (tile->2bpp (gethash name indexes->tiles))))
                  (loop for (b1 b2) on converted-tile by #'cddr
                        for i from 0 by 2 do
                          (update-hash (dpb b2 (byte 8 8) b1)
                                       (lambda (x) (cons (+ #x8800 (* idx 16) i) x)) nil all-writes))))
              (loop for data being each hash-key of all-writes do
                (format stream "ld sp, $~X~%" data)
                (inc-cycles 3)
                (loop for address in (gethash data all-writes) do
                  (format stream "ld [$~X], sp~%" address)
                  (inc-cycles 5)))))))

      (when (and include-halts (not done-dma))
        (format stream ".wait_for_oam_scanline~%")
        (format stream "ld a, [rLY]~%")
        (format stream "cp 30~%")
        (format stream "jr nz, .wait_for_oam_scanline~%")
        ;; (format stream "rept 20~% nop~%endr~%")
        (format stream ".wait_for_oam_scanline_hblank~%")
        (format stream "ld a, [rSTAT]~%")
        (format stream "and %0000011~%")
        (format stream "jr nz, .wait_for_oam_scanline_hblank~%")
        (format stream "ld a, [rLCDC]~%")
        (format stream "set 2, a~%")
        (format stream "ld [rLCDC], a~%")
        (format stream "ld a, HIGH(wShadowOAM2)~%")
        (format stream "ld sp, hTempStack~%")
        (format stream "call hOAMDMA~%"))

      (format stream "ld a, LOW(~a_gfx~a)~%" prefix next-frame)
      (format stream "ld [ptr_next_update_bg], a~%")
      (format stream "ld a, HIGH(~a_gfx~a)~%" prefix next-frame)
      (format stream "ld [ptr_next_update_bg+1], a~%")

      (format stream "ld a, BANK(~a_gfx~a)~%" prefix next-frame)
      (format stream "ld [next_gfx_bank], a~%")

      (format stream "jp update_bg_done~%"))))

(defun dbg (f val)
  (funcall f val)
  val)

(defparameter *shite*
  (elt (skippy:images (skippy:load-data-stream "HUD_strip.gif")) 0))

(defun fuckup (frame)
  (let* ((d1 (skippy:image-data frame))
         (d2 (skippy:image-data *shite*))
         (d-out (loop for q across d1
                      for w across d2
                      collect (if (= w 2) q w)))
         (newimg (skippy:make-image :width (skippy:width *shite*)
                                    :height (skippy:height *shite*)
                                    :image-data (skippy:make-image-data (skippy:width *shite*) (skippy:height *shite*) :initial-contents d-out))))
    (skippy:composite newimg frame :sx 0 :sy 0 :dx 0 :dy 0)))

(defun gif->tiles (filename out-filename)
  (let* ((frames (coerce (skippy:images (skippy:load-data-stream filename)) 'list))
         (_ (loop for frame in frames do (fuckup frame)))
         (split-frames (mapcar #'splitimg frames))
         (tiles (make-hash-table :test #'gif-data=)) ; map of tile data -> tile name
         (indexes->tiles (make-hash-table)) ; map of tile name -> tile data
         frame-sets ; list of (set of the tile names for each frame)
         assignments ; list of (list of (idx . name) pairs) for tile map updates
         assignment-diffs ; pared down assignment list which just includes the updates
         tilemaps ; list of (list of (loc . idx) representing a tile map)
         )

    ;; Build tiles and frame-sets
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
      (let ((name (gethash tile tiles)))
        (setf (gethash name indexes->tiles) tile)))

                                        ; An "assignment" is a mapping from tile index -> tile name
    (setf assignments
          (let* ((max-tiles (min (1- (hash-table-count tiles)) 255))
                 (initial-part (loop for tname being each hash-key of (car frame-sets)
                                     for i from *reserved-tiles-count*
                                     collect (cons i tname)))
                 (first-assignment (loop for i from *reserved-tiles-count* to max-tiles
                                         collect (or (assoc i initial-part) (cons i i))))
                 (current-assignment first-assignment)
                 (free-idxs (loop for i from (length initial-part) to max-tiles collect i)))
            (flet ((assignment-contains (name assignment)
                     (find name assignment :key #'cdr))
                   (frame-set-contains (name frame-set)
                     (gethash name frame-set))
                   (is-active (idx)
                     (not (find idx free-idxs))))
              (cons first-assignment
                    (loop for frame-set in (cdr frame-sets)
                          collect
                          (let ((freed-idxs (loop for (idx . name) in current-assignment
                                                  when (not (frame-set-contains name frame-set))
                                                    collect idx)))
                            (setf free-idxs (remove-duplicates (append free-idxs freed-idxs)))
                            (let ((needed-names
                                    (loop for name being each hash-key of frame-set
                                          for pair = (assignment-contains name current-assignment)
                                          for pair-is-active = (and pair (is-active (car pair)))
                                          when (and pair (not pair-is-active)) do
                                            (setf free-idxs (remove (car pair) free-idxs)) ; activate it
                                          when (not pair) collect name))
                                  (new-assignment (copy-alist current-assignment)))
                              (loop for name in needed-names do
                                (rplacd (assoc (pop free-idxs) new-assignment) name))
                              (setf current-assignment new-assignment)
                              new-assignment)))))))

    (setf assignment-diffs
          (loop for (before after) on (cons (car (last assignments)) assignments)
                when (and before after)
                  collect
                  (loop for (i1 . n1) in before
                        for (i2 . n2) in after
                        when (not (equalp i1 i2)) do (format t "Critical error~%")
                          when (not (equalp n1 n2)) collect (cons i2 n2))))

    (setf tilemaps
          (loop for split-img in split-frames
                for assignment in assignments
                collect
                (loop for tile in split-img
                      for name = (gethash tile tiles)
                      for index = (car (find name assignment :key #'cdr))
                      for loc from 0
                      collect (cons loc index))))

    ;; Dump map initialization code
    (with-open-file (out out-filename
                         :direction :output
                         :if-exists :supersede)
      (let ((prefix (pathname-name out-filename)))
        (format out "SECTION \"~a\", ROMX~%" (gensym prefix))
        (format out "~a_gfx_init:~%" prefix)
        (format out (gfx->source (car tilemaps)
                                 indexes->tiles
                                 (car assignments)
                                 0
                                 :include-halts nil
                                 :prefix prefix))

        (loop for frame in frames
              for assignment-diff in assignment-diffs
              for tmap in tilemaps
              for i from 0 do
                (let ((next-frame (mod (1+ i) (length frames))))
                  (format out "SECTION \"~a\", ROMX~%" (gensym prefix))
                  (format out "~a_gfx~a:~%" prefix i)
                  (format out (gfx->source tmap indexes->tiles assignment-diff next-frame :prefix prefix))
                  (format t "~a gfx updates between frames ~a, ~a~%" (length assignment-diff) i (1+ i))

                  (format out "SECTION \"~a\", ROMX~%" (gensym prefix))
                  (format out "~a_map~a:~%" prefix i)
                  (format out (tmap->source tmap next-frame prefix))))))

    (format t "~%")
    (format t "~a unique tiles~%" (hash-table-count tiles))
    (format t "~%")
    (loop for sp-img in split-frames
          for i from 0 do
            (let ((ut (length (remove-duplicates sp-img :test #'equalp :key #'skippy:image-data))))
              (when (> ut 256)
                (format t "Frame ~a has too many (~a) unique tiles!" i ut))))))

(defun main ()
  (apply #'gif->tiles (cdr sb-ext:*posix-argv*))
  (exit))
