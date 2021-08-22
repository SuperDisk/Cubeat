(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:skippy) :silent t))

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

(defun wrap-location (loc)
  (let ((remainder (mod loc 20))
        (row (floor loc 20)))
    (+ #x9800 (* row 32) remainder)))

(defun generate-playfield-buffer ()
  (flet ((wrap-playfield-location (x y)
           (+ (* (+ 4 y) 18) x 1)))
    (let (idx->buffer-offset)
      (values
       (with-output-to-string (out)
         (let ((current-row-start-pos #x9881)
               (bytes-generated 0))
           (loop for y from 0 to 12 do
             (loop for x from 0 to (1- 17) do
               (push (cons (wrap-playfield-location x y) (1+ bytes-generated))
                     idx->buffer-offset)
               (format out "ld a, 0~%")
               (format out "ld [hl+], a~%")
               (incf bytes-generated 3))

             (push (cons (wrap-playfield-location 17 y) (1+ bytes-generated))
                   idx->buffer-offset)
             (format out "ld [hl], 0~%")
             (incf bytes-generated 2)

             (incf current-row-start-pos #x20)
             (let ((new-l (ldb (byte 8 0) current-row-start-pos)))
               (format out "ld l, ~a~%" new-l)
               (incf bytes-generated 2)
               (when (= new-l #x01)
                 (format out "inc h~%")
                 (incf bytes-generated 1))))))
       (reverse idx->buffer-offset)))))

(defparameter *idx->playfield-buffer-offset*
  (nth-value 1 (generate-playfield-buffer)))

(defun playfield-idx-p (idx)
  (let ((x (mod idx 20))
        (y (floor idx 18)))
    (and (>= x 1) (<= x 17)
         (>= y 4) (<= y 16))))

(defun gameplay-frame->source (non-playfield-tilemap parity next-frame prefix &key (initial nil))
  (with-output-to-string (stream)
    (when (not initial)
      (format stream "call playfield_buffer~%"))
    (loop for (location . tile) in non-playfield-tilemap do
      (format stream "ld a, ~a~%" (logxor #b10000000 tile))
      (format stream "ld [$~X], a~%" (+ (if (equalp parity 'even) 0 #x400)
                                        (wrap-location location))))

    (when (not initial)
      (format stream "ld a, LOW(~a_render~a)~%" prefix next-frame)
      (format stream "ld [ptr_next_update_bg], a~%")
      (format stream "ld a, HIGH(~a_render~a)~%" prefix next-frame)
      (format stream "ld [ptr_next_update_bg+1], a~%")

      (format stream "ld a, BANK(~a_render~a)~%" prefix next-frame)
      (format stream "ld [next_frame_bank], a~%")

      (format stream "jp update_bg_done~%"))))

(defun animation-frame->source (next-playfield-tilemap indexes->tiles assignment next-frame prefix &key (initial nil))
  (with-output-to-string (stream)
    (let ((total-cycles 0)
          (cycle-counter 0))
      (flet ((inc-cycles (amount)
               (when (not initial)
                 (incf total-cycles amount)
                 (when (> total-cycles 1090) (incf cycle-counter amount))
                 (when (> cycle-counter 42)
                   (format stream "xor a~%")
                   (format stream "ldh [rIF], a~%")
                   (format stream "halt~%")
                   (setf cycle-counter 0)))))
        ;; Swap BGs
        (when (not initial)
          (format stream "ld a, [rLCDC]~%")
          (format stream "xor %00001000~%")
          (format stream "ld [rLCDC], a~%")
          (format stream "ld [hLCDC], a~%")
          (inc-cycles 9))

        ;; Graphics
        (let ((all-writes (make-hash-table)))
          (loop for (idx . name) in assignment do
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

        ;; Buffer
        (loop for (location . tile) in next-playfield-tilemap
              for loc = (cdr (assoc location *idx->playfield-buffer-offset*))
              when loc do ; TODO
                (format stream "ld a, ~a~%" (logxor #b10000000 tile))
                (format stream "ld [playfield_buffer + $~X], a~%" loc))))

    (format stream "ld a, LOW(~a_gameplay~a)~%" prefix next-frame)
    (format stream "ld [ptr_next_update_bg], a~%")
    (format stream "ld a, HIGH(~a_gameplay~a)~%" prefix next-frame)
    (format stream "ld [ptr_next_update_bg+1], a~%")

    (format stream "ld a, BANK(~a_gameplay~a)~%" prefix next-frame)
    (format stream "ld [next_frame_bank], a~%")

    (format stream "jp update_bg_done~%")))

(defun dbg (f val)
  (funcall f val)
  val)

(defun gif->tiles (filename out-filename)
  (let* ((frames (coerce (skippy:images (skippy:load-data-stream filename)) 'list))
         (split-frames (mapcar #'splitimg frames))
         (tiles (make-hash-table :test #'gif-data=)) ; map of tile data -> tile name
         (indexes->tiles (make-hash-table)) ; map of tile name -> tile data
         frame-sets ; list of (set of the tile names for each frame)
         assignments ; list of (list of (idx . name) pairs) for tile map updates
         assignment-diffs ; pared down assignment list which just includes the updates
         tilemaps ; list of (list of (loc . idx) representing a tile map)
         tilemap-diffs-even ; pared down tile map list whcih just includes the updates (even frames)
         tilemap-diffs-odd ; pared down tile map list whcih just includes the updates (odd frames)
         tilemap-diffs-interspersed ; the full list of tilemap diffs
         tilemap-diffs
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

    ;; An "assignment" is a mapping from tile index -> tile name
    (setf assignments
          (let* ((max-tiles (min (1- (hash-table-count tiles)) 255))
                 (initial-part (loop for tname being each hash-key of (car frame-sets)
                                     for i from 6 ; reserved tiles
                                     collect (cons i tname)))
                 (first-assignment (loop for i from 4 to max-tiles
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


    (setf tilemap-diffs
          (loop for (before after) on (cons (car (last tilemaps)) tilemaps)
                when (and before after)
                  collect
                  (loop for (l1 . i1) in before
                        for (l2 . i2) in after
                        when (not (equalp l1 l2)) do (format t "Critical error~%")
                          when (not (equalp i1 i2)) collect (cons l2 i2))))

    (setf tilemap-diffs-even
          (let ((even-tilemaps (loop for tm in tilemaps by #'cddr collect tm)))
            (loop for (before after) on (cons (car (last even-tilemaps)) even-tilemaps)
                  when (and before after)
                    collect
                    (loop for (l1 . i1) in before
                          for (l2 . i2) in after
                          when (not (equalp l1 l2)) do (format t "Critical error~%")
                            when (not (equalp i1 i2)) collect (cons l2 i2)))))

    (setf tilemap-diffs-odd
          (let ((odd-tilemaps (loop for tm in (cdr tilemaps) by #'cddr collect tm)))
            (loop for (before after) on (cons (car (last odd-tilemaps)) odd-tilemaps)
                  when (and before after)
                    collect
                    (loop for (l1 . i1) in before
                          for (l2 . i2) in after
                          when (not (equalp l1 l2)) do (format t "Critical error~%")
                            when (not (equalp i1 i2)) collect (cons l2 i2)))))

    (setf tilemap-diffs-interspersed
          (loop for even in tilemap-diffs-even
                for odd in tilemap-diffs-odd
                append (list even odd)))

    ;; Dump map initialization code
    (with-open-file (out out-filename
                         :direction :output
                         :if-exists :supersede)
      (let ((prefix (pathname-name out-filename)))
        (format out "SECTION \"~a\", ROMX~%" (gensym prefix))

        (format out "~a_init_even:~%" prefix)
        (format out (gameplay-frame->source (car tilemaps) 'even 1 prefix :initial t))

        (format out "~a_init_odd:~%" prefix)
        (format out (gameplay-frame->source (cadr tilemaps) 'odd 1 prefix :initial t))

        (format out "~a_init_gfx:~%" prefix)
        (let ((second-frame-playfield-tilemap (remove-if-not #'playfield-idx-p (cadr tilemaps) :key #'car)))
          (format out (animation-frame->source second-frame-playfield-tilemap
                                               indexes->tiles
                                               (car assignments)
                                               1
                                               prefix
                                               :initial t)))

        (loop for frame in frames
              for assignment-diff in assignment-diffs
              for tilemap-diff in tilemap-diffs-interspersed
              for fine-tilemap-diff in tilemap-diffs
              for i from 0
              with parity = 'even do
                (let ((next-frame (mod (1+ i) (length frames)))
                      (non-playfield-tilemap-diff (remove-if #'playfield-idx-p tilemap-diff :key #'car))
                      (playfield-next-tilemap-diff (remove-if-not #'playfield-idx-p fine-tilemap-diff :key #'car)))
                  (format out "SECTION \"~a\", ROMX~%" (gensym prefix))

                  (format out "~a_render~a:~%" prefix i)
                  (format out (animation-frame->source playfield-next-tilemap-diff
                                                       indexes->tiles
                                                       assignment-diff
                                                       next-frame
                                                       prefix))
                  (format out "~a_gameplay~a:~%" prefix i)
                  (format out (gameplay-frame->source non-playfield-tilemap-diff parity next-frame prefix))

                  ;; (format t "~a map updates between frames ~a, ~a~%" (length tilemap-diff) i (1+ i))
                  ;; (format t "~a gfx updates between frames ~a, ~a~%" (length assignment-diff) i (1+ i))

                  (if (equalp parity 'even)
                      (setf parity 'odd)
                      (setf parity 'even))))))

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
