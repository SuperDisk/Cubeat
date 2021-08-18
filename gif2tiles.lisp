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

(defun anim->source (annotated-tmap indexes->tiles assignment next-frame &key (include-halts t))
  (with-output-to-string (stream)
    (let ((total-cycles 0)
          (cycle-counter 0)
          (current-a-value 0))
      (flet ((inc-cycles (amount &key (using-a nil))
               (when include-halts
                 (incf total-cycles amount)
                 (when (> total-cycles 1090) (incf cycle-counter amount))
                 (when (> cycle-counter 42)
                   (format stream "xor a~%")
                   (format stream "ldh [rIF], a~%")
                   (format stream "halt~%")
                   (if using-a (format stream "ld a, ~a~%" current-a-value))
                   (setf cycle-counter 0)))))
        (multiple-value-bind (top-tilemap bottom-tilemap)
            (partition (lambda (loc) (<= (car loc) 150)) annotated-tmap)
          (multiple-value-bind (top-assignment bottom-assignment)
              (partition (lambda (a) (find (car a) top-tilemap :key #'cdr)) assignment)

            ;; Top tilemap
            (loop for (location . tile) in top-tilemap do
              (setf current-a-value (logxor #b10000000 tile))
              (format stream "ld a, ~a~%" current-a-value)
              (inc-cycles 2 :using-a t)
              (format stream "ld [$~X], a~%" (wrap-location location))
              (inc-cycles 4 :using-a t))

            ;; Top Graphics
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

            ;; Bottom tilemap
            (loop for (location . tile) in bottom-tilemap do
              (setf current-a-value (logxor #b10000000 tile))
              (format stream "ld a, ~a~%" current-a-value)
              (inc-cycles 2 :using-a t)
              (format stream "ld [$~X], a~%" (wrap-location location))
              (inc-cycles 4 :using-a t))


            ;; Bottom Graphics
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
                  (inc-cycles 5))))))))
    (format stream "ld a, LOW(frame~a)~%" next-frame)
    (format stream "ld [ptr_next_update_bg], a~%")
    (format stream "ld a, HIGH(frame~a)~%" next-frame)
    (format stream "ld [ptr_next_update_bg+1], a~%")

    (format stream "ld a, BANK(frame~a)~%" next-frame)
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
         tilemap-diffs ; pared down tile map list whcih just includes the updates
         (name-commonness (make-hash-table))
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

    ;; Collect the "commonness" of each name
    (loop for split-img in split-frames do
      (loop for tile in split-img do
        (update-hash (gethash tile tiles) #'1+ 0 name-commonness)))
    ;; (break)

    ; An "assignment" is a mapping from tile index -> tile name
    (setf assignments
          (let* ((max-tiles (min (1- (hash-table-count tiles)) 255))
                 (initial-part (loop for tname being each hash-key of (car frame-sets)
                                     for i from 4 ; reserve 4 tiles
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
                            (setf free-idxs
                                  (remove-duplicates (append free-idxs freed-idxs))
                                  #+nil(sort (remove-duplicates (append free-idxs freed-idxs))
                                        (lambda (i1 i2)
                                          (let ((n1 (cdr (assoc i1 current-assignment)))
                                                (n2 (cdr (assoc i2 current-assignment))))
                                            (> n1 n2)))))
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

    ;; Dump map initialization code
    (with-open-file (out out-filename
                         :direction :output
                         :if-exists :supersede)

      (format out "SECTION \"~a\", ROMX~%" (gensym "ANIMATION"))
      (format out "frame_initial:~%")
      (format out (anim->source (car tilemaps) indexes->tiles (car assignments) 0 :include-halts nil))

      (loop for frame in frames
            for assignment-diff in assignment-diffs
            for tilemap-diff in tilemap-diffs
            for i from 0 do
              (format out "SECTION \"~a\", ROMX~%" (gensym "ANIMATION"))
              (format out "frame~a:~%" i)
              (let ((next-frame (mod (1+ i) (length frames))))
                (format t "~a map updates between frames ~a, ~a~%" (length tilemap-diff) i (1+ i))
                (format t "~a gfx updates between frames ~a, ~a~%" (length assignment-diff) i (1+ i))
                (format out (anim->source tilemap-diff indexes->tiles assignment-diff next-frame)))))

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
  0)
