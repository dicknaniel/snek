
(load "../utils/text")
(load "../utils/lorem-common")



(defun estimate-nc (bbox-fxn)
  (let* ((samples (funcall bbox-fxn 1000))
         (mid (math:mid samples))
         (ma (apply #'max (mapcar (lambda (a) (vec:dst a mid)) samples))))
    (if (< ma 30d0) 4 7)))


(defun make-glyph (bbox-fxn ncn)
  (let* ((nc (estimate-nc bbox-fxn))
         (centroids (get-centroids bbox-fxn 0d0 nc)))

    (lambda ()
      (let ((counts (make-hash-table :test #'equal))
            (centroid-pts (make-hash-table :test #'equal)) )
        (loop for i from 0 do
          (let ((cand (first (funcall bbox-fxn 1))))
            (destructuring-bind (c dst)
              (-get-dst centroids cand)
              (multiple-value-bind (val exists)
                (gethash c counts)

                (cond ((and exists (< val ncn))
                       (setf (gethash c centroid-pts)
                             (append (list cand) (gethash c centroid-pts)))
                       (incf (gethash c counts)))
                      ((not exists)
                       (setf (gethash c centroid-pts) (list cand)
                             (gethash c counts) 1))))))
                      ;else: exists and has too many pts
          until (-test-centroids counts nc ncn))

        (apply #'append (loop for i from 0 below nc
                            collect (gethash i centroid-pts)))))))
      ;)))


(defun get-alphabet (get-bbox-fxn ncn)
  (let ((alphabet (make-hash-table :test #'equal)))
    (loop for i from 0 and c across "abcdefghijklmnopqrstuvwxyz.,?-'" do
          (format t "~a ~a ~%" c
                  (setf (gethash c alphabet)
                        (make-glyph (funcall get-bbox-fxn) ncn))))
    alphabet))


(defun do-write (snk alphabet bbox top right bottom left sentence &key (tweak-fxn))
  (vec:with-xy ((vec:scale bbox 2d0) bx by)
    (let ((g nil)
          (cursor (vec:vec left top)))

      (block outer
        (loop for (word wl) in (get-words sentence) do
          (if (> (+ (vec::vec-x cursor) (* (+ 1 wl) bx)) right)
            (progn
              (format t "~%")
              (setf cursor (vec:vec left (+ by (vec::vec-y cursor))))))

          (if (> (vec::vec-y cursor) bottom) (return-from outer t))

          (setf g (snek:add-grp! snk))

          (loop for c across word do
            (format t "~a" c)
            (aif (gethash c alphabet)
              (snek:add-path! snk (math:vadd (funcall it) cursor) :g g))
            (setf cursor (vec:add cursor (vec:vec bx 0d0))))
          (setf cursor (vec:add cursor (vec:vec bx 0d0)))
          (format t " "))))))

