#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))

(defvar *text* "lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
eiusmod tempor incididunt ut labore et dolore magna aliqua. ut enim ad minim
veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
commodo consequat. duis aute irure dolor in reprehenderit in voluptate velit
esse cillum dolore eu fugiat nulla pariatur. excepteur sint occaecat
cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est
laborum. sed ut perspiciatis unde omnis iste natus error sit voluptatem
accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo
inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. nemo
enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia
consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. neque
porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur,
adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et
dolore magnam aliquam quaerat voluptatem. ut enim ad minima veniam, quis
nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex
ea commodi consequatur? quis autem vel eum iure reprehenderit qui in ea
voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem
eum fugiat quo voluptas nulla pariatur?")


(defun -test-centroids (counts nc ncn)
  (reduce (lambda (x y) (and x y))
          (loop for i from 0 below nc collect
            (multiple-value-bind (val exists)
              (gethash i counts)
              (and exists (>= val ncn))))))


(defun -get-dst (centroids cand)
  (first (sort (loop for c in centroids
                     and i from 0
                     collect (list i (vec:dst cand c)))
            #'< :key #'second)))


(defun -glyph-generate-pts (xy bbox centroids nc ncn)
  (let ((counts (make-hash-table :test #'equal))
        (centroid-pts (make-hash-table :test #'equal)))

    (loop for i from 0 do
      (vec:with-xy (bbox bx by)
        (let ((cand (rnd:in-box bx by :xy xy)))
          (destructuring-bind (c dst)
            (-get-dst centroids cand)
            (multiple-value-bind (val exists)
              (gethash c counts)

              (cond ((and exists (< val ncn))
                  (progn
                    (setf (gethash c centroid-pts) (append (list cand)
                                                           (gethash c centroid-pts)))
                    (incf (gethash c counts))))
                  ((not exists)
                    (progn
                      (setf (gethash c centroid-pts) (list cand))
                      (setf (gethash c counts) 1))))))))
      until (-test-centroids counts nc ncn))

    (let ((pts (loop for i from 0 below nc
                          collect (gethash i centroid-pts))))
      (apply #'append pts))))


(defun make-glyph (bbox nc ncn)
  (vec:with-xy ((vec:scale bbox 0.5d0) bx by)
    (-glyph-generate-pts
      (vec:zero)
      bbox
      (rnd:nin-box nc bx by :xy (vec:zero))
      nc
      ncn)))


(defun get-alphabet (bbox nc ncn)
  (let ((alphabet (make-hash-table :test #'equal)))
    (loop for i from 0 and c across "abcdefghijklmnopqrstuvwxyz.,?" do
      (setf (gethash c alphabet) (make-glyph bbox nc ncn)))
    (setf (gethash " " alphabet) nil)
    alphabet))

(defun do-write (snk alphabet bbox top right bottom left sentence)
  (vec:with-xy (bbox bx by)
    (let ((g (snek:add-grp! snk))
          (cursor (vec:vec left left)))
      (loop for c across sentence do
        (format t "~a" c)
        (aif (gethash c alphabet)
          (snek:add-path! snk (mapcar (lambda (p) (vec:add cursor p))
                                      (gethash c alphabet))
                          :g g)
          (setf g (snek:add-grp! snk)))
        (if (> (vec::vec-x cursor) right)
          (progn
            (setf cursor (vec:vec left (+ (* 2d0 by) (vec::vec-y cursor)) ))
            (setf g (snek:add-grp! snk)))
          (setf cursor (vec:add cursor (vec:vec (* 2d0 bx) 0d0))))
        until (> (vec::vec-y cursor) bottom)))))


(defun main (size fn)
  (let ((left 50d0)
        (top 50d0)
        (bottom 950d0)
        (right 950d0)
        (grains 300)
        (bbox (vec:vec 12d0 25d0))
        (sand (sandpaint:make size
                :active (color:black 0.009)
                :bg (color:white))))

    (let ((alphabet (get-alphabet bbox 2 2))
          (snk (snek:make))
          (state-gen (math:get-state-gen (lambda () (rnd:get-acc-circ-stp*)))))
      (do-write snk alphabet bbox top right bottom left *text*)
      (loop for i from 0 below 500 do
        (snek:with (snk)
          (snek:itr-all-verts (snk v)
            (snek:move-vert? v (rnd:in-circ 0.1d0))
            (snek:move-vert? v (funcall state-gen v 0.00001d0))))
        (snek:itr-grps (snk g)
          (aif (snek:get-grp-as-bzspl snk g)
               (sandpaint:pix sand (bzspl:rndpos it grains))))))

    (sandpaint:pixel-hack sand)
    (sandpaint:save sand fn :gamma 1.5)))

(time (main 1000 (second (cmd-args))))

