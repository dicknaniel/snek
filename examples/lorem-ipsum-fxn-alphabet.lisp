#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/lorem-fxn")
(load "../utils/state")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))

; excerpt from oryx and crake by margaret atwood
(defvar *text* "snowman wakes before dawn. he lies unmoving, listening to the tide coming in, wave after wave sloshing over the various barricades, wish-wash, wish-wash, the rhythm of heartbeat. he would so like to believe he is still asleep. on the eastern horizon there's a greyish haze, lit now with a rosy, deadly glow. strange how that colour still seems tender. the offshore towers stand out in dark silhouette against it, rising improbably out of the pink and pale blue of the lagoon. the shrieks of the birds that nest out there and the distant ocean grinding against the ersatz reefs of rusted car parts and jumbled bricks and assorted rubble sound almost like holiday traffic.")


(defun main (size fn)
  (let ((left 55d0)
        (top 70d0)
        (bottom 950d0)
        (right 950d0)
        (grains 20)
        (bbox (vec:vec 14d0 30d0))
        (ncn 1)
        (spacebox (vec:vec 14d0 30d0))
        (snk (snek:make))
        (sand (sandpaint:make size
                :active (color:black 0.009)
                :bg (color:white))))

    (labels ((get-bbox-fxn ()
      (let ((bbox  (if (< (rnd:rnd) 0.2)
                     (vec:mult bbox (vec:vec 1d0 4d0))
                     bbox)))
        (vec:with-xy ((vec:scale bbox 0.5d0) bx by)
          (lambda (n)
            (mapcar (lambda (v) (vec:rot v (* PI 0.2d0)))
                    (rnd:nin-box n bx by)))))))

      (let ((alphabet (get-alphabet #'get-bbox-fxn ncn))
            (state-gen (get-state-gen (lambda () (rnd:get-acc-circ-stp*)))))
        (do-write snk alphabet spacebox top right bottom left *text*)
        (loop for i from 0 below 500 do
          (snek:with (snk)
            (snek:itr-all-verts (snk v)
              (snek:move-vert? v (rnd:in-circ 0.25d0))
              (snek:move-vert? v (funcall state-gen v 0.000008d0))))
          (snek:itr-grps (snk g)
            (aif (snek:get-grp-as-bzspl snk g)
                 (sandpaint:pix sand (bzspl:rndpos it
                                       (* (rnd:rndi (- grains 5) (+ grains 5))
                                          (bzspl::bzspl-n it)))))))))

  (sandpaint:pixel-hack sand)
  (sandpaint:save sand fn :gamma 1.5)))


(time (main 1000 (second (cmd-args))))

