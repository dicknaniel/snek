
(proclaim '(optimize speed))
(proclaim '(optimize (space 0)))
(proclaim '(inline last1 single append1 conc1 mklist))
;(declaim '(optimize (debug 1)))

; TODO: restructure this while file more once i know it works.

(load "~/quicklisp/setup.lisp")

(ql:quickload "zpng")


(asdf:defsystem "snek"
  :description "SNEK - A Generative System for Writing Generative Systems"
  :version "1.4.5"
  :author "inconvergent"
  :licence "MIT"
  :serial t
  :depends-on ("zpng")
  :components ((:file "pg-utils")
               (:file "utils")
               (:file "utils-time")
               (:file "math")
               (:file "rnd")
               (:file "color")
               (:file "linear-path")
               (:file "bzspline")
               (:file "sandpaint")
               (:file "plot")
               (:file "zmap")
               (:file "snek-macros")
               (:file "snek")
               (:file "snek-utils")
               (:file "snek-alterations")
               (:file "snek-alterations-mutate")
               (:file "snek-misc")))

(asdf:load-system "snek")

