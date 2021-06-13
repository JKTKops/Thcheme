;; build/bootstrap.scm
;;
;; This file is intended to be run by a boostrapping Scheme compiler, to
;; expand Thcheme's expander before starting up the REPL.
;; Thcheme can expand the expander itself, but it takes a _lot_ longer.
;;
;; $ cd build
;; $ guile -s bootstrap.scm
;; $ cd ..
;;
;; This should run in a few seconds, and it will be faster if Guile doesn't
;; have to recompile some of the scm files involved.
;;
;; It's currently set up for Guile, but it's easy to write compat files
;; for other Scheme compilers too. See more info in SRFI 72.

(load "compat-guile.scm")
(load "../lib/expander/runtime.scm")
(load "expander.scm")
(ex:expand-file "standard-libraries.scm" "../lib/expander/standard-libraries.exp")
(ex:expand-r5rs-file
  "expander.scm"
  "../lib/expander/expander.exp"
  (ex:environment '(scheme base)))
