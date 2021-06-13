;;;===============================================================================
;;;
;;; Guile compatibility file:
;;;
;;; Uncomment the appropriate LOAD command in macros-core.scm
;;;
;;;===============================================================================

;; A numeric string that uniquely identifies this run in the universe

(define (ex:unique-token)
  (number->string (current-time)))

;; The letrec black hole and corresponding setter.

(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.

(define ex:guid-prefix "&")
(define ex:free-prefix "~")

;; Just give this damn thing a binding

(define assertion-violation
  (lambda args
    (display 'assertion-violation)
    (newline)
    (display args)
    (newline)
    (car #f)))

(define pretty-print write)

;; These are only partial implementations for specific use cases needed.
;; Full implementations should be provided by host implementation.

(define (memp proc ls)
  (cond ((null? ls) #f)
        ((pair? ls) (if (proc (car ls))
                        ls
                        (memp proc (cdr ls))))
        (else (assertion-violation 'memp "Invalid argument" ls))))

(define (for-all proc l . ls)
  (or (null? l)
      (and (apply proc (car l) (map car ls))
           (apply for-all proc (cdr l) (map cdr ls)))))


;; Guile has r6rs procedural records.
(import (rnrs records procedural))
