;;;===============================================================================
;;;
;;; Thcheme compatibility file:
;;;
;;; Uncomment the appropriate LOAD command in macros-core.scm
;;;
;;;===============================================================================

;; A numeric string that uniquely identifies this run in the universe

(define ex:unique-token
  (let ([tick 0])
    (lambda ()
      (define r tick)
      (set! tick (+ tick 1))
      (number->string r)))) ; only suitable for single-repl-session use

;; The letrec black hole and corresponding setter.

(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.

(define ex:guid-prefix "&")
(define ex:free-prefix "~")

(define display write)

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

(define for-all all)

;; The best we can do in r5rs is make these no-ops

(define (file-exists? fn)
  #f)

(define (delete-file fn)
  (values))

;; Only the most minimal extremely partial implementation
;; of r6rs records as needed for our specific use cases.
;; Note that most arguments are ignored.

(define make-record-type-descriptor        records:make-record-type-descriptor)
(define make-record-constructor-descriptor records:make-record-constructor-descriptor)
(define record-accessor                    records:record-accessor)
(define record-constructor                 records:record-constructor)
(define record-predicate                   records:record-predicate)
