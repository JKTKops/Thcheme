;;; CONSTANTS
(define nil ())
(define call/cc call-with-current-continuation)

;;; LIST FUNCTIONS
;;; not currently organized...
;;;
;;; None of the functions that accept multiple lists are standard,
;;; because they should terminate when the shortest one runs out
;;; but these terminate when the first one runs out.
(define first car)
(define second cadr)
(define third caddr)

(define (length xs)
  (define (go acc xs)
    (if (null? xs)
        acc
        (go (+ 1 acc) (cdr xs))))
  (go 0 xs))

(define (reverse xs)
  (define (loop acc xs)
    (if (null? xs)
        acc
        (loop (cons (car xs) acc)
              (cdr xs))))
  (loop '() xs))

#| Tail Recursion Modulo Cons

Functions here are hand-optimized using a technique called
"tail recursion modulo cons optimization."

The idea is that if a function is tail-recursive except that the recursive
call is the second argument to a call to cons (in tail position), we notice
that prepending to an "accumulator" on exit from a function is the same as
_appending_ to an accumulator on _entry_ to the function. Implementing this
requires mutability:

(define (inefficient x)
  (if stopping-condition
      '()
      (cons x (inefficient (- x 1)))))

becomes

(define (efficient x)
  (define head (list 1))
  (define end  head)
  (define (loop x)
    (if stopping-condition
        (cdr head)
        (begin (set-cdr! end (list x))
               (set! end (cdr end))
               (loop (- x 1)))))
  (loop x))

We maintain pointers to the start of the list and the end of the list.
We use the pointer to the end to append values before recursing.
We use the pointer to the start to return the list at the end.
Note that in this form, 'efficient' is tail recursive.

There are other ways to do the same optimization. Here, we avoid putting
(not stopping-condition) in the if.

|#

; map1 is plenty good as a primitive map1 function.
(define (map1 f xs)
  (define head (list 1))
  (define end  head)
  (define (loop xs)
    (if (null? xs)
        (cdr head)
        (begin (set-cdr! end (list (f (car xs))))
               (set! end (cdr end))
               (loop (cdr xs)))))
  (loop xs))

; This map implementation is not a good primitive map implementation,
; because it stops only when the _first_ list runs out.
; It is needed by the expander and the relaxed condition gives a
; performance boost.
(define (map f xs . xss)
  (define head (list 1))
  (define end  head)
  (define (loop xs xss)
    (if (null? xs)
        (cdr head)
        (begin (set-cdr! end (list (apply f
                                          (car xs)
                                          (map1 car xss))))
               (set! end (cdr end))
               (loop (cdr xs) (map1 cdr xss)))))
  (loop xs xss))

(define (filter p xs)
  (define head (list 1))
  (define end  head)
  (define (loop xs)
    (if (null? xs)
        (cdr head)
        (if (p (car xs))
            (begin (set-cdr! end (list (car xs)))
                   (set! end (cdr end))
                   (loop (cdr xs)))
            (loop (cdr xs)))))
  (loop xs))

(define (all p? lst . lsts) ;; is (p x) truthy for each x in lst?
  (if (null? lst)
      #t
      (if (apply p? (car lst) (map1 car lsts))
          (apply all p? (cdr lst) (map1 cdr lsts))
          #f)))

#|
This definition of memp is the expansion of this:

(define (memp p? lst)
  (let loop ([lst lst])
    (cond [(null? lst) #f]
          [(p? (car lst)) lst]
          [else (loop (cdr lst))])))
|#
(define (memp p? lst)
  (define (loop lst)
    (if (null? lst)
        #f
        (if (p? (car lst))
            lst
            (loop (cdr lst)))))
  (loop lst))

#|
Version with cond for ease of reading:
(define (member obj lst . compare)
  (cond [(null? compare) (member obj lst equal?)]
        ; this check is omitted, as is typical in Thcheme for
        ; optional-argument functions. That may change way down the line.
        [(not (null? (cdr compare)))
         (error "too many arguments to member")]
        [else
         (set! compare (car compare))
         (define (e? m) (compare obj m))
         (memp e? lst)]))
|#
(define (member obj lst . compare)
  (if (null? compare)
      (member obj lst equal?)
      ; this is not valid r7rs Scheme, but Thcheme accepts
      ; defines in the middle of bodies like this in code which
      ; does not go through the expander. It behaves like a 'let'
      ; form that got optimized away, except it's not hygienic.
      (begin (set! compare (car compare))
             (define (e? m) (compare obj m))
             (memp e? lst))))

(define (memq obj lst) (member obj lst eq?))
(define (memv obj lst) (member obj lst eqv?))

(define (assp p? alist)
  (define (loop alist)
    (if (null? alist)
        #f
        (if (p? (caar alist))
            (car alist)
            (loop (cdr alist)))))
  (loop alist))

; See the comments on member, the sugaring is essentially the same.
(define (assoc obj alist . compare)
  (if (null? compare)
      (assoc obj alist equal?)
      (begin (set! compare (car compare))
             (define (e? key) (compare obj key))
             (assp e? alist))))

(define (assv obj alist) (assoc obj alist eqv?))
(define (assq obj alist) (assoc obj alist eq?))

;; This is not the standard for-each since it does not validate
;; its inputs and stops only when the _first_ list runs out.
(define (for-each proc list1 . lists)
  (define (loop xs xss)
    (if (null? xs)
        (values)
        (begin (apply proc (car xs) (map1 car xss))
               (loop (cdr xs) (map1 cdr xss)))))
  (loop list1 lists))

;;; Smallest possible subset of R7RS 'eval' spec so that the expander can run
#|
(let ([prim-eval eval]
      [new-eval #f])
  (let () ;; silly funging to get the correct procedure name into the closure
    (define (eval expr env)
      (prim-eval expr))
    (set! new-eval eval))
  (set! eval new-eval))
|#
((lambda (prim-eval new-eval)
   ((lambda ()
      (define (eval expr env)
        (prim-eval expr))
      (set! new-eval eval)))
   (set! eval new-eval))
 eval
 #f)

(define (interaction-environment) 'interaction-environment)
