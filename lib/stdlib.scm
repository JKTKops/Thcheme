;;; CONSTANTS
(define nil ())
(define call/cc call-with-current-continuation)
(define define-macro defmacro)

;;; MACROS
(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x### test1)) ; best we can do until hygiene
       (if x### x### (or test2 ...))))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax-rule (when test result result* ...)
  (if test (begin result result* ...)))

(define-syntax-rule (unless test result result* ...)
  (if (not test) (begin result result* ...)))

;;; BINDINGS

(defmacro defun (name args . body)
  (cond ((pair? args) `(define ,(cons name args) ,@body))
        ((symbol? args) `(define ,name (lambda ,args ,@body)))
        (else (error "defun: Invalid type: expected symbol or pair"))))

; (defmacro let (lst body1 . body*) ; must have at least one body form
;   (if (null? lst) 
;       `(begin ,body1 ,@body*)
;       `((lambda ,(map car lst) ,body1 ,@body*) ,@(map cadr lst))))

(define-syntax let
  (syntax-rules ()
    ((let ((var val) ...) body1 body* ...)
     ((lambda (var ...) body1 body* ...) val ...))
    ((let tag ((var val) ...) body1 body* ...)
     (letrec ([tag (lambda (var ...) body1 body* ...)])
       (tag val ...)))))

; (defmacro let* (lst body1 . body*) ; same as above
;   (if (null? lst)
;       `(let () ,body1 ,@body*)
;       `(let ((,(first (car lst)) ,(second (car lst))))
;          (let* ,(cdr lst) ,body1 ,@body*))))

(define-syntax let*
  (syntax-rules ()
    ((let* (binding1 binding* ...) body1 body* ...)
     (let (binding1) (let* (binding* ...) body1 body* ...)))
    ((let* () body1 body* ...)
     (begin body1 body* ...))))

; (defmacro letrec (lst body1 . body*)
;   (let* ((make-define (lambda (pair)
;            (if (not (pair? pair))
;              (error "letrec: binding is not a pair")
;              (cons 'define pair))))
;          (dfns (map make-define lst)))
;     `(begin ,@dfns ,body1 ,@body*)))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var val) ...) body1 body* ...)
     ((lambda () (define var val) ... body1 body* ...)))))

; These macros appear to be correct, but are currently
; commented out because they rely on hygiene.
; (define-syntax let-values
;   (syntax-rules ()
;     ((let-values (binding ...) body1 body* ...)
;      (let-values "bind"
;        (binding ...) () (begin body1 body* ...)))
;     
;     ((let-values "bind" () tmps body)
;      (let tmps body))
;     
;     ((let-values "bind" ((b0 e0) binding ...) tmps body)
;      (let-values "mktmp" b0 e0 () (binding ...) tmps body))
;     
;     ((let-values "mktmp" () e0 args bindings tmps body)
;      (call-with-values
;        (lambda () e0)
;        (lambda args
;          (let-values "bind" bindings tmps body))))
;     
;     ((let-values "mktmp" (a . b) e0 (arg ...) bindings (tmp ...) body)
;      (let-values "mktmp" b e0 (arg ... x) bindings (tmp ... (a x)) body))
;   
;     ((let-values "mktmp" a e0 (arg ...) bindings (tmp ...) body)
;      (call-with-values
;        (lambda () e0)
;        (lambda (arg ... . x)
;          (let-values "bind" bindings (tmp ... (a x)) body))))))
; 
; (define-syntax let*-values
;   (syntax-rules ()
;     ((let*-values () body body* ...)
;      (let () body body* ...))
;     ((let*-values (binding binding* ...) body body* ...)
;      (let-values (binding) (let*-values (binding* ...) body body* ...)))))
; 
; I don't really know if this one is entirely safe because of
; hygiene concerns. Really, once the hygienic system is in place,
; the local macro should be defined with let-syntax instead.
; Also, recursive case-lambda functions are quite slow as long
; as macros expansion still happens during evaluation.
(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda (params body0 ...) ...)
     (lambda args
       (let ([len### (length args)])
         (define-syntax cl###
           (syntax-rules ::: ()
             ((cl###)
              (error "no matching clause"))
             ((cl### ((p :::) . body) . rest)
              (if (= len### (length '(p :::)))
                  (apply (lambda (p :::) . body) args)
                  (cl### . rest)))
             ((cl### ((p ::: . tail) . body) . rest)
              (if (>= len (length '(p :::)))
                  (apply
                   (lambda (p ::: . tail) . body)
                   args)
                  (cl### . rest)))))
         (cl### (params body0 ...) ...))))))

;;; LIST FUNCTIONS
;;; not currently organized...
;;; the expander will need very little of this,
;;; which means it should be moved into the appropriate
;;; build-up libraries once we have them.
;;;
;;; The parts that the expander requires will need to be
;;; expanded by hand.
;;;
;;; None of the functions that accept multiple lists are standard,
;;; because they should terminate when the shortest one runs out
;;; but these terminate when the first one runs out.
(define first car)
(define second cadr)
(define third caddr)

(define (length xs)
  (if (null? xs)
      0
      (+ 1 (length (cdr xs)))))

(define (reverse xs)
  (let loop ([xs xs] [acc ()])
    (if (null? xs)
        acc
        (loop (cdr xs) (cons (car xs) acc)))))

(define (map f xs . xss)
  (let loop ([xs xs] [xss xss])
    (if (null? xs)
        ()
        (cons (apply f (car xs) (map car xss))
              (loop (cdr xs) (map cdr xss))))))

(define (filter p xs)
  (if (null? xs)
      ()
      (if (p (car xs))
          (cons (car xs) (filter p (cdr xs)))
          (filter p (cdr xs)))))

(define (all p? lst . lsts) ;; is (p x) truthy for each x in lst?
  (or (null? lst)
      (and (apply p? (car lst) (map car lsts))
           (apply all p? (cdr lst) (map cdr lsts)))))

(define (memp p? lst)
  (let loop ([lst lst])
    (cond [(null? lst) #f]
          [(p? (car lst)) lst]
          [else (loop (cdr lst))])))

(define (member obj lst . compare)
  (cond [(null? compare) (member obj lst equal?)]
        [(not (null? (cdr compare)))
         (error "too many arguments to member")]
        [else
         (set! compare (car compare))
         (define (e? m) (compare obj m))
         (memp e? lst)]))

(define (memq obj lst) (member obj lst eq?))
(define (memv obj lst) (member obj lst eqv?))

(define (assp p? alist)
  (let loop ([alist alist])
    (cond [(null? alist) #f]
          [(p? (caar alist))
           (car alist)]
          [else (loop (cdr alist))])))

(define (assoc obj alist . compare)
  (cond [(null? compare) (assoc obj alist equal?)]
        [(not (null? (cdr compare)))
         (error "too many arguments to assoc")]
        [else
         (set! compare (car compare))
         (define (e? key) (compare obj key))
         (assp e? alist)]))

(define (assv obj alist) (assoc obj alist eqv?))
(define (assq obj alist) (assoc obj alist eq?))

;; This is not the standard for-each since it does not validate
;; its inputs and stops only when the _first_ list runs out.
(define (for-each proc list1 . lists)
  (let loop ([xs list1] [xss lists])
    (if (null? xs)
        (values)
        (begin (apply proc (car xs) (map car xss))
               (loop (cdr xs) (map cdr xss))))))

;;; String functions that should arguably be primitive but we just need for now
(define (string-append . strs)
  (list->string
    (apply append
      (map string->list strs))))

;;; Smallest possible subset of R7RS 'eval' spec so that the expander can run
(let ([prim-eval eval]
      [new-eval #f])
  (let () ;; silly funging to get the correct procedure name into the closure
    (define (eval expr env)
      (prim-eval expr))
    (set! new-eval eval))
  (set! eval new-eval))

(define (interaction-environment) 'interaction-environment)
