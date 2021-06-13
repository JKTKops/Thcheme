;;;=====================================================================
;;;
;;; Derived forms:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at http://srfi.schemers.org/srfi-process.html
;;;
;;;=====================================================================
;;;
;;; As allowed by that copyright statement, modified for Thcheme
;;; by Max Kopinsky, 2021.
;;;
;;;=====================================================================
;;;
;;; This file builds r7rs up using a sequence of libraries.
;;; It constitutes a nontrivial example, tutorial and test
;;; of the library system.
;;;
;;; It is meant to be expanded by expander.scm and compiled
;;; together with the latter before using in a production system.
;;;
;;; Various of the standard macros were copied from
;;; SRFI-93 reference implementation.
;;;
;;; An explicit renaming library is included for easier
;;; porting of legacy macros in some implementations.
;;;
;;;=====================================================================

(library (core primitives)

  (export

   ;; Macros defined in core expander:

   begin if lambda quote set! and or
   define define-syntax let-syntax letrec-syntax
   _ ... syntax syntax-case

   ;; Procedures and values defined in core expander:

   (rename (ex:make-variable-transformer make-variable-transformer)
           (ex:identifier?               identifier?)
           (ex:bound-identifier=?        bound-identifier=?)
           (ex:free-identifier=?         free-identifier=?)
           (ex:generate-temporaries      generate-temporaries)
           (ex:datum->syntax             datum->syntax)
           (ex:syntax->datum             syntax->datum)
           (ex:syntax-violation          syntax-violation)
           (ex:environment               environment)
           (ex:environment-bindings      environment-bindings)
           (ex:eval                      eval)
           (ex:undefined                 undefined)))

  (import

   (only (core primitive-macros)

     begin if set! and or lambda quote
     define define-syntax let-syntax letrec-syntax
     syntax syntax-case _ ...)

   ;; An extension to the r6rs import syntax, used here to make
   ;; available variable bindings provided natively.
   ;; This will not work for macros, which have to be defined
   ;; within the context of this expander.

   (primitives

    ;; Procedures and values defined in the core expander:

    ex:make-variable-transformer ex:identifier? ex:bound-identifier=?
    ex:free-identifier=? ex:generate-temporaries ex:datum->syntax ex:syntax->datum
    ex:syntax-violation ex:environment ex:environment-bindings ex:eval
    ex:undefined
    ))

  ) ;; core primitives

(library (core with-syntax)
  (export with-syntax)
  (import (for (core primitives) run expand)
          (primitives list))

  (define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
        ((_ () e1 e2 ...)             (syntax (begin e1 e2 ...)))
        ((_ ((out in)) e1 e2 ...)     (syntax (syntax-case in ()
                                                (out (begin e1 e2 ...)))))
        ((_ ((out in) ...) e1 e2 ...) (syntax (syntax-case (list in ...) ()
                                                ((out ...) (begin e1 e2 ...))))))))
  )

(library (core syntax-rules)
  (export syntax-rules)
  (import (for (core primitives)        expand run)
          (for (core with-syntax)       expand)
          (for (primitives for-all map) expand))

  (define-syntax syntax-rules
    (lambda (x)
      (define clause
        (lambda (y)
          (syntax-case y ()
            (((keyword . pattern) template)
             (syntax ((dummy . pattern) (syntax template))))
            (_
             (syntax-violation 'syntax-rules "Invalid expression" x)))))
      (syntax-case x ()
        ((_ (k ...) cl ...)
         (for-all identifier? (syntax (k ...)))
         (with-syntax (((cl ...) (map clause (syntax (cl ...)))))
           (syntax
            (lambda (x) (syntax-case x (k ...) cl ...))))))))
  )

(library (core let)
  (export let letrec letrec*)
  (import (for (core primitives)        expand run)
          (for (core with-syntax)       expand)
          (for (primitives for-all)     expand))

  (define-syntax let
    (lambda (x)
      (syntax-case x ()
        ((_ ((x v) ...) e1 e2 ...)
         (for-all identifier? (syntax (x ...)))
         (syntax ((lambda (x ...) e1 e2 ...) v ...)))
        ((_ f ((x v) ...) e1 e2 ...)
         (for-all identifier? (syntax (f x ...)))
         (syntax ((letrec ((f (lambda (x ...) e1 e2 ...))) f) v ...))))))

  (define-syntax letrec
    (lambda (x)
      (syntax-case x ()
        ((_ ((i v) ...) e1 e2 ...)
         (with-syntax (((t ...) (generate-temporaries (syntax (i ...)))))
           (syntax (let ((i undefined) ...)
                     (let ((t v) ...)
                       (set! i t) ...
                       (let () e1 e2 ...)))))))))

  (define-syntax letrec*
    (lambda (x)
      (syntax-case x ()
        ((_ ((i v) ...) e1 e2 ...)
         (syntax (let ()
                   (define i v) ...
                   (let () e1 e2 ...)))))))

  ) ; let

(library (core derived)
  (export let* cond case else =>)
  (import (for (core primitives)       expand run)
          (for (core let)              expand run)
          (for (core with-syntax)      expand)
          (for (core syntax-rules)     expand)
          (for (primitives for-all null? memv car cdr) expand run))

  (define-syntax let*
    (lambda (x)
      (syntax-case x ()
        ((_ () e1 e2 ...)
         (syntax (let () e1 e2 ...)))
        ((_ ((x v) ...) e1 e2 ...)
         (for-all identifier? (syntax (x ...)))
         (let f ((bindings (syntax ((x v) ...))))
           (syntax-case bindings ()
             (((x v))        (syntax (let ((x v)) e1 e2 ...)))
             (((x v) . rest) (with-syntax ((body (f (syntax rest))))
                               (syntax (let ((x v)) body))))))))))

  (define-syntax cond
    (lambda (x)
      (syntax-case x ()
        ((_ c1 c2 ...)
         (let f ((c1  (syntax c1))
                 (c2* (syntax (c2 ...))))
           (syntax-case c2* ()
             (()
              (syntax-case c1 (else =>)
                ((else e1 e2 ...) (syntax (begin e1 e2 ...)))
                ((e0)             (syntax (let ((t e0)) (if t t))))
                ((e0 => e1)       (syntax (let ((t e0)) (if t (e1 t)))))
                ((e0 e1 e2 ...)   (syntax (if e0 (begin e1 e2 ...))))
                (_                (syntax-violation 'cond "Invalid expression" x))))
             ((c2 c3 ...)
              (with-syntax ((rest (f (syntax c2)
                                     (syntax (c3 ...)))))
                (syntax-case c1 (else =>)
                  ((e0)           (syntax (let ((t e0)) (if t t rest))))
                  ((e0 => e1)     (syntax (let ((t e0)) (if t (e1 t) rest))))
                  ((e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...) rest)))
                  (_              (syntax-violation 'cond "Invalid expression" x)))))))))))

  (define-syntax case
    (lambda (x)
      (syntax-case x ()
        ((_ e c1 c2 ...)
         (with-syntax ((body
                        (let f ((c1 (syntax c1))
                                (cmore (syntax (c2 ...))))
                          (if (null? cmore)
                              (syntax-case c1 (else)
                                ((else e1 e2 ...)    (syntax (begin e1 e2 ...)))
                                (((k ...) e1 e2 ...) (syntax (if (memv t '(k ...))
                                                                 (begin e1 e2 ...)))))
                              (with-syntax ((rest (f (car cmore) (cdr cmore))))
                                (syntax-case c1 ()
                                  (((k ...) e1 e2 ...)
                                   (syntax (if (memv t '(k ...))
                                               (begin e1 e2 ...)
                                               rest)))))))))
           (syntax (let ((t e)) body)))))))

  (define-syntax =>
    (lambda (x)
      (syntax-violation '=> "Invalid expression" x)))

  (define-syntax else
    (lambda (x)
      (syntax-violation 'else "Invalid expression" x)))

  ) ; derived

(library (core identifier-syntax)
  (export identifier-syntax)
  (import (for (core primitives)
            expand
            run
            ;; since generated macro contains (syntax set!) at level 0
            (meta -1)))

  (define-syntax identifier-syntax
    (lambda (x)
      (syntax-case x (set!)
        ((_ e)
         (syntax (lambda (x)
                   (syntax-case x ()
                     (id (identifier? (syntax id)) (syntax e))
                     ((_ x (... ...))              (syntax (e x (... ...))))))))
        ((_ (id exp1)
            ((set! var val) exp2))
         (and (identifier? (syntax id))
              (identifier? (syntax var)))
         (syntax
          (make-variable-transformer
           (lambda (x)
             (syntax-case x (set!)
               ((set! var val)               (syntax exp2))
               ((id x (... ...))             (syntax (exp1 x (... ...))))
               (id (identifier? (syntax id)) (syntax exp1))))))))))
  )

;;;=========================================================
;;;
;;; Quasisyntax in terms of syntax-case.
;;;
;;;=========================================================
;;;
;;; To make nested unquote-splicing behave in a useful way,
;;; the R5RS-compatible extension of quasiquote in appendix B
;;; of the following paper is here ported to quasisyntax:
;;;
;;; Alan Bawden - Quasiquotation in Lisp
;;; http://citeseer.ist.psu.edu/bawden99quasiquotation.html
;;;
;;; The algorithm converts a quasisyntax expression to an
;;; equivalent with-syntax expression.
;;; For example:
;;;
;;; (quasisyntax (set! #,a #,b))
;;;   ==> (with-syntax ((t0 a)
;;;                     (t1 b))
;;;         (syntax (set! t0 t1)))
;;;
;;; (quasisyntax (list #,@args))
;;;   ==> (with-syntax (((t ...) args))
;;;         (syntax (list t ...)))
;;;
;;; Note that quasisyntax is expanded first, before any
;;; ellipses act.  For example:
;;;
;;; (quasisyntax (f ((b #,a) ...))
;;;   ==> (with-syntax ((t a))
;;;         (syntax (f ((b t) ...))))
;;;
;;; so that
;;;
;;; (let-syntax ((test-ellipses-over-unsyntax
;;;               (lambda (e)
;;;                 (let ((a (syntax a)))
;;;                   (with-syntax (((b ...) (syntax (1 2 3))))
;;;                     (quasisyntax
;;;                      (quote ((b #,a) ...))))))))
;;;   (test-ellipses-over-unsyntax))
;;;
;;;     ==> ((1 a) (2 a) (3 a))

(library (core quasisyntax)
  (export quasisyntax unsyntax unsyntax-splicing)
  (import (for (core primitives)  run expand)
          (for (core let)         run expand)
          (for (core derived)     run expand)
          (for (core with-syntax) run expand)
          (for (primitives = > + - vector->list) run expand))

  (define-syntax quasisyntax
    (lambda (e)

      ;; Expand returns a list of the form
      ;;    [template[t/e, ...] (replacement ...)]
      ;; Here template[t/e ...] denotes the original template
      ;; with unquoted expressions e replaced by fresh
      ;; variables t, followed by the appropriate ellipses
      ;; if e is also spliced.
      ;; The second part of the return value is the list of
      ;; replacements, each of the form (t e) if e is just
      ;; unquoted, or ((t ...) e) if e is also spliced.
      ;; This will be the list of bindings of the resulting
      ;; with-syntax expression.

      (define (expand x level)
        (syntax-case x (quasisyntax unsyntax unsyntax-splicing)
          ((quasisyntax e)
           (with-syntax (((k _)     x) ;; original identifier must be copied
                         ((e* reps) (expand (syntax e) (+ level 1))))
             (syntax ((k e*) reps))))
          ((unsyntax e)
           (= level 0)
           (with-syntax (((t) (generate-temporaries '(t))))
             (syntax (t ((t e))))))
          (((unsyntax e ...) . r)
           (= level 0)
           (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                         ((t ...)        (generate-temporaries (syntax (e ...)))))
             (syntax ((t ... . r*)
                      ((t e) ... rep ...)))))
          (((unsyntax-splicing e ...) . r)
           (= level 0)
           (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                         ((t ...)        (generate-temporaries (syntax (e ...)))))
             (with-syntax ((((t ...) ...) (syntax ((t (... ...)) ...))))
               (syntax ((t ... ... . r*)
                        (((t ...) e) ... rep ...))))))
          ((k . r)
           (and (> level 0)
                (identifier? (syntax k))
                (or (free-identifier=? (syntax k) (syntax unsyntax))
                    (free-identifier=? (syntax k) (syntax unsyntax-splicing))))
           (with-syntax (((r* reps) (expand (syntax r) (- level 1))))
             (syntax ((k . r*) reps))))
          ((h . t)
           (with-syntax (((h* (rep1 ...)) (expand (syntax h) level))
                         ((t* (rep2 ...)) (expand (syntax t) level)))
             (syntax ((h* . t*)
                      (rep1 ... rep2 ...)))))
          (#(e ...)
           (with-syntax ((((e* ...) reps)
                          (expand (vector->list (syntax #(e ...))) level)))
             (syntax (#(e* ...) reps))))
          (other
           (syntax (other ())))))

      (syntax-case e ()
        ((_ template)
         (with-syntax (((template* replacements) (expand (syntax template) 0)))
           (syntax
            (with-syntax replacements (syntax template*))))))))

  (define-syntax unsyntax
    (lambda (e)
      (syntax-violation 'unsyntax "Invalid expression" e)))

  (define-syntax unsyntax-splicing
    (lambda (e)
      (syntax-violation 'unsyntax "Invalid expression" e)))
  )

(library (core quasiquote)
  (export quasiquote unquote unquote-splicing)
  (import (for (core primitives)  run expand)
          (for (core let)         run expand)
          (for (core derived)     run expand)
          (for (core with-syntax) expand)
          (for (core quasisyntax) expand)
          (for (primitives = + - null? cons car cdr append map list vector list->vector)
            run expand))

  ;; Optimised version copied from portable syntax-case (Dybvig)

  (define-syntax quasiquote
    (let ()
      (define (quasi p lev)
        (syntax-case p (unquote quasiquote)
          ((unquote p)
           (if (= lev 0)
               (syntax ("value" p))
               (quasicons (syntax ("quote" unquote)) (quasi (syntax (p)) (- lev 1)))))
          ((quasiquote p) (quasicons (syntax ("quote" quasiquote)) (quasi (syntax (p)) (+ lev 1))))
          ((p . q)
           (syntax-case (syntax p) (unquote unquote-splicing)
             ((unquote p ...)
              (if (= lev 0)
                  (quasilist* (syntax (("value" p) ...)) (quasi (syntax q) lev))
                  (quasicons
                   (quasicons (syntax ("quote" unquote)) (quasi (syntax (p ...)) (- lev 1)))
                   (quasi (syntax q) lev))))
             ((unquote-splicing p ...)
              (if (= lev 0)
                  (quasiappend (syntax (("value" p) ...)) (quasi (syntax q) lev))
                  (quasicons
                   (quasicons (syntax ("quote" unquote-splicing)) (quasi (syntax (p ...)) (- lev 1)))
                   (quasi (syntax q) lev))))
             (_ (quasicons (quasi (syntax p) lev) (quasi (syntax q) lev)))))
          (#(x ...) (quasivector (vquasi (syntax (x ...)) lev)))
          (p (syntax ("quote" p)))))
      (define (vquasi p lev)
        (syntax-case p ()
          ((p . q)
           (syntax-case (syntax p) (unquote unquote-splicing)
             ((unquote p ...)
              (if (= lev 0)
                  (quasilist* (syntax (("value" p) ...)) (vquasi (syntax q) lev))
                  (quasicons
                   (quasicons (syntax ("quote" unquote)) (quasi (syntax (p ...)) (- lev 1)))
                   (vquasi (syntax q) lev))))
             ((unquote-splicing p ...)
              (if (= lev 0)
                  (quasiappend (syntax (("value" p) ...)) (vquasi (syntax q) lev))
                  (quasicons
                   (quasicons
                    (syntax ("quote" unquote-splicing))
                    (quasi (syntax (p ...)) (- lev 1)))
                   (vquasi (syntax q) lev))))
             (_ (quasicons (quasi (syntax p) lev) (vquasi (syntax q) lev)))))
          (() (syntax ("quote" ())))))
      (define (quasicons x y)
        (with-syntax ((x x) (y y))
          (syntax-case (syntax y) ()
            (("quote" dy)
             (syntax-case (syntax x) ()
               (("quote" dx) (syntax ("quote" (dx . dy))))
               (_ (if (null? (syntax dy)) (syntax ("list" x)) (syntax ("list*" x y))))))
            (("list" . stuff) (syntax ("list" x . stuff)))
            (("list*" . stuff) (syntax ("list*" x . stuff)))
            (_ (syntax ("list*" x y))))))
      (define (quasiappend x y)
        (syntax-case y ()
          (("quote" ())
           (cond
             ((null? x) (syntax ("quote" ())))
             ((null? (cdr x)) (car x))
             (else (with-syntax (((p ...) x)) (syntax ("append" p ...))))))
          (_
           (cond
             ((null? x) y)
             (else (with-syntax (((p ...) x) (y y)) (syntax ("append" p ... y))))))))
      (define (quasilist* x y)
        (let f ((x x))
          (if (null? x)
              y
              (quasicons (car x) (f (cdr x))))))
      (define (quasivector x)
        (syntax-case x ()
          (("quote" (x ...)) (syntax ("quote" #(x ...))))
          (_
           (let f ((y x) (k (lambda (ls) (quasisyntax ("vector" (unsyntax-splicing ls))))))
             (syntax-case y ()
               (("quote" (y ...)) (k (syntax (("quote" y) ...))))
               (("list" y ...) (k (syntax (y ...))))
               (("list*" y ... z) (f (syntax z) (lambda (ls) (k (append (syntax (y ...)) ls)))))
               (else (quasisyntax ("list->vector" (unsyntax x)))))))))
      (define (emit x)
        (syntax-case x ()
          (("quote" x) (syntax 'x))
          (("list" x ...) (quasisyntax (list (unsyntax-splicing (map emit (syntax (x ...)))))))
          ;; could emit list* for 3+ arguments if implementation supports list*
          (("list*" x ... y)
           (let f ((x* (syntax (x ...))))
             (if (null? x*)
                 (emit (syntax y))
                 (quasisyntax (cons (unsyntax (emit (car x*))) (unsyntax (f (cdr x*))))))))
          (("append" x ...) (quasisyntax (append (unsyntax-splicing (map emit (syntax (x ...)))))))
          (("vector" x ...) (quasisyntax (vector (unsyntax-splicing (map emit (syntax (x ...)))))))
          (("list->vector" x) (quasisyntax (list->vector (unsyntax (emit (syntax x))))))
          (("value" x) (syntax x))))
      (lambda (x)
        (syntax-case x ()
          ;; convert to intermediate language, combining introduced (but not
          ;; unquoted source) quote expressions where possible and choosing
          ;; optimal construction code otherwise, then emit Scheme code
          ;; corresponding to the intermediate language forms.
          ((_ e) (emit (quasi (syntax e) 0)))))))

  (define-syntax unquote
    (lambda (e)
      (syntax-violation 'unquote "Invalid expression" e)))

  (define-syntax unquote-splicing
    (lambda (e)
      (syntax-violation 'unquote-splicing "Invalid expression" e)))
  )

(library (core let-values)
  (export let-values let*-values)
  (import (for (core primitives)   expand run)
          (for (core syntax-rules) expand)
          (core let)
          (primitives call-with-values))

  (define-syntax let-values
    (syntax-rules ()
      ((let-values (?binding ...) ?body0 ?body1 ...)
       (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))
      ((let-values "bind" () ?tmps ?body)
       (let ?tmps ?body))
      ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
       (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))
      ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
       (call-with-values
        (lambda () ?e0)
        (lambda ?args
          (let-values "bind" ?bindings ?tmps ?body))))
      ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
       (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))
      ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
       (call-with-values
        (lambda () ?e0)
        (lambda (?arg ... . x)
          (let-values "bind" ?bindings (?tmp ... (?a x)) ?body))))))

  (define-syntax let*-values
    (syntax-rules ()
      ((let*-values () ?body0 ?body1 ...)
       (begin ?body0 ?body1 ...))
      ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
       (let-values (?binding0)
         (let*-values (?binding1 ...) ?body0 ?body1 ...)))))

  ) ; core let-values

(library (core define-values)
  (export define-values)
  (import (for (core primitives)   expand run)
          (for (core syntax-rules) expand)
          (core let)
          (primitives call-with-values car cadr cddr set-cdr! list))

  (define-syntax define-values
    (syntax-rules ()
      ((define-values () expr)
       (define dummy
         (call-with-values (lambda () expr)
                           (lambda args #f))))
      ((define-values (var) expr)
       (define var expr))
      ((define-values (var0 var1 ... varn) expr)
       (begin
         (define var0
           (call-with-values (lambda () expr) list))
         (define var1
           (let ([v (cadr var0)])
             (set-cdr! var0 (cddr var0))
             v)) ...
         (define varn
           (let ([v (cadr var0)])
             (set! var0 (car var0))
             v))))
      ((define-values var expr)
       (define var
         (call-with-values (lambda () expr) list)))))

  ) ; core define-values

(library (rnrs control (6))
  (export when unless do case-lambda)
  (import (for (core primitives)   expand run)
          (for (core let)          expand run)
          (for (core with-syntax)  expand)
          (for (core syntax-rules) expand)
          (for (primitives not map length assertion-violation = >= apply)
            expand run) )

  (define-syntax when
    (syntax-rules ()
      ((when test result1 result2 ...)
       (if test
           (begin result1 result2 ...)))))

  (define-syntax unless
    (syntax-rules ()
      ((unless test result1 result2 ...)
       (if (not test)
           (begin result1 result2 ...)))))

  (define-syntax do
    (lambda (orig-x)
      (syntax-case orig-x ()
        ((_ ((var init . step) ...) (e0 e1 ...) c ...)
         (with-syntax (((step ...)
                        (map (lambda (v s)
                               (syntax-case s ()
                                 (()  v)
                                 ((e) (syntax e))
                                 (_   (syntax-violation 'do "Invalid step" orig-x s))))
                             (syntax (var ...))
                             (syntax (step ...)))))
           (syntax-case (syntax (e1 ...)) ()
             (()          (syntax (let do ((var init) ...)
                                    (if (not e0)
                                        (begin c ... (do step ...))))))
             ((e1 e2 ...) (syntax (let do ((var init) ...)
                                    (if e0
                                        (begin e1 e2 ...)
                                        (begin c ... (do step ...))))))))))))

  (define-syntax case-lambda
    (syntax-rules ()
      ((_ (fmls b1 b2 ...))
       (lambda fmls b1 b2 ...))
      ((_ (fmls b1 b2 ...) ...)
       (lambda args
         (let ((n (length args)))
           (case-lambda-help args n
                             (fmls b1 b2 ...) ...))))))

  (define-syntax case-lambda-help
    (syntax-rules ()
      ((_ args n)
       (assertion-violation #f "unexpected number of arguments"))
      ((_ args n ((x ...) b1 b2 ...) more ...)
       (if (= n (length '(x ...)))
           (apply (lambda (x ...) b1 b2 ...) args)
           (case-lambda-help args n more ...)))
      ((_ args n ((x1 x2 ... . r) b1 b2 ...) more ...)
       (if (>= n (length '(x1 x2 ...)))
           (apply (lambda (x1 x2 ... . r) b1 b2 ...)
                  args)
           (case-lambda-help args n more ...)))
      ((_ args n (r b1 b2 ...) more ...)
       (apply (lambda r b1 b2 ...) args))))

  ) ; rnrs control

(library (thcheme syntax-error (7))
  (export syntax-error)
  (import (for (core primitives) expand run)
          (for (primitives apply error) expand))
  
  (define-syntax syntax-error
    (lambda (e)
      (syntax-case e ()
        ((_ msg args ...)
         (apply error #'(msg args ...))))))
  ) ; rnrs syntax-error

(library (thcheme parameters (7))
  (export make-parameter parameterize)
  (import (for (core syntax-rules) expand)
          (for (core primitives)   expand run)
          (for (core derived)      expand run)
          (core let)
          (primitives dynamic-wind id eq? error list
                      pair? null? cons car cadr))

  (define <param-set!>    (list '<param-set!>))
  (define <param-convert> (list '<param-convert>))

  (define (make-parameter init . converter)
    (let* ([converter (if (pair? converter)
                          (car converter)
                          id)]
           [value (converter init)])
      (lambda args
        (cond
         [(null? args) value]
         [(eq? (car args) <param-set!>)
          (set! value (cadr args))]
         [(eq? (car args) <param-convert>)
          converter]
         [else
          (error "bad parameter syntax: " (cons '<parameter> args))]))))

  (define-syntax parameterize
    (syntax-rules ()
      ((parameterize "step"
                     ((param value p old new) ...)
                     ()
                     body)
       (let ([p param] ...)
         (let ([old (p)] ...
               [new ((p <param-convert>) value)] ...)
           (dynamic-wind
            (lambda () (p <param-set!> new) ...)
            (lambda () . body)
            (lambda () (p <param-set!> old) ...)))))
      ((parameterize "step"
                     args
                     ((param value) . rest)
                     body)
       (parameterize "step"
                     ((param value p old new) . args)
                     rest
                     body))
      ((parameterize ((param value) ...) . body)
       (parameterize "step" 
                     ()
                     ((param value) ...)
                     body))))
  ) ; thcheme parameters

(library (thcheme ports)
  (export call-with-port call-with-input-file call-with-output-file
          input-port? output-port? textual-port? binary-port? port?
          input-port-open? output-port-open?
          current-input-port current-output-port current-error-port
          with-input-from-file with-output-to-file
          with-input-from-string with-output-to-string
          open-input-file open-output-file
          close-port close-input-port close-output-port
          open-input-string open-output-string get-output-string)

  (import (core primitives)
          (thcheme parameters)
          (primitives
           call-with-port call-with-input-file call-with-output-file
           input-port? output-port? textual-port? binary-port? port?
           input-port-open? output-port-open?
           open-input-file open-output-file
           close-port close-input-port close-output-port
           open-input-string open-output-string get-output-string
           standard-input-port standard-output-port standard-error-port))

  (define current-input-port  (make-parameter (standard-input-port)))
  (define current-output-port (make-parameter (standard-output-port)))
  (define current-error-port  (make-parameter (standard-error-port)))

  (define (with-input-from-file filename thunk)
    (define p (open-input-file filename))
    (define result
      (parameterize ([current-input-port p])
        (thunk)))
    (close-input-port p)
    result)
  
  (define (with-output-to-file filename thunk)
    (define p (open-output-file filename))
    (define result
      (parameterize ([current-output-port p])
        (thunk)))
    (close-output-port p)
    result)
  
  (define (with-input-from-string string thunk)
    (define p (open-input-string string))
    (define result
      (parameterize ([current-input-port p])
        (thunk)))
    (close-input-port p)
    result)
  
  (define (with-output-to-string thunk)
    (define p (open-output-string))
    (parameterize ([current-output-port p])
      (thunk))
    (get-output-string p))
  ) ; thcheme ports

(library (thcheme with-default-port)
  (export with-default-port0 with-default-port1)
  (import (core primitives)
          (core let)
          (core derived)
          (primitives null? pair? car))

  (define (with-default-port0 get-port proc)
    (lambda port
      (let ([port (cond [(null? port) (get-port)]
                        [(pair? port) (car port)])])
        (proc port))))
 
  (define (with-default-port1 get-port proc)
    (lambda (obj . port)
      (let ([port (cond [(null? port) (get-port)]
                        [(pair? port) (car port)])])
        (proc obj port))))
  ) ; thcheme with-default-port

; probably want (thcheme input) and (thcheme output) libraries here.

(library (scheme base (7))

  (export
   
   ;; Macros defined in core expander:
   
   begin if lambda quote set! and or
   define define-syntax let-syntax letrec-syntax
   _ ...
   
   ;; Derived syntax:
   
   let let* letrec letrec* let-values let*-values
   case cond else =>
   define-values
   ; guard
   when unless do
   parameterize
   quasiquote unquote unquote-splicing
   syntax-error
   syntax-rules identifier-syntax

   ;; R5RS primitives:

   * + - / < <= = > >= abs append apply
   boolean? call-with-current-continuation
   call-with-values car cdr caar cadr cdar cddr
   ceiling char? char->integer
   complex? cons
   denominator dynamic-wind
   eq? equal? eqv? even? exact? expt floor for-each
   gcd imag-part inexact? integer->char integer?
   lcm length list list->string
   list->vector list-ref list-tail list?
   make-string make-vector map max min
   negative? not null? number->string number? numerator
   odd? pair?
   positive? procedure? rational? rationalize
   real-part real? reverse round
   string string->list string->number string->symbol
   string-append
   string-copy string-length string-ref string<=? string<?
   string=? string>=? string>? string? substring symbol->string symbol?
   truncate values vector vector->list
   vector-fill! vector-length vector-ref vector-set! vector? zero?

   ;; R7RS additional procedures

   assoc assq assv
   binary-port? boolean=? bytevector?
   call-with-port call/cc char-ready? char<=? char<? char=? char>=? char>?
   close-input-port close-output-port close-port
   current-input-port current-output-port current-error-port
   define-record-type
   eof-object eof-object? error error-object-irritants error-object-message
   exact exact-integer-sqrt exact-integer?
   features file-error? floor-quotient floor-remainder floor/ flush-output-port
   get-output-string
   inexact input-port-open? input-port?
   list-copy list-set!
   make-list make-parameter member memq memv modulo
   newline
   open-input-string open-output-string output-port-open? output-port?
   peek-char port?
   quotient
   raise raise-continuable read-char read-error? read-line read-string
   remainder
   set-car! set-cdr!
   square string->vector string-copy! string-fill! string-for-each string-map
   string-set! symbol=?
   textual-port? truncate-quotient truncate-remainder truncate/
   vector->string vector-append vector-copy vector-copy! vector-for-each
   vector-map
   with-exception-handler write-char write-string)

  (import
   (except (core primitives) _ ...)
   (for (only (core primitives) _ ... set!) expand)
   (core let)
   (core derived)
   (core quasiquote)
   (core let-values)
   (core define-values)
   (rnrs control)
   (thcheme syntax-error)
   (thcheme parameters)
   (thcheme ports)
   (for (core syntax-rules)      expand)
   (for (core identifier-syntax) expand)
   (primitives
    ;; R5RS primitives:

    * + - / < <= = > >= abs append apply
    boolean? call-with-current-continuation
    call-with-values car cdr caar cadr cdar cddr
    ceiling char? char->integer
    complex? cons
    denominator dynamic-wind
    eq? equal? eqv? even? exact? expt floor for-each
    gcd imag-part inexact? integer->char integer?
    lcm length list list->string
    list->vector list-ref list-tail list?
    make-string make-vector map max min
    negative? not null? number->string number? numerator
    odd? pair?
    positive? procedure? rational? rationalize
    real-part real? reverse round
    string string->list string->number string->symbol
    string-append
    string-copy string-length string-ref string<=? string<?
    string=? string>=? string>? string? substring symbol->string symbol?
    truncate values vector vector->list
    vector-fill! vector-length vector-ref vector-set! vector? zero?

    ;; R7RS additional procedures

    assoc assq assv
    binary-port? boolean=? bytevector?
    call-with-port call/cc char-ready? char<=? char<? char=? char>=? char>?
    close-input-port close-output-port close-port
    define-record-type
    eof-object eof-object? error error-object-irritants error-object-message
    exact exact-integer-sqrt exact-integer?
    features file-error? floor-quotient floor-remainder floor/ flush-output-port
    get-output-string
    inexact input-port-open? input-port?
    list-copy list-set!
    make-list member memq memv modulo
    newline
    open-input-string open-output-string output-port-open? output-port?
    peek-char port?
    quotient
    raise raise-continuable read-char read-error? read-line read-string
    remainder
    set-car! set-cdr!
    square string->vector string-copy! string-fill! string-for-each string-map
    string-set! symbol=?
    textual-port? truncate-quotient truncate-remainder truncate/
    vector->string vector-append vector-copy vector-copy! vector-for-each
    vector-map
    with-exception-handler write-char write-string
    zero?))

  ) ; scheme base

(library (scheme case-lambda)
  (export case-lambda)
  (import (rnrs control)))

(library (scheme char)
  (export ; TODO
          )
  (import (primitives ; TODO
          )))

(library (scheme complex)
  (export angle imag-part magnitude make-polar make-rectangular real-part)
  (import
   (primitives
    angle imag-part magnitude make-polar make-rectangular real-part)))

(library (scheme cxr)
  (export
    caaar  caadr  cadar  caddr  cdaar  cdadr  cddar  cdddr
   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
  (import (primitives
    caaar  caadr  cadar  caddr  cdaar  cdadr  cddar  cdddr
   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)))

(library (scheme eval)
  (export environment eval)
  (import (core primitives)))

(library (thcheme eval reflection)
  (export environment-bindings)
  (import (core primitives)))

(library (scheme file)
  (export
   call-with-input-file call-with-output-file delete-file file-exists?
   open-input-file open-output-file with-input-from-file with-output-to-file)
  
  (import
   (thcheme ports)
   (primitives delete-file file-exists?)))

(library (scheme inexact)
  (export acos asin atan cos exp finite? infinite? log nan? sin sqrt tan)
  (import (primitives
           acos asin atan cos exp finite? infinite? log nan? sin sqrt tan)))

#| We can't load this library until define-record-type is known to the expander
   which will require probably a (core define-record-type) library, I guess.
   It also might not load for other reasons, but the logic should be right.
(library (scheme lazy)
  (export delay delay-force force make-promise promise?)
  (import (rnrs base)
          (primitives define-record-type))

  (define-record-type
    promise
    (box-promise payload)
    promise?
    ;; payload is a pair, because we need to be able to share the cons cell
    (payload payload-get payload-set!))

  (define (build-promise done? proc)
    (box-promise (cons done? proc)))
  
  (define (promise-done? p)
    (car (payload-get p)))
  (define (promise-value p)
    (cdr (payload-get p)))
  (define (promise-update! new old)
    (set-car! (payload-get old) (promise-done? new))
    (set-cdr! (payload-get old) (promise-value new))
    (payload-set! new (payload-get old)))
  
  (define-syntax delay-force
    (syntax-rules ()
      ((delay-force expression)
       (build-promise #f (lambda () expression)))))
  (define-syntax delay
    (syntax-rules ()
      ((delay expression)
       (delay-force (build-promise #t expression)))))

  (define (force promise)
    (if (promise-done? promise)
        (promise-value promise)
        (let ([promise* ((promise-value promise))])
          (unless (promise-done? promise)
            (promise-update! promise* promise))
          (force promise))))

  (define (make-promise obj)
    (if (promise? obj)
        obj
        (delay obj)))

  ) ; scheme lazy
|#

(library (scheme load)
  (export (rename (ex:load load)))
  (import (primitives ex:load)))

(library (scheme process-context)
  (export command-line emergency-exit exit
          get-environment-variable get-environment-variables)
  (import
   (primitives
    command-line emergency-exit exit
    get-environment-variable get-environment-variables)))

(library (scheme read)
  (export read)
  (import (core primitives)
          (thcheme ports)
          (thcheme with-default-port)
          (prefix (primitives read) thcheme:))
  (define read (with-default-port0 current-input-port thcheme:read))
  ) ; scheme read

(library (scheme repl)
  (export (rename (ex:interaction-environment interaction-environment)))
  (import (primitives ex:interaction-environment)))

(library (scheme time)
  (export current-jiffy current-second jiffies-per-second)
  (import (primitives current-jiffy current-second jiffies-per-second)))

(library (scheme write)
  (export display write write-shared write-simple)
  (import (core primitives)
          (thcheme ports)
          (thcheme with-default-port)
          (prefix (primitives display write write-shared write-simple)
                  thcheme:))

  (define write (with-default-port1 current-output-port thcheme:write))
  (define write-simple
    (with-default-port1 current-output-port thcheme:write-simple))
  (define write-shared
    (with-default-port1 current-output-port thcheme:write-shared))
  (define display
    (with-default-port1 current-output-port thcheme:display))
  ) ; scheme write

(library (rnrs syntax-case (6))
  (export make-variable-transformer
          identifier? bound-identifier=? free-identifier=?
          generate-temporaries datum->syntax syntax->datum
          syntax-violation syntax syntax-case quasisyntax
          unsyntax unsyntax-splicing with-syntax
          _ ...)

  (import (core primitives)
          (core with-syntax)
          (core quasisyntax))

  ) ;; rnrs syntax-case

;; Nonstandard explicit renaming library:
;; See also examples and discussion in file examples.scm.
;;
;; Exports:
;;
;;    er-transformer     (syntax)
;;    bound-identifier=? (procedure)
;;    datum->syntax      (procedure)
;;
;; Differences with traditional explicit renaming:
;;
;; - The renaming procedure has signature <symbol> -> <identifier>,
;;   where the <identifier> type is disjoint from the <symbol> type.
;;
;; - The renaming procedure acts as a mathematical function in the sense that
;;   the identifiers obtained from any two calls with the same argument will
;;   be the same in the sense of bound-identifier=?, not eqv?
;;
;; - The output may not contain raw symbols, so implicit identifiers must
;;   be introduced using datum->syntax.
;;
;; - Breaking hygiene with datum->syntax allows more modular macro
;;   programming than traditional explicit renaming.
;;   See in particular the example of while in terms of loop below.
;;
;; - The renaming procedure is aware of the transformer environment,
;;   so that identifiers not bound at the usage site will resolve to
;;   the r6rs library-local bindings at the transformer site.
;;   More precisely, they will be resolved in the lexical environment
;;   of the er-transformer keyword.
;;
;; - Fully compatible with my r6rs syntax-case macro system.
;;
;; Portability and complexity note:
;;
;;   This library is not r6rs-portable, since it assumes that the input
;;   to a transformer is always an unwrapped syntax object, which is
;;   allowed but not required by r6rs, and is currently only true for my
;;   implementation.  The library could be ported to other implementations
;;   by inserting a step that unwrapped the input to the transformer.
;;   However, that would adversely modify the complexity class of
;;   er-transformer macros in those implementations.

(library (explicit-renaming helper)
  (export er-transformer)
  (import (for (only (core primitives)
              define-syntax lambda syntax-case syntax datum->syntax free-identifier=?)
            expand run))

  (define-syntax er-transformer
    (lambda (exp)
      (syntax-case exp ()
        ((k proc)
         (syntax
          (lambda (form)
            (proc form
                  (lambda (symbol) (datum->syntax (syntax k) symbol))
                  free-identifier=?))))))))

(library (explicit-renaming)
  (export er-transformer identifier? bound-identifier=? datum->syntax)
  (import (explicit-renaming helper)
          (rnrs syntax-case)))