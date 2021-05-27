; the primary export of the file, R7RS-compliant define-record-type macro.
; See https://small.r7rs.org/attachment/r7rs.pdf, section 5.5.
; Though records are disjoint from vectors according to vector?, primitive
; vector procedures such as vector-ref will not raise errors when passed a
; record. This is a wontfix issue. Thcheme will provide record types as a
; primitive construct, this is merely a prototype.
(define define-record-type #f)

; secondary exports; a minimal, partial implementation of
; R6RS procedural records. Used by the earliest version of the expander.
; We'll get rid of this eventually.
(define records:make-record-type-descriptor #f)
(define (records:make-record-constructor-descriptor rtd . _) rtd)
(define records:record-accessor #f)
(define records:record-constructor #f)
(define records:record-predicate #f)

; additional exports for debugging
; (define records:field-spec? #f)
; (define records:normalize-field-spec #f)
; (define records:process-field-specs #f)

;;----------------------------------------------------------------------
;; The records psuedolibrary.
;; The surrounding `(let () ... )` prevents the non-exported functions
;; from leaking into the global namespace. The (single) export,
;; define-record-type, is exported at the end using set!.
;;----------------------------------------------------------------------

(let ()
  (define old-vector? vector?)
  ;; a "magic" token that identifies records. Note that it is a list
  ;; and not merely a symbol; symbols that are spelled the same compare
  ;; equal according to eq?, which would lead to the following catastrophe:
  ;; (define-record-type <box> (box x) box? (x box-get box-set!))
  ;; (define bogus-box (vector '(record . <box>)))
  ;; (box-get bogus-box)
  ;; => ERROR: vector index out of bounds
  ;; instead of ERROR: invalid type to record function.
  (define record-token (list 'record))
  (define (make-record-type-descriptor name)
    (list name)) ;; using list here is to make the
                 ;; macro generative (see R7RS).
  (define (record? x)
    (and (old-vector? x)
         (eq? (vector-ref x 0) record-token)
         #t))
  ;; redefine vector? to reject records, thus making record types
  ;; disjoint from all other types.
  (set! vector?
    (lambda (x)
      (and (old-vector? x)
           (or (= 0 (vector-length x))
               (not (eq? (vector-ref x 0) record-token))))))

  ;; Given the type descriptor,
  ;; produce a type predicate that determines if an object
  ;; is a record of that type.
  ;; Additionally produce a type assertion which raises an
  ;; error if an object is not a record of that type.
  ;; The procedures are returned bundled in a list, since
  ;; Thcheme does not yet support define-values.
  (define (make-record-predicate+assertion descriptor)
    (let* ([pred (lambda (record)
                   (and (record? record)
                        (eq? (vector-ref record 1) descriptor)))]
           [assert (lambda (record)
                     (if (pred record)
                         'all-good
                         (error "invalid type to record function")))])
      (list pred assert)))

  ;; Generic record getters and setters given
  ;; a field index and a type assertion.
  (define (make-indexed-record-get i asserter)
    (lambda (r)
      (asserter r)
      (vector-ref r (+ i 2))))
  (define (make-indexed-record-set i asserter)
    (lambda (r v)
      (asserter r)
      (vector-set! r (+ i 2) v)))

  ;; Valid field-specs are (name getter-name)
  ;; and (name getter-name setter-name) only.
  (define (field-spec? spec)
    (and (list? spec)
         (let ([n (length spec)]) (and (>= n 2) (<= n 3)))
         (all symbol? spec)))
  
  ;; Normalized field specs always have 3 elements.
  ;; The third is #f if the field shouldn't have a setter.
  (define (normalize-field-spec spec)
    ;; spec -> (<name> <getter name> <modifier name or #f>)
    (cond [(not (field-spec? spec))
           (error "invalid field specifier")]
          [(null? (cddr spec))
           (append spec '(#f))]
          [else spec]))

  ;; Given a list of field specifiers,
  ;; produce an association list that maps field names
  ;; to (1) the field's index in the backing vector,
  ;; and (2) the getter and setter names.
  ;; All that data is bundled up in a list, so we end up with
  ;; a 4 element list for each spec. But intuitively it is an
  ;; association list.
  (define (process-field-specs specs)
    ;; [spec] -> assc:(<name> . (<index> <getter> <setter or #f>))
    (define (iota n)
      (let loop ([i 0])
        (if (= i n)
            ()
            (cons i (loop (+ i 1))))))
    (map* (lambda (i spec)
            (let ([normalized (normalize-field-spec spec)])
              (cons (first normalized)
                (cons i (cdr normalized)))))
          (iota (length specs))
          specs))

  ;; Validate and extract the constructor's name.
  (define (constructor-spec->name spec)
    (let ([name (first spec)])
      (if (symbol? name)
          name
          (error "invalid constructor name"))))

  ;; Given a constructor spec and the association list of processed field-specs,
  ;; transform the list of constructor argument names (which should match field
  ;; names) to a list of the backing indices of the matching fields,
  ;; in the same order.
  (define (constructor-spec->field-indices spec fields)
    ;; (<constr name> <field name> ...) -> (<field index> ...)
    (let ([args (cdr spec)])
      (and (all symbol? args)
           (define indices
             (map (lambda (arg)
                    (cond [(assq arg fields) => second]
                          [else 
                           (error "unknown field name in constructor specifier")]))
                  args))
           (all id indices)
           indices)))

  ;; Given the output of constructor-spec->field-indices,
  ;; create the equivalent of an <init> procedure in an OO language.
  ;; The resulting procedure takes an uninitialized record
  ;; and a list containing the arguments passed to the constructor,
  ;; then initializes the record according to the field-indices.
  ;; Finally, the record itself is returned.
  ;; Fields which are declared but not named in the constructor-spec
  ;; will remain uninitialized. Their values are unspecified.
  (define (field-indices->constructor-proc indices)
    (lambda (record args) ;; args already packaged,
                          ;; see specs+make->constructor-definition
      (let loop ([args args] [ixs indices])
        (if (null? args)
            record
            (begin (vector-set! record
                                (+ 2 (car ixs))
                                (car args))
                   (loop (cdr args) (cdr ixs)))))))

  ;; Given a single field spec and the type assertion for its type,
  ;; produce the code to define the field's getter and setter.
  (define (field-spec+asserter->definitions spec asserter)
    ;; (<name> <index> <getter> <setter or #f>) asserter
    ;; -> (begin <set getter> <set setter>?)
    (let* ([ix (cadr spec)]
           [getter-name (caddr spec)]
           [setter-name (cadddr spec)]
           [define-getter
             `(define ,getter-name ,(make-indexed-record-get ix asserter))]
           [define-setter
             (if setter-name
                 `(define ,setter-name ,(make-indexed-record-set ix asserter))
                 ;; double-quoted so that the object code is 'no-setter,
                 ;; which is a no-op to evaluate, instead of no-setter,
                 ;; which would raise an unknown symbol error.
                 ''no-setter)])
      `(begin ,define-getter ,define-setter)))

  ;; Produce the code to define all fields' getters and setters.
  (define (field-specs+asserter->definitions specs asserter)
    (map (lambda (spec)
           (field-spec+asserter->definitions spec asserter))
     specs))

  ;; Given the constructor spec, all field specs, and a procedure to make
  ;; an unitialized record, produce the code to define the constructor.
  (define (specs+make->constructor-definition constr-spec field-specs make)
    (let ([constr-name (constructor-spec->name constr-spec)]
          [indices (constructor-spec->field-indices
                     constr-spec
                     field-specs)])
      `(define ,constr-name
         (lambda ,(cdr constr-spec) ;; use the field names as the lambda's
                                    ;; argument list, which will be visible
                                    ;; at the prompt.
           (,(field-indices->constructor-proc indices)
             (,make) ;; take care to evaluate (make) when constructor is
                       ;; called, not when it is defined
             ,(cons 'list (cdr constr-spec)))))))

  ;; Finally, the define-record-type macro.
  ;; Takes a type name, constr-spec, predicate name, and field specs.
  ;; Produces a (somewhat nested) 'begin' form which defines all
  ;; of the relevant names. Typical output is something like
  ;; (define-record-type <pare> (kons x y) pare? (x kar set-kar!) (y kdr))
  ;; =>
  #| (begin (define <pare> '<pare>)
            (define pare? #<lamda (record) ...>)
            (define kons #<lambda (x y) ...>)
            (begin (define kar #<lambda (r) ...>)
                   (define set-kar! #<lambda (r v) ...>))
            (begin (define kdr #<lambda (r) ...>)
                   'no-setter)
            '<pare>)
  |#
  (defmacro real-define-record-type (name constr pred . orig-fields)
    (define token (make-record-type-descriptor name))
    (define pred+assert
      (make-record-predicate+assertion token))
    (define predicate (first pred+assert))
    (define assertion (second pred+assert))
    (define fields (process-field-specs orig-fields))
    (define num-fields (length fields))
    (define (make)
      (define r (make-vector (+ 2 num-fields) #f))
      (vector-set! r 0 record-token)
      (vector-set! r 1 token)
      r)
    (if (not (symbol? pred)) (error "invalid predicate name"))
            ;; bind the name to the (name) object in the token,
            ;; which is unique to this record type even if other
            ;; record types have the same name.
    `(begin (define ,name ',token)
            (define ,pred ,predicate)
            ,(specs+make->constructor-definition constr fields make)
            ,@(field-specs+asserter->definitions fields assertion)
            ',name))
  ;; export.
  (set! define-record-type real-define-record-type)
  
  ;; secondary exports
  (set! records:make-record-type-descriptor
    (lambda (name . _)
      (make-record-type-descriptor name)))
  (set! records:record-accessor
    (lambda (rtd k)
      (make-indexed-record-get k (lambda (_) #t))))
  (set! records:record-constructor
    (lambda (cd)
      (lambda args
        (apply vector record-token cd args))))
  (set! records:record-predicate
    (lambda (rtd)
      (car (make-record-predicate+assertion rtd)))))

  ; export for debugging
  ; (set! records:field-spec? field-spec?)
  ; (set! records:normalize-field-spec normalize-field-spec)
  ; (set! records:process-field-specs process-field-specs)

"loaded records.scm"
