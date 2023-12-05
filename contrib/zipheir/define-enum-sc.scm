(import (rnrs)
        (rnrs syntax-case (6))
        (only (srfi :1) find iota))

(define-syntax define-enum
  (lambda (stx)
    (define (parse-name-val nv-syn)
      (syntax-case nv-syn ()
        (id (identifier? #'id) #'id)
        ((id _) (identifier? #'id) #'id)
        (_ (syntax-violation 'define-enum
            "invalid enum syntax" stx nv-syn))))

    (define (unique-ids? ids)
      (let unique ((ids ids))
        (or (null? ids)
            (let ((id (car ids)) (rest (cdr ids)))
              (and (not (find (lambda (x) (free-identifier=? x id))
                              rest))
                   (unique rest))))))

    (syntax-case stx ()
      ((_ type-name (name-val ...) constructor)
       (and (identifier? #'type-name)
            (identifier? #'constructor))
       (with-syntax (((name ...) (map parse-name-val #'(name-val ...)))
                     ((idx ...) (iota (length #'(name-val ...)))))
         (unless (unique-ids? #'(name ...))
           (syntax-violation 'define-enum
             "duplicated enum names" stx #'(quote (name ...))))
         (syntax
          (begin
           (define new-type (make-enum-type '(name-val ...)))

           ;; Helper
           (define-syntax enum-name-to-ordinal-syn
             (syntax-rules (name ...)
               ((_ loc name) idx) ...
               ((_ loc x)
                (syntax-violation 'loc "invalid enum name" 'x))))

           (define-syntax type-name
             (syntax-rules ()
               ((_ (x . _))
                (syntax-violation 'type-name "invalid syntax" 'x))
               ((_ id)
                (%enum-ordinal->enum-no-assert
                 new-type
                 (enum-name-to-ordinal-syn type-name id)))))

           (...  ; escape ellipsis for the following
            (define-syntax constructor
              (lambda (stx)
                (syntax-case stx ()
                  ((_ arg ...)
                   (every identifier? #'(arg ...))
                   (syntax
                    (let ((vec (make-bitvector (enum-type-size new-type)
                                               #f)))
                      ;; Unroll for-each loop
                      (bitvector-set!
                       vec
                       (enum-name-to-ordinal-syn constructor arg)
                       #t) ...
                      (make-enum-set new-type vec)))))))))))))))
