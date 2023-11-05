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
                     ((i ...) (iota (length #'(name-val ...)))))
         (unless (unique-ids? #'(name ...))
           (syntax-violation 'define-enum
             "duplicated enum names" stx #'(name ...)))
         (syntax
          (begin
           (define new-type (make-enum-type '(name-val ...)))

           (define-syntax type-name
             (syntax-rules (name ...)
               ((_ name)
                (%enum-ordinal->enum-no-assert new-type i)) ...
               ((_ (x . _))
                (syntax-violation 'type-name "invalid syntax" x))
               ((_ id)
                (syntax-violation 'type-name "invalid enum name" id))))
          )))))))
