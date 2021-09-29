#lang br/quicklang

;; stacker
;; this is our reader (converts source code os our lang from string of chars to s-exps)
;; reverse polish notation
;; a "little language" made for
;; a specialized purpose (a DSL)


;; more general notes:
;; with the reader, we're putting our program into the proper form
;; with the expander, we'll be giving meaning to these forms


;; read-syntax : path-to-src port -> syntax-obj
;; return code describing a module, packaged as a syntax object
;; module will invoke the expander
(define read-syntax
  (λ (path port)
    (let* ([src-lines (port->lines port)]
           [src-datums (format-datums '(handle ~a) src-lines)]
           [module-datum `(module stacker-mod "stacker.rkt"
                            ,@src-datums)]) ;; to insert multiple values
      (datum->syntax #f module-datum)))) ;; a module named lucy, w expander from the br lang, evaluates expr 42

(provide read-syntax)

;; new syntax: quasiquote and @:
(define xs (list 42 43 44))
;;`(41 ,xs 45) => '(41 (42 43 44) 45)
;; vs
;;`(41 ,@xs 45) => '(41 42 43 44 45)

(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...
     (display (car stack))))

(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)

;; ! syntax to let user know that it creates a side effect
(define pop-stack!
  (λ ()
    (let ([arg (car stack)])
      (begin (set! stack (cdr stack))
             arg))))

(define push-stack!
  (λ (arg)
    (set! stack (cons arg stack))))

(define (handle [arg #f])
  (cond
    [(number? arg) (push-stack! arg)]
    [(or (equal? + arg) (equal? * arg)) (let ([op-result (arg (pop-stack!) (pop-stack!))])
                                          (push-stack! op-result))]))
(provide handle)
(provide + *)