#lang racket/base

(provide ~> ~>>)

(require (for-syntax racket/base syntax/parse))

;; Clojure threading macros. Original versions courtesy of Asumu
;; Takikawa.
;;
;; Rewritten with help from Sam Tobin-Hochstadt to use whatever #%app
;; is bound in the lexical context of the macro usage. That way, these
;; will expand to the default Racket #%app, or to the applicative
;; dictionary #%app from app.rkt that's part of #lang rackjure, or to
;; whatever other #%app is bound at the macro usage site.
;;
;; Among other things this allows using `(require rackjure/threading)`
;; to get ~> and ~>> without the applicative dict overhead associated
;; with #lang rackjure.

;; The rewrite somewhat obfuscates the original logic. When reading
;; the code below, keep in mind that:
;;
;; - `op` means the ~> or ~>> used to invoke the macro. We need its
;;   lexical context!
;;
;; - Whenever the macro recursively expands to another pattern
;;   variation of itself, it is careful to use `op` to pass through
;;   and preserve the original lexical context. We need it!
;;
;; - On the terminal patterns, the macro needs to use the lexical
;;   context of `op` for "the parens" -- for the cons or list -- of
;;   the expanded form. For example, instead of a template like:
;;
;;       #'(f x a ...)
;;
;;   it must be:
;;
;;       (datum->syntax #'op (cons #'f #'(x a ...)))
;;
;;   This correctly handles both cases, where `e` is a function
;;   binding (a "function call") and where it is a transformer binding
;;   (a "macro call").
;;
;;   - If the form is a function application, the expander will use
;;     whatever #%app is bound in the lexical context where the macro
;;     is used.
;;
;;   - If the form uses a transformer binding (it is a "macro call"),
;;     the expander will handle it as usual. (An earlier version of
;;     this always injected an #%app, which prevented ~> and ~>>
;;     continuing to work with macros.)
;;
;; Rewritten yet again so that the implementation does not recursively
;; expand into itself, which would not compose well with other macros.
;; See: <http://dev.clojure.org/jira/browse/CLJ-1121>

(define-syntax (~> stx)
  (syntax-parse stx
    [(op x . forms)
     (quasisyntax/loc stx
       #,(let loop ([x #'x]
                    [forms #'forms])
           (syntax-parse forms
             ;; (~> x) => x
             [() x]
             ;; (~> x 's) => ((quote s) x) ...NOT (quote s x)
             [(((~literal quote) e) . more)
              (loop (datum->syntax #'op (list #'(quote e) x))
                    #'more)]
             ;; (~> x (f a ...)) => (f x a ...)
             [((f a ...) . more)
              (loop (datum->syntax #'op (cons #'f #`(#,x a ...)))
                    #'more)]
             ;; (~> x f) => (f x)
             [(f . more)
              (loop (datum->syntax #'op (list #'f x))
                    #'more)])))]))

(define-syntax (~>> stx)
  (syntax-parse stx
    [(op x . forms)
     (quasisyntax/loc stx
       #,(let loop ([x #'x]
                    [forms #'forms])
           (syntax-parse forms
             ;; (~>> x) => x
             [() x]
             ;; (~>> x 's) => ((quote s) x) ...NOT (quote s x)
             [(((~literal quote) e) . more)
              (loop (datum->syntax #'op (list #'(quote e) x))
                    #'more)]
             ;; (~>> x (f a ...)) => (f a ... x)
             [((f a ...) . more)
              (loop (datum->syntax #'op (cons #'f #`(a ... #,x)))
                    #'more)]
             ;; (~>> x f) => (f x)
             [(f . more)
              (loop (datum->syntax #'op (list #'f x))
                    #'more)])))]))

(module* test racket/base
  (require (submod ".."))
  (require rackunit
           (only-in racket/string string-split string-replace))
  (check-equal? (~> "a b c d"
                    string-upcase
                    (string-replace "A" "X")
                    (string-split " ")
                    car)
                "X")
  (check-equal? (~>> 5 (+ 3) (/ 2) (- 1))
                (/ 3 4))
  (check-equal? (~>> 1 add1)
                2)
  (check-equal? (~>> 1 + (~>> 1 +)) ;; see CLJ-1121
                2)
  ;; Confirm expansion using default #%app
  (module plain racket/base
    (require rackunit)
    (require (submod ".." "..")) ;; for ~>
    (check-equal? (~> 1 (+ 1) (* 2) (+ 1))
                  5)
    (let ([d (hasheq 'a 42)])
      ;; these should NOT work without custom #%app
      (check-exn exn:fail? (lambda () (d 'a)))
      (check-exn exn:fail? (lambda () (~> 'a d)))
      (check-exn exn:fail? (lambda () ('a d)))
      (check-exn exn:fail? (lambda () (~> d 'a)))))
  (require 'plain)
  ;; Confirm expansion using applicative dict #%app
  (module dict racket/base
    (require rackunit)
    (require (submod ".." "..")) ;; for ~>
    (require (rename-in "app.rkt" [-#%app #%app]))
    (check-equal? (~> 1 (+ 1) (* 2) (+ 1))
                  5)
    ;; these SHOULD work with custom #%app
    (let ([d (hasheq 'a 42)])
      (check-equal? (d 'a) 42)
      (check-equal? (~> 'a d) 42)
      (check-equal? ('a d) 42)
      (check-equal? (~> d 'a) 42))
    ;; Nested using ~> (threading macro)
    (check-equal? (~> (hasheq 'a (hasheq 'b (hasheq 'c 42)))
                      'a 'b 'c)
                  42))
  (require 'dict)
  ;; Confirm still works with syntax forms as well as function #%app.
  (check-true (~> #t (if #t #f))))
