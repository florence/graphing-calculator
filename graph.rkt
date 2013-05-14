#lang racket
(require "parser.rkt" racket/sandbox plot)
(provide graph2d)

(define evaluate (make-evaluator 'racket/base '(define ^ expt)))

(define (graph2d str min max)
  (define expr (parse-string str))
  (define vars (let ([f (free-vars expr)]) (if (null? f) '(_) (list (first f)))))
  (plot (list (function (evaluate `(λ ,vars ,expr)) min max #:label str)
              (axes))))

(define (free-vars x)
  (remove-duplicates
   (cond [(and (symbol? x) (= 1 (string-length (symbol->string x))))
          (list x)]
         [(list? x) (append-map free-vars (rest x))]
         [else null])))
  