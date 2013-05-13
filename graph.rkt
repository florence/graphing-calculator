#lang racket
(require "parser.rkt" racket/sandbox plot)

(define evaluate (make-evaluator 'racket/base))

(define (graph2d str min max)
  (define expr (parse-string str))
  (define vars (free-vars expr))
  (plot (function (evaluate `(λ ,vars ,expr)) min max #:label str)))

(define (free-vars x)
  (cond [(and (symbol? x) (= 1 (string-length (symbol->string x))))
                 (list x)]
        [(list? x) (append-map free-vars (free-vars x))]
        [else null]))
  