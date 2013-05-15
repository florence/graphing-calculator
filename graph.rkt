#lang racket
(require "parser.rkt" racket/sandbox plot)
(provide graph2d)

(define evaluate (make-evaluator 'racket/base 
                   '(define ^ expt)
                   '(define arctan atan)
                   '(define arcsin asin)
                   '(define arccos acos)
                   '(define e #i2.718281828459045)))

(define (graph2d str min max [invert? #f])
  (define expr (parse-string str))
  (plot (list ((if invert? inverse function) 
               (evaluate `(λ (x) ,expr))
               min max #:label str)
              (axes))))