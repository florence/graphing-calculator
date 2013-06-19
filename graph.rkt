#lang racket
(require "parser.rkt" racket/sandbox plot (only-in racket/snip image-snip%))
(provide 
 (contract-out
  [graph2d (grapher/c 1)]
  [parametric2d (grapher/c 2)]
  [polar2d (grapher/c 1)]))

(define (grapher/c l)
  (->i ([min real?] [max real?] [invert? boolean?])
       #:rest [strs (listof string?)]
       #:pre/name (strs) "wrong number of functions given" (= (length strs) l)
       #:pre/name (min max) "min and max out of range" (> max min)
       [_ (is-a?/c image-snip%)]))

(define evaluate (make-evaluator 'racket/base 
                   '(define ^ expt)
                   '(define arctan atan)
                   '(define arcsin asin)
                   '(define arccos acos)
                   '(define (E number exp) (* number (^ 10 exp)))
                   '(define e   #i2.718281828459045)
                   '(define pi  #i3.14159)
                   '(define phi #i1.61803398875)
                   '(define g   #i9.81)
                   ;; the - prevents the input from calling it
                   ;; -guard helps when graphing functions that approach inifinity,
                   ;; in graph modes that expect only reals
                   '(define (-guard v) (if (real? v) v +nan.0))))

(define ((grapher builder normal inverse) min max invert? . str)
  (define exprs (map parse-string str))
  (plot (list ((if invert? inverse normal)
               (evaluate (apply builder exprs))
               min max #:label (~a str #:separator ","))
              (axes))))

(define graph2d (grapher (λ (e) `(λ (x) (-guard ,e))) function inverse))

(define (iparametric f)
  (parametric (λ (t) (reverse (f t)))))
(define parametric2d 
  (grapher (λ (e1 e2) `(λ (t) (list (-guard ,e1) (-guard ,e2))))
           parametric
           iparametric))

(define polar2d (grapher (λ (e) `(λ (t) (-guard ,e))) polar polar)) 
  