#lang racket
(require
 parser-tools/lex
 parser-tools/yacc
 racket/list
 (prefix-in : parser-tools/lex-sre))
(module+ test
  (require rackunit))

(provide parse-string)

#|
Accepted Grammar:

Expression := <Variable>
            | <Number>
            | <Number><Variable>
            | <Expression><Operator><Expression>
            | <Function>(<Expression,...)
            | <Number><Function>(<Expression,...)
            | <Expression>(Expression)
            | <Single><Expression>

Variable: <Alpha>
Operator: <Alpha>...+
Single:   + | -
Number:   Same as racket
|#

(define (parse-string s)
  (define p (open-input-string s))
  (cal-parser (λ () (cal-lexer p))))

(define-lex-abbrevs
  (letter (:/ "a" "z"))
  (operator (:or "-" "+" "^" "*" "/"))
  (digit (:/ "0" "9")))
(define-tokens values (NUM VAR FUNC OPERATOR))
(define-empty-tokens prog (OP CP END COMMA))

(define cal-lexer
  (lexer
   [(eof) (token-END)]
   [(:or #\tab #\space) (cal-lexer input-port)]
   [(:: (:+ digit) (:? (:: "." (:+ digit)))) (token-NUM (string->number lexeme))]
   [operator (token-OPERATOR (string->symbol lexeme))] 
   ["(" 'OP]
   [")" 'CP]
   ["," 'COMMA]
   [letter (token-VAR (string->symbol lexeme))]
   [(:: letter (:+ letter)) (token-FUNC (string->symbol lexeme))]))

(define cal-parser
  (parser
   (tokens values prog)
   (end END)
   (error (lambda args (map displayln args) (error 'parse "bad syntax")))
   (start expr)
   (precs
    (left OPERATOR)
    (left OP CP))
   (grammar
    (expr
     [(NUM) $1]
     [(VAR) $1]
     [(NUM VAR) `(* ,$1 ,$2)]
     [(expr OPERATOR expr) `(,$2 ,$1 ,$3)]
     [(function) $1]
     [(NUM function) `(* ,$1 ,$2)]
     [(expr OP expr CP) `(* ,$1 ,$3)]
     [(OP expr CP) $2]
     [(OPERATOR expr) `(,$1 ,$2)])
    (function [(FUNC OP args-list CP) `(,$1 ,@$3)])
    (args-list
     [(expr) (list $1)]
     [(expr COMMA args-list) (cons $1 $3)]))))

(module+ test
  (define-syntax-rule (test t r) (check-equal? (parse-string t) r))
  (test "1" 1)
  (test "2.6" 2.6)
  (test "x" 'x)
  (test "6x" '(* 6 x))
  (test "sin(y)" '(sin y))
  (test "garbage(y,2)" '(garbage y 2))
  (test "4(3)" '(* 4 3))
  (test "4sin(x)" '(* 4 (sin x)))
  (test "-2" '(- 2))
  (test "sin(-9)" '(sin (- 9)))
  (test "3+5" '(+ 3 5))
  (test "3-x" '(- 3 x))
  (test "(3- x)" '(- 3 x))
  (test "3^2" '(^ 3 2))
  (test "3^2/2" '(/ (^ 3 2) 2))
  (test "(3-x)+(sin(4) + 6)" '(+ (- 3 x) (+ (sin 4) 6))))