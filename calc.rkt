#lang racket
;; An interactive calculator inspired by the calculator example in the bison manual.

;; Import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM VAR FNCT))
(define-empty-tokens op-tokens (newline = OP CP + - * / ^ EOF NEG begin end token-newline))

;; A hash table to store variable values in for the calculator
(define vars (make-hash))

(define-lex-abbrevs
 (lower-letter (:/ "a" "z"))
 (upper-letter (:/ #\A #\Z))
 ;; (:/ 0 9) would not work because the lexer does not understand numbers.
 ;; But (:/ #\0 #\9) is ok.
 (digit (:/ "0" "9")))

(define (cube3 x)
  (* x x x));syntactic sugar
 
(define calcl
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.
   ;; Returning the result of that operation.  
   ;; This effectively skips all whitespace.
   [(:or #\tab #\space) (calcl input-port)]
   ;; (token-newline) returns 'newline
   [#\newline (token-newline)]
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "=" "+" "-" "*" "/" "^") (string->symbol lexeme)]
   ["**" '^]
   ["(" 'OP]
   [")" 'CP]
   ["{" 'begin]
   ["}" 'end]
   ["sin" (token-FNCT sin)]
   ["cos" (token-FNCT cos)]
   ["tan" (token-FNCT tan)]
   ["cube" (token-FNCT cube3)]
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))
   
(define calcp
  (parser
   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))
   (precs (right =)
          (left - +)
          (left * /)
          (left NEG)
          (right ^))
   (grammar
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(begin prog) (list 'let '() $2)])
    (prog [(prog exp token-newline end) $2]
          [(exp token-newline) $1]
          [(end) #f])
    (exp [(NUM) $1]
         [(VAR) ;(hash-ref vars $1 (lambda () 0))
                $1]
         [(VAR = exp) ;(begin (hash-set! vars $1 $3)
                      ;      $3)
                      (list 'set! $1 $3)]
         [(FNCT OP exp CP) ($1 $3)]
         [(exp + exp) (+ $1 $3)]
         [(exp - exp) (- $1 $3)]
         [(exp * exp) (* $1 $3)]
         [(exp / exp) (/ $1 $3)]
         [(- exp) (prec NEG) (- $2)]
         [(exp ^ exp) (expt $1 $3)]
         [(OP exp CP) $2]))))
           
;; run the calculator on the given input-port       
(define (calc ip)
  (port-count-lines! ip)
  (letrec ((one-line
	    (lambda ()
	      (let ((result (calcp (λ() (calcl ip)))))
		(when result
                  (printf "~a\n" result)
                  (one-line))))))
    (one-line)))

;; Some examples
(calc (open-input-string "{ x=1\n \n(x + 2 * 3) - (1+2)*3 }"))
;(calc (open-input-string "x"))
;(calc (open-input-string "y=2+(x=5)"))
;(calc (open-input-string "x"))
;(calc (open-input-string "sin(90)"))
;(calc (open-input-string "cos(90)"))
;(calc (open-input-string "tan(90)"))
;(calc (open-input-string "cube(3)"))
;(calc (open-input-string "2**8"))
;(calc (open-input-string "2^8"))
