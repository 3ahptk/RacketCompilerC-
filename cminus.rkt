#lang racket
;; NOTE: THIS DOES NOT YET HAVE THE FULL C- GRAMMAR!!!!!
;;       THIS IS AN EVOLUTION OF THE CALCULATOR CODE.

;; Import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; -------------------
;; Zero Page Locations
;; -------------------
;; Math Variables
(define MATH1LO #x10) ;; augend, subtrahend
(define MATH1HI #x11) 
(define MATH2LO #x12) ;; addend, minuend
(define MATH2HI #x13)
(define MATH3LO #x14) ;; sum, difference
(define MATH3HI #x15)
(define MATH4LO #x16)
(define MATH4HI #x17)
(define MATH5LO #x18)
(define MATH5HI #x19)
(define MATH6LO #x1A)
(define MATH6HI #x1B)
(define MATH7LO #x1C)
(define MATH7HI #x1D)
(define MATH8LO #x1E)
(define MATH8HI #x1F)

;; Work Space
(define WORK1LO #x20)
(define WORK1HI #x21)

;; Symbol Table
(define VARS #xE0)

;; Memory Mapped I/O
(define IOFROB #xFD)
(define IOINTL #xFE)
(define IOINTH #xFF)

;; ---------------
;; ROM Subroutines
;; ---------------
(define ADD #xF000)
(define SUBTRACT #xF00E)

(define-tokens value-tokens (NUM VAR FNCT))
(define-empty-tokens op-tokens (newline = OP CP + - * / ^ EOF NEG))

;; A hash table to store variable values in for the calculator
(define vars (make-hash))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.
  ;; But (:/ #\0 #\9) is ok.
  (digit (:/ "0" "9")))

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
   ["(" 'OP]
   [")" 'CP]
   ["sin" (token-FNCT sin)]
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
           [(exp) $1])    
    (exp [(NUM) (let-values ([(lo hi hexLo hexHi) (int->16bit $1)])
                  ;; Assembly Code
                  ;; Push number onto stack
                  (printf "LDA #$~a~n" hexHi)
                  (printf "PHA~n")
                  (printf "LDA #$~a~n" hexLo)
                  (printf "PHA~n")
                  $1)
                ]
         [(VAR) (begin
                  ;; Assembly Code
                  ;; Retrieve variable value from Symbol Table
                  ;; Push value onto Stack
                  (printf "LDX #$~a~n" (symbol->var-lookup $1))
                  (printf "INX~n")
                  (printf "INX~n")
                  (printf "LDA *$~a,X~n" (8bit->hex VARS))
                  (printf "PHA~n")
                  (printf "DEX~n")
                  (printf "LDA *$~a,X~n" (8bit->hex VARS))
                  (printf "PHA~n")
                  (hash-ref vars $1 (lambda () 0)))]
         [(VAR = exp) (let-values ([(lo hi hexLo hexHi) (int->16bit $3)])
                        ;; Assembly Code
                        ;; Pull expression off Stack
                        (printf "PLA~n")
                        (printf "STA *$~a~n" (8bit->hex WORK1LO))
                        (printf "PLA~n")
                        (printf "STA *$~a~n" (8bit->hex WORK1HI))
                        ;; Lookup variable location in Symbol Table
                        ;; Store expression value in variable
                        ;; Push value onto Stack
                        (printf "LDX #$~a~n" (symbol->var-lookup $1))
                        (printf "INX~n")
                        (printf "INX~n")
                        (printf "LDA *$~a~n" (8bit->hex WORK1HI))
                        (printf "STA *$~a,X~n" (8bit->hex VARS))
                        (printf "PHA~n")
                        (printf "DEX~n")
                        (printf "LDA *$~a~n" (8bit->hex WORK1LO))
                        (printf "STA *$~a,X~n" (8bit->hex VARS))
                        (printf "PHA~n")
                        (hash-set! vars $1 $3)
                        $3)]
         [(FNCT OP exp CP) ($1 $3)]
         [(exp + exp) (let-values ([(lo hi hexLo hexHi) (int->16bit ADD)])
                        ;; Assembly Code
                        ;; Pull Right-hand expression off Stack
                        ;; Store in Addend workspace
                        (printf "PLA~n")
                        (printf "STA *$~a~n" (8bit->hex MATH2LO))
                        (printf "PLA~n")
                        (printf "STA *$~a~n" (8bit->hex MATH2HI))
                        ;; Pull Left-hand expression off Stack
                        ;; Store in Augend workspace
                        (printf "PLA~n")
                        (printf "STA *$~a~n" (8bit->hex MATH1LO))
                        (printf "PLA~n")
                        (printf "STA *$~a~n" (8bit->hex MATH1HI))
                        ;; Call ADD subroutine
                        (printf "JSR $~a~a~n" hexHi hexLo)
                        ;; Push sum onto Stack
                        (printf "LDA *$~a~n" (8bit->hex MATH3HI))
                        (printf "PHA~n")
                        (printf "LDA *$~a~n" (8bit->hex MATH3LO))
                        (printf "PHA~n")
                        (+ $1 $3))
                      ]
         [(exp - exp) (let-values ([(lo hi hexLo hexHi) (int->16bit SUBTRACT)])
                        ;; Assembly Code
                        ;; Pull Right-hand expression off Stack
                        ;; Store in Minuend workspace
                        (printf "PLA~n")
                        (printf "STA *$~a~n" (8bit->hex MATH2LO))
                        (printf "PLA~n")
                        (printf "STA *$~a~n" (8bit->hex MATH2HI))
                        ;; Pull Left-hand expression off Stack
                        ;; Store in Subtrahend workspace
                        (printf "PLA~n")
                        (printf "STA *$~a~n" (8bit->hex MATH1LO))
                        (printf "PLA~n")
                        (printf "STA *$~a~n" (8bit->hex MATH1HI))
                        ;; Call SUBTRACT subroutine
                        (printf "JSR $~a~a~n" hexHi hexLo)
                        ;; Push difference onto Stack
                        (printf "LDA *$~a~n" (8bit->hex MATH3HI))
                        (printf "PHA~n")
                        (printf "LDA *$~a~n" (8bit->hex MATH3LO))
                        (printf "PHA~n")
                        (- $1 $3))
                      ]
         [(exp * exp) (* $1 $3)]
         [(exp / exp) (/ $1 $3)]
         [(- exp) (prec NEG) (- $2)]
         [(exp ^ exp) (expt $1 $3)]
         [(OP exp CP) $2]))))

;; -------------------------------
;; DECIMAL / BINARY NUMBER FORMATS
;; -------------------------------
;; Convert an integer to the 6502 Lo/Hi-byte 16-bit Integer format.
;; Returns bytes and integers and hex strings.
(define (int->16bit arg)
  (let ((i (bitwise-and arg #xFFFF))
        (lo 0)
        (hi 0))
    (when (< i 0) 
      ;; Use Two's Compliment for negative numbers
      (set! i (+ (bitwise-xor i #xFFFF) 1)))
    (set! lo (bitwise-and i #x00FF))
    (set! hi (/ (bitwise-and i #xFF00) 256))
    (values lo hi 
            (8bit->hex lo)
            (8bit->hex hi))))

;; Convert 6502 Lo/Hi-bytes to an integer.
(define (16bit->int lo hi)
  (let ((i 0))
    (cond ((> (bitwise-and hi #x80) 0) 
           ;; Use Two's Compliment for negative numbers
           (set! i (- (+ (* (bitwise-xor hi #xFF) 256) (bitwise-xor lo #xFF) 1))))
          (#t (set! i (+ (* hi 256) lo))))
    i))

;; Converts an 8-bit integer (< 256) to a 2-digit hex string.
(define (8bit->hex number)
  (let ((padded (string-upcase (string-append "0" (number->string number 16)))))
    (substring padded (- (string-length padded) 2))))

;; Do the math locally to calculate index of variable in Symbol Table
(define (symbol->var-lookup var)
  (*
   (- (bytes-ref (string->bytes/utf-8 (string-upcase (symbol->string var))) 0)
      65)
   3))

;; run the calculator on the given input-port       
(define (calc ip)
  (display "*=$D000")
  (newline)
  (port-count-lines! ip)
  (letrec ((one-line
            (lambda ()
              (let ((result (calcp (lambda () (calcl ip)))))
                (when result
                  ;; Assembly Code
                  ;; EOL pull last value off stack and print result
                  (printf "PLA~n")
                  (printf "STA *$~a~n" (8bit->hex IOINTL))
                  (printf "PLA~n")
                  (printf "STA *$~a~n" (8bit->hex IOINTH))
                  (printf "STA *$~a~n" (8bit->hex IOFROB))
                  ;; Scheme result
                  ;; (printf "~a\n" result)
                  (one-line))))))
    (one-line))
  (display "HLT") ;; Hex 02
  (newline)
  (display ".end")
  (newline)
  )

;; Some examples
(calc (open-input-string "a=1\nb=1234\na + (b - 20)"))
