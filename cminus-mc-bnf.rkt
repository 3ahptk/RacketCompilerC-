#lang racket
;; NOTE: THIS DOES NOT YET HAVE THE FULL C- GRAMMAR!!!!!
;;       THIS IS AN EVOLUTION OF THE CALCULATOR CODE.

;; Thanks to John Thomas for suggesting loop mechanism

;; Import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; -------------------
;; Zero Page Locations
;; -------------------
;; Math Variables
(define MATH1LO #x10) ;; augend, subtrahend, multiplier, divisor
(define MATH1HI #x11) ;; augend, subtrahend, multiplier, divisor
(define MATH2LO #x12) ;; addend, minuend, multiplicand, dividend
(define MATH2HI #x13) ;; addend, minuend, multiplicand, dividend
(define MATH3LO #x14) ;; sum, difference, product, quotient
(define MATH3HI #x15) ;; sum, difference, product, quotient
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
(define RETURN1LO #x22)
(define RETURN1HI #x23)

;; Symbol Table
(define SYMBOL-TABLE #x30) ;$30 - $7F

;; Memory Mapped I/O
(define IOFROB #xFD)
(define IOINTL #xFE)
(define IOINTH #xFF)

;; ---------------
;; ROM Subroutines
;; ---------------
(define ADD #xF000)
(define SUBTRACT #xF00E)
(define MULTIPLY #xE000)
(define DIVISION #xE050)

;; A hash table to store variable values in for the calculator
(define vars (make-hash))

;A hash table to store the locations of blocks of code 
(define functions (make-hash))

;; Where to start running the code
(define ENTRYPOINT #xD000)

;; Program Counter
(define program-counter ENTRYPOINT)

(define (format-code . args)
  (begin 
    (set! program-counter (+ (car args) program-counter))
    (if (= 0 (length (cdr (cdr args))))
        (format (car (cdr args)))
        (apply format (car (cdr args)) (cdr (cdr args)))
        )))

;; Tokens
(define-tokens value-tokens (NUM VAR-ID FNCT))
(define-empty-tokens op-tokens (= + SEMICOLON COMMA OB CB OP CP EQUIVALENCE NOTEQUALS WHILE INT VOID EOF NEG))

;; LEXER SHORTHAND
(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.
  ;; But (:/ #\0 #\9) is ok.
  (digit (:/ "0" "9")))

;; LEXER
(define lex 
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.
   ;; Returning the result of that operation.  
   ;; This effectively skips all whitespace.
   [(:or #\tab #\space #\newline) (lex input-port)]
   [(:or "=" "+" ) (string->symbol lexeme)]
   ["void" 'VOID]
   ["int" 'INT]
   ["while" 'WHILE]
   ["==" 'EQUIVALENCE]
   ["!=" 'NOTEQUALS]
   ["," 'COMMA]
   [";" 'SEMICOLON]
   ["(" 'OP]
   [")" 'CP]
   ["{" 'OB]
   ["}" 'CB]
   [(:= 1 (:or lower-letter upper-letter)) (token-VAR-ID (string->symbol lexeme))]
   [(:+ (:or lower-letter upper-letter)) (token-FNCT (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]
   )
  )

;; PARSER
(define parse
  (parser
   (start program)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (printf "~n~a ~a ~a~n" a b c)))
   (precs 
    (right SEMICOLON)
    (right =)
    (left +))
   (grammar
    ;; USE GRAMMAR FROM C- SPEC
    ;; This is a partial implementation of that grammar
    (program [() #f]
             ;; If there is an error, ignore everything before the error
             ;; and try to start over right after the error
             [(error program) $2]
             [(DECLARATION-LIST) (printf "~a" $1)])
    
    (DECLARATION-LIST
     [(DECLARATION-LIST DECLARATION) (string-append $1 $2)]
     [(DECLARATION) $1])    
    
    (DECLARATION
     [(VAR-DECLARATION) $1]
     [(FUN-DECLARATION) $1])
    
    ;; Declare variables. E.g.: int foo;
    ;; No inline initialization
    (VAR-DECLARATION
     [(INT VAR-ID SEMICOLON) 
      (begin ;stores the address of a variable on the heap and incs the heap pointer
        (hash-set! vars $2 SYMBOL-TABLE) 
        (set! SYMBOL-TABLE (+ SYMBOL-TABLE #x02)) "")] )
    
    ;; Only two types
    (TYPE-SPECIFIER 
     [(INT) 'INT] 
     [(VOID) 'VOID])
    
    ;; Function Declaration
    (FUN-DECLARATION 
     [(TYPE-SPECIFIER FNCT OP CP COMPOUND-STMT) 
      (cond ((string=? (symbol->string $2) "main")
          (begin
            (set! ENTRYPOINT (- program-counter (/ (string-length $5) 3)))
            $5))
          ;; else
          ;; define code for non-main functions
          )
      ])  
    
    (COMPOUND-STMT
     [(OB STATEMENT-LIST CB) $2])
    
    (STATEMENT-LIST
     [(STATEMENT-LIST STATEMENT)  (string-append $1 $2)]
     [() ""])    
    
    (STATEMENT
     [(EXPRESSION-STMT) $1]
     [(COMPOUND-STMT) $1]
     [(ITERATION-STMT) $1]
     )    
    
    (EXPRESSION-STMT
     [(EXPRESSION SEMICOLON) (string-append $1 (format-code 2 "68 68 "))]
     [(SEMICOLON) ""]) 
    
    (ITERATION-STMT
     ;; While Loops
     [(WHILE OP EXPRESSION CP STATEMENT)
      (let-values ([(lo hi hexLo hexHi) (int->16bit (- program-counter (/ (string-length $5) 3) (/ (string-length $3) 3)))])
        (string-append
         $3
         (format-code 6 "68 85 ~a 68 85 ~a " (8bit->hex WORK1LO) (8bit->hex WORK1HI));
         (format-code 2 "A5 ~a " (8bit->hex WORK1LO)) ;lda worklo 
         (format-code 2 "D0 05 ") ;beq to start of stmt
         (format-code 2 "A5 ~a " (8bit->hex WORK1HI)) ;lda workhi
         (let-values ([(lo hi hexLo hexHi) (int->16bit (+ program-counter 6))])
           (format-code 3 "4C ~a ~a " hexLo hexHi) ;jmp past stmt
           )
         $5
         (format-code 3 "4C ~a ~a " hexLo hexHi);jmp to $3 
         )
        )
      ])  
    
    (EXPRESSION
     [(VAR = EXPRESSION) 
      (string-append (car $1) $3 
                     ;; Assembly Code
                     ;; Pull right-hand expression off Stack
                     (format-code 6 "68 85 ~a 68 85 ~a " 
                                  (8bit->hex WORK1LO)
                                  (8bit->hex WORK1HI))
                     ;; Lookup variable location in Symbol Table
                     ;; Store expression value in variable
                     ;; Push value onto Stack
                     (format-code 2 "A5 ~a "(8bit->hex WORK1HI));lda workhi
                     (format-code 1 "48 ");pha
                     (format-code 2 "A2 01 ") ;ldx 01
                     (format-code 3 "9D ~a ~a " (8bit->hex (symbol->var-lookup (cdr $1))) (8bit->hex SYMBOL-TABLE));sta into variable hi
                     
                     (format-code 2 "A5 ~a "(8bit->hex WORK1LO));lda worklo
                     (format-code 1 "48 ");pha
                     (format-code 3 "8D ~a ~a " (8bit->hex (symbol->var-lookup (cdr $1))) (8bit->hex SYMBOL-TABLE));sta into variable lo
                     )
      ] 
     
     [(SIMPLE-EXPRESSION) $1]
     )
    
    (VAR
     [(VAR-ID) 
      (begin
        (hash-ref vars $1 (lambda () 0))
        (cons (string-append
               ;; Assembly Code
               ;; Retrieve variable address from Symbol Table
               ;; Push value onto Stack
               (format-code 2 "A2 01 ") ;ldx 01
               (format-code 3 "BD ~a ~a " (8bit->hex (symbol->var-lookup $1)) (8bit->hex SYMBOL-TABLE)) ; lda X var address
               (format-code 1 "48 ") ;pha hi value
               (format-code 3 "AD ~a ~a " (8bit->hex (symbol->var-lookup $1)) (8bit->hex SYMBOL-TABLE));; lda var address
               (format-code 1 "48 ") ;pha lo value
               )
              $1)
        )]
     )  
    
    (SIMPLE-EXPRESSION
     [(ADDITIVE-EXPRESSION RELOP ADDITIVE-EXPRESSION) 
      (cond
        [(equal? $2 'EQUIVALENCE)
         (string-append $1 $3
                        ;; Assembly Code
                        ;; Pull Right-hand expression off Stack
                        (format-code 6 "68 85 ~a 68 85 ~a " 
                                     (8bit->hex MATH2LO)
                                     (8bit->hex MATH2HI))
                        ;; Pull Left-hand expression off Stack
                        (format-code 6 "68 85 ~a 68 85 ~a " 
                                     (8bit->hex MATH1LO)
                                     (8bit->hex MATH1HI))
                        (format-code 2 "A9 00 ") ;lda 0
                        (format-code 1 "48 ") ;pha
                        (format-code 2 "A5 ~a " (8bit->hex MATH1LO));lda math1o
                        (format-code 2 "C5 ~a " (8bit->hex MATH2LO));cmp math2lo
                        (format-code 2 "D0 0A ") ;bne false
                        (format-code 2 "A5 ~a " (8bit->hex MATH1HI)) ;lda math1hi
                        (format-code 2 "C5 ~a " (8bit->hex MATH2HI)) ;cmp math2hi
                        (format-code 1 "08 ") ;php
                        (format-code 2 "A9 01 ") ;lda 01
                        (format-code 1 "28 ") ;plp
                        (format-code 2 "F0 02 ") ;beq done
                        ;false
                        (format-code 2 "A9 00 ") ;lda 0
                        ;; Push answer onto Stack
                        ;done:
                        (format-code 1 "48 ") ;pha
                        )
         ]
        [(equal? $2 'NOTEQUALS)
         (string-append $1 $3
           ;; Assembly Code
           ;; Pull Right-hand expression off Stack
           (format-code 6 "68 85 ~a 68 85 ~a " 
                   (8bit->hex MATH2LO)
                   (8bit->hex MATH2HI))
           ;; Pull Left-hand expression off Stack
           (format-code 6 "68 85 ~a 68 85 ~a " 
                   (8bit->hex MATH1LO)
                   (8bit->hex MATH1HI))
           
           (format-code 2 "A9 00 ") ;lda 0
           (format-code 1 "48 ") ;pha
           (format-code 2 "A5 ~a " (8bit->hex MATH1LO));lda math1o
           (format-code 2 "C5 ~a " (8bit->hex MATH2LO));cmp math2lo
           (format-code 2 "D0 06 ") ;bne true
           (format-code 2 "A5 ~a " (8bit->hex MATH1HI)) ;lda math1hi
           (format-code 2 "C5 ~a " (8bit->hex MATH2HI)) ;cmp math2hi
           (format-code 2 "F0 04 ") ;beq false
           ;true
           (format-code 2 "A9 01 ") ;lda 01
           (format-code 2 "D0 02 ");bne done
           
           ;false
           (format-code 2 "A9 00 ") ;lda 00

           ;done:
           (format-code 1 "48 ") ;pha
           )
         ]
        )                                            
      ]
     
     [(ADDITIVE-EXPRESSION) $1])
    
    (RELOP
     [(EQUIVALENCE) 'EQUIVALENCE] ;; ==
     [(NOTEQUALS) 'NOTEQUALS]) ;; !=
    
    (ADDITIVE-EXPRESSION
     [(ADDITIVE-EXPRESSION ADDOP TERM) 
      (cond
        ;; Only Addition is defined
        [(equal? $2 '+) 
         (let-values ([(lo hi hexLo hexHi) (int->16bit ADD)])
           (string-append $1 $3
                          ;; Assembly Code
                          ;; Pull Right-hand expression off Stack
                          ;; Store in Addend workspace
                          (format-code 6 "68 85 ~a 68 85 ~a " 
                                       (8bit->hex MATH2LO)
                                       (8bit->hex MATH2HI))
                          ;; Pull Left-hand expression off Stack
                          ;; Store in Augend workspace
                          (format-code 6 "68 85 ~a 68 85 ~a " 
                                       (8bit->hex MATH1LO)
                                       (8bit->hex MATH1HI))
                          ;; Call ADD subroutine
                          (format-code 3 "20 ~a ~a " hexLo hexHi) ;; <==== byte reversal!!!
                          ;; Push sum onto Stack
                          (format-code 6 "A5 ~a 48 A5 ~a 48 " 
                                       (8bit->hex MATH3HI)
                                       (8bit->hex MATH3LO))
                          )
           )
         ]
        )
      ]
     
     [(TERM) $1])
    
    ;; Only Addition is defined
    (ADDOP
     [(+) '+]) 
    
    (TERM
     [(FACTOR) $1]
     )
    
    (FACTOR
     [(OP EXPRESSION CP ) $2]
     
     [(VAR) (car $1)]
     
     [(CALL) $1]
     
     [(NUM) (let-values ([(lo hi hexLo hexHi) (int->16bit $1)])
              ;; Assembly Code
              ;; Push the number onto Stack
              (format-code 6 "A9 ~a 48 A9 ~a 48 " hexHi hexLo)
              )])
    
    ;; Calling Functions
    (CALL
     ;; Functions with one argument
     [(FNCT OP ARGS CP )      
      (cond ((string=? (symbol->string $1) "output")
          (string-append $3
                         (format-code 8 "68 85 ~a 68 85 ~a 85 ~a " 
                                      (8bit->hex IOINTL) 
                                      (8bit->hex IOINTH)
                                      (8bit->hex IOFROB))
                         (format-code 2 "A5 ~a " (8bit->hex IOINTH));lda IOINTH
                         (format-code 1 "48 ");pha
                         (format-code 2 "A5 ~a " (8bit->hex IOINTL));lda IOINTL
                         (format-code 1 "48 ");pha
                         ))
          )
      ])    
    (ARGS
     [(ARG-LIST) $1]
     [() ""])
    (ARG-LIST
     ;; single argument arg-lists
     [(EXPRESSION) $1])
    )))

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
(define (make ip)
  (printf "D000") ;; NOT LITTLE-ENDIAN!!!!!
  (newline)
  (letrec ((one-seq
            (lambda ()
              (let ((result (parse (lambda () (lex ip)))))
                (when (not result)
                  (one-seq))))))
    (one-seq))
  ;  (display "HLT") ;; Hex 02
  (display "02") ;; Hex 02
  (newline)
  ;  (display ".end")
  (display "FFFFFF") ;; Hex 02
  (let-values ([(lo hi hexLo hexHi) (int->16bit ENTRYPOINT)])
    ;; Assembly Code
    ;; jump to the address of the subroutine
    (printf "~n~a~a~nFFFFFF" hexHi hexLo) ;; <==== byte reversal!!!
    )
  ;  (newline)
  )

;; quick test
(make (open-input-string "int x; void main(){x=0;while(x!=5){output(x);x=x+1;}\noutput(x);}"))