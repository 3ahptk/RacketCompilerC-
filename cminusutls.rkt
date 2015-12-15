#lang racket
;; ============================================================================
;; AUTHORS: Leo C. Ureel II
;; PROJECT: C- Compiler
;; COURSE: CS 4121 Programming Languages
;; DESCRIPTION: This file contains several utilities for use in the C- Compiler
;; ============================================================================

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

;; END OF FILE ================================================================