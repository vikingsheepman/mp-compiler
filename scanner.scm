#!/usr/bin/env racket
#lang racket

;;---------------------------------------------------------;;
;;                                                         ;;
;; CSCI-468 Compilers Project                              ;;
;; Phase 1: Scanner                                        ;;
;;                                                         ;;
;; Group Members:                                          ;;
;;      Killian Smith                                      ;;
;;      Joshua Kilpatrick                                  ;;
;;                                                         ;;
;;---------------------------------------------------------;;
;;                                                         ;;
;; The purpose of this program is to convert a minimal     ;;
;; pascal grammar, called micro-pascal, into a token list. ;;
;; The program is written in the dialect of scheme known   ;;
;; as racket, and makes heavy use of deterministic finite  ;;
;; automaton for single pass pattern matching.             ;;
;;                                                         ;;
;; To run this program from the terminal, issue the        ;;
;; command:                                                ;;
;;                                                         ;;
;;     ./scanner.scm <filename>                            ;;   
;;                                                         ;;
;;---------------------------------------------------------;;


;;---------------------------------------------------------;;
;;                                                         ;;
;; ---- Command Line Arguments ----                        ;;
;;                                                         ;;
;; Accepts the pascal filename as only argument            ;;
;;                                                         ;;
;;---------------------------------------------------------;;

(define infile
  (command-line
   #:args (filename)
   filename))

;; open and name the file pointer
(define fp (open-input-file infile))


;; - flag for logging - begin scanner execution
(printf "~%<-- Begin Scanning --> ~%~%")


;;---------------------------------------------------------;;
;;                                                         ;;
;; ---- Global Variables ----                              ;;
;;                                                         ;;
;; linum      -> stores the current line number of the     ;;
;;               pascal program that is being scanned      ;;
;; colnum     -> stores the current column number of the   ;;
;;               pascal program that is being scanned      ;;
;; token-list -> list contains all found tokens            ;;
;;                                                         ;;
;;---------------------------------------------------------;;

(define linum 0)
(define colnum 0)
(define token-list '())



;;---------------------------------------------------------;;
;;                                                         ;;
;; ---- DFA modules ----                                   ;;
;;                                                         ;;
;; DFA's are expressed as tuples that consist of:          ;;
;;        1) a hash of each states transition function     ;;
;;                - key is current-state                   ;;
;;                - value is transition-function           ;;
;;        2) the list of accepted states                   ;;
;;                                                         ;;
;;---------------------------------------------------------;;

;; DFA identifies add tokens that start with a digit
(define mp-digit-start-dfa
  (list
   (hash "q0"            '(lambda (x) (cond ((regexp-match? #rx"[0-9]"  x)  '"mp-integer")
                                            (else                           '"reject")))
              
         "mp-integer"    '(lambda (x) (cond ((regexp-match? #rx"[0-9]"  x)  '"mp-integer")
                                            ((string=?      "."         x)  '"digit-dot")
                                            ((regexp-match? #rx"e|E"    x)  '"e-char")
                                            ((regexp-match? #rx"\\+|-"  x)  '"signed-char")
                                            (else                           '"reject")))
              
         "digit-dot"     '(lambda (x) (cond ((regexp-match? #rx"[0-9]"  x)  '"mp-float")
                                            (else                           '"reject")))
         
         "mp-float"      '(lambda (x) (cond ((regexp-match? #rx"[0-9]"  x)  '"mp-float")
                                            ((regexp-match? #rx"e|E"    x)  '"e-char")
                                            ((regexp-match? #rx"\\+|-"  x)  '"signed-char")
                                            (else                           '"reject")))
         
         "e-char"        '(lambda (x) (cond ((regexp-match? #rx"[0-9]"  x)  '"mp-exponent")
                                            (else                           '"reject")))
         
         "signed-char"   '(lambda (x) (cond ((regexp-match? #rx"e|E"    x)  '"e-char")
                                            (else                           '"reject")))
         
         "mp-exponent"   '(lambda (x) (cond ((regexp-match? #rx"[0-9]"  x)  '"mp-exponent")
                                            (else                           '"reject")))
         )
   '("mp-integer" "mp-float" "mp-exponent")))


;; DFA identifies micro-pascal comments, and keeps track of any newline characters
(define mp-comment-dfa
  (list
   (hash "q0"            '(lambda (x) (cond ((string=?      "{"         x)  '"unclosed")
                                            (else                           '"reject")))
              
         "unclosed"      '(lambda (x) (cond ((string=?      "}"         x)  '"mp-comment")
                                            ((string=?      "\n"        x)  ((set! linum (+ linum 1))
                                                                             '"unclosed"))
                                            (else                           '"unclosed")))
         
         "mp-comment"    '(lambda (x)                                       '"reject"))
   '("mp-comment")))


;; DFA identifies all tokens that begin with a letter
;;   ->  needs to look up keywords this hashtable inside of
;;       the get-next-token function
(define mp-keyword-table
  (hash "and"        '"mp-and"
        "begin"      '"mp-begin"
        "div"        '"mp-div"
        "do"         '"mp-do"
        "downto"     '"mp-downto"
        "else"       '"mp-else"
        "end"        '"mp-end"
        "fixed"      '"mp-fixed"
        "float"      '"mp-float"
        "for"        '"mp-for"
        "function"   '"mp-function"
        "if"         '"mp-if"
        "integer"    '"mp-integer"
        "mod"        '"mp-mod"
        "not"        '"mp-not"
        "or"         '"mp-or"
        "procedure"  '"mp-procedure"
        "program"    '"mp-program"
        "read"       '"mp-read"
        "repeat"     '"mp-repeat"
        "then"       '"mp-then"
        "to"         '"mp-to"
        "until"      '"mp-until"
        "var"        '"mp-var"
        "while"      '"mp-while"
        "write"      '"mp-write"
        ))

(define mp-letter-start-dfa
  (list
   (hash "q0"              '(lambda (x) (cond ((regexp-match? #rx"[a-zA-Z]"     x)  '"word")
                                              (else                                 '"reject")))
         
         "word"            '(lambda (x) (cond ((regexp-match? #rx"[a-zA-Z]"     x)  '"word")
                                              ((regexp-match? #rx"[0-9]"        x)  '"mp-identifier")
                                              (else                                 '"reject")))
         
         "mp-identifier"   '(lambda (x) (cond ((regexp-match? #rx"[0-9a-zA-Z]"  x)  '"mp-identifier")
                                              (else                                 '"reject"))))
   '("word" "mp-identifier")))


;; TODO ==> Finish filling out all of mini-pascals token DFA's
;;          Make sure all states and transitions are accounted
;;          for when constructing a token dfa.
;;      ==> Only special characters are left (like < > ( ) . , ; : + - * / ... etc),
;;          as well as strings (inside single quotes)
;;      ==> Entry points will need to be added below



;;---------------------------------------------------------;;
;;                                                         ;;
;; ---- Driver function for Scanner ----                   ;;
;;                                                         ;;
;; Function attempts to find and return the next token     ;;
;; of a given micro-pascal program. If the function fails, ;;
;; then the program returns an error message that details  ;;
;; the line and column number where it failed              ;;
;;                                                         ;;
;;---------------------------------------------------------;;

(define (get-next-token)
  (let ((next-char   (peek-char fp))
        (lexeme      '"")
        (last-token  '"")
        (fp-offset   0))
    
    ;; step through a given DFA and return the resulting token-lexeme pair
    (define (run-dfa dfa current-state char)
      (let ((next-state    ((eval (hash-ref (car dfa) current-state)
                                  (make-base-namespace))
                            char))
            (final-states  (cadr dfa)))

        (cond ((eq? next-state "reject")
               (cond ((member current-state final-states)   (list current-state lexeme))
                     
                     (else                                  (file-position (- fp fp-offset))
                                                             last-token)))
              
              (else
               (cond ((member next-state final-states)      (set! last-token  lexeme)
                                                            (set! fp-offset   0)
                                                            (set! lexeme      (string-append lexeme char))
                                                            (set! colnum      (+ colnum 1))
                                                            (read-char fp)
                                                            (run-dfa dfa next-state (string (peek-char fp))))
                                                         
                     (else                                  (set! fp-offset   (+ fp-offset 1))
                                                            (set! lexeme      (string-append lexeme char))
                                                            (set! colnum      (+ colnum 1))
                                                            (read-char fp)
                                                            (run-dfa dfa next-state (string (peek-char fp))))))
              ))) ;; end run-dfa

    
    (cond ((eof-object? next-char) '"EOF")
          
          ((regexp-match? #rx"[a-zA-Z]"  (string next-char))
           (let ((id-found (run-dfa mp-letter-start-dfa '"q0" (string next-char))))
             (cond ((string=? (car id-found) "word")
                    (list
                     (hash-ref mp-keyword-table (string-downcase (cadr id-found)) '"mp-identifier")
                     (cadr id-found)))
                   (else id-found))))
          
          ((regexp-match? #rx"[0-9]"     (string next-char))    (run-dfa mp-digit-start-dfa   '"q0" (string next-char)))

          ((string=?      "{"            (string next-char))    (run-dfa mp-comment-dfa       '"q0" (string next-char)))



          ;; TODO ==>  finish putting other starting character DFA entry points here
          
          
          
          ((string=?      "\n"           (string next-char))    (set! linum (+ linum 1))
                                                                (set! colnum 0)
                                                                (read-char fp)
                                                                '())
          
          ((string=?      " "            (string next-char))    (set! colnum (+ colnum 1))
                                                                (read-char fp)
                                                                '())

          ((string=?      "\t"           (string next-char))    (set! colnum (+ colnum 4))
                                                                (read-char fp)
                                                                '())
          
          
          ((string=?      "\r"           (string next-char))    (read-char fp)
                                                                '())
          
          (else                                                 (list
                                                                 (string-append
                                                                  '"  ->  unreconized start of token at ["
                                                                  (number->string linum)
                                                                  ":"
                                                                  (number->string colnum)
                                                                  "] :: \""
                                                                  (string (read-char fp))
                                                                  "\" ")))))) ;; end get-next-token



;; test the functionality of the scanner
(define (get-token-list)
  (let ((token (get-next-token)))
    (cond ((eq? token "EOF") (printf "\n<EOF>\n")
                             '"")
          (else (cond ((not (eq? token '())) (append token token-list)
                                             (printf "~a~%" token)))
                (get-token-list)))))

(printf "~a~%" (get-token-list))



;; - flag for logging - finished executing scanner
(printf "~%~%<-- Finished Scanning -->~%~%")


;;---------------------------------------------------------;;
;;                                                         ;;
;;                        EOF                              ;;
;;                                                         ;;
;;---------------------------------------------------------;;
