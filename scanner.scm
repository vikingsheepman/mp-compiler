#!/usr/bin/env racket
#lang racket

;;---------------------------------------------------------;;
;;                                                         ;;
;; CSCI-468 Compilers Project                              ;;
;; Phase 1: Scanner                                        ;;
;;                                                         ;;
;; Last Modified: 2014-01-30                               ;;
;;                                                         ;;
;; Author: Killian Smith                                   ;;
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
;;     ./scanner.scm <filename> > out.txt                  ;;   
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


;;----------------------------------------------------------------;;
;;                                                                ;;
;; ---- Global Variables ----                                     ;;
;;                                                                ;;
;; linum            -> stores the current line number of the      ;;
;;                     pascal program that is being scanned       ;;
;; colnum           -> stores the current column number of the    ;;
;;                     pascal program that is being scanned       ;;
;; mp-keyword-table -> hash table that contains all of the micro  ;;
;;                     pascal reservered words                    ;;
;;                                                                ;;
;;----------------------------------------------------------------;;

(define linum 1)
(define colnum 0)

(define mp-keyword-table
  (hash "and"        '"mp-and"
        "begin"      '"mp-begin"
        "Boolean"    '"mp-boolean"
        "div"        '"mp-div"
        "do"         '"mp-do"
        "downto"     '"mp-downto"
        "else"       '"mp-else"
        "end"        '"mp-end"
        "false"      '"mp-false"
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
        "string"     '"mp-string"
        "then"       '"mp-then"
        "true"       '"mp-true"
        "to"         '"mp-to"
        "until"      '"mp-until"
        "var"        '"mp-var"
        "while"      '"mp-while"
        "write"      '"mp-write"
        "writeln"    '"mp-writeln"))


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
   (hash "q0"            '(lambda (x) (cond ((regexp-match? #rx"[0-9]"  x)  '"mp-integer-lit")
                                            (else                           '"reject")))
              
         "mp-integer-lit"    '(lambda (x) (cond ((regexp-match? #rx"[0-9]"  x)  '"mp-integer-lit")
                                            ((string=?      "."         x)  '"digit-dot")
                                            ((regexp-match? #rx"e|E"    x)  '"e-char")
                                            ((regexp-match? #rx"\\+|-"  x)  '"signed-char")
                                            (else                           '"reject")))
              
         "digit-dot"     '(lambda (x) (cond ((regexp-match? #rx"[0-9]"  x)  '"mp-fixed-lit")
                                            (else                           '"reject")))
         
         "mp-fixed-lit"      '(lambda (x) (cond ((regexp-match? #rx"[0-9]"  x)  '"mp-fixed-lit")
                                            ((regexp-match? #rx"e|E"    x)  '"e-char")
                                            ((regexp-match? #rx"\\+|-"  x)  '"signed-char")
                                            (else                           '"reject")))
         
         "e-char"        '(lambda (x) (cond ((regexp-match? #rx"[0-9]"  x)  '"mp-float-lit")
                                            (else                           '"reject")))
         
         "signed-char"   '(lambda (x) (cond ((regexp-match? #rx"e|E"    x)  '"e-char")
                                            (else                           '"reject")))
         
         "mp-float-lit"   '(lambda (x) (cond ((regexp-match? #rx"[0-9]"  x)  '"mp-float-lit")
                                            (else                           '"reject")))
         )
   '("mp-integer-lit" "mp-fixed-lit" "mp-float-lit")))


;; DFA identifies micro-pascal comments, and keeps track of any newline characters
(define mp-comment-dfa
  (list
   (hash "q0"            '(lambda (x) (cond ((string=?      "{"         x)  '"unclosed")
                                            (else                           '"reject")))
              
         "unclosed"      '(lambda (x) (cond ((string=?      "}"         x)  '"mp-comment")
                                            (else                           '"unclosed")))
         
         "mp-comment"    '(lambda (x)                                       '"reject"))
   '("mp-comment")))


;; DFA identifies all tokens that begin with a letter
(define mp-letter-start-dfa
  (list
   (hash "q0"              '(lambda (x) (cond ((regexp-match? #rx"[a-zA-Z]"     x)  '"word")
                                              (else                                 '"reject")))
         
         "word"            '(lambda (x) (cond ((regexp-match? #rx"[a-zA-Z]"     x)  '"word")
                                              ((regexp-match? #rx"[0-9]"        x)  '"mp-identifier")
                                              ((string=? "_"                    x)  '"uscore")
                                              (else                                 '"reject")))

         "uscore"          '(lambda (x) (cond ((regexp-match? #rx"[0-9a-zA-Z]"  x)  '"mp-identifier")
                                              (else                                 '"reject")))
         
         "mp-identifier"   '(lambda (x) (cond ((regexp-match? #rx"[0-9a-zA-Z]"  x)  '"mp-identifier")
                                              ((string=? "_"                    x)  '"uscore")
                                              (else                                 '"reject"))))
   '("word" "uscore" "mp-identifier")))


;; DFA identifies mp-string values
(define mp-string-lit-dfa
  (list
   (hash "q0"                  '(lambda (x) (cond ((string=? x "'")        '"unclosed-str")
                                                  (else                    '"reject")))
         "unclosed-str"        '(lambda (x) (cond ((string=? x "'")        '"mp-string-lit")
                                                  (else                    '"unclosed-str")))
         "mp-string-lit"       '(lambda (x)                                '"reject"))
   '("mp-string-lit")))


;; remaining DFA's are single string tokens
(define mp-colon-start-dfa
  (list
   (hash "q0"          '(lambda (x) (cond ((string=? x ":")      '"mp-colon")
                                          (else                  '"reject")))
         "mp-colon"    '(lambda (x) (cond ((string=? x "=")      '"mp-assign")
                                         (else                   '"reject")))
         "mp-assign"   '(lambda (x)                              '"reject"))
   '("mp-colon" "mp-assign")))

(define mp-comma-dfa
  (list
   (hash "q0"         '(lambda (x) (cond ((string=? x ",")     '"mp-comma")
                                         (else                 '"reject")))
         "mp-comma"   '(lambda (x)                             '"reject"))
   '("mp-comma")))

(define mp-equal-dfa
  (list
   (hash "q0"         '(lambda (x) (cond ((string=? x "=")     '"mp-equal")
                                         (else                 '"reject")))
         "mp-equal"   '(lambda (x)                             '"reject"))
   '("mp-equal")))

(define mp-float-divide-dfa
  (list
   (hash "q0"                '(lambda (x) (cond ((string=? x "/")     '"mp-float-divide")
                                                (else                 '"reject")))
         "mp-float-divide"   '(lambda (x)                             '"reject"))
   '("mp-float-divide")))

(define mp-greater-start-dfa
  (list
   (hash "q0"          '(lambda (x) (cond ((string=? x ">")      '"mp-gthan")
                                          (else                  '"reject")))
         "mp-gthan"    '(lambda (x) (cond ((string=? x "=")      '"mp-gequal")
                                          (else                  '"reject")))
         "mp-gequal"   '(lambda (x)                              '"reject"))
   '("mp-gthan" "mp-gequal")))

(define mp-less-start-dfa
  (list
   (hash "q0"          '(lambda (x) (cond ((string=? x "<")     '"mp-lthan")
                                          (else                 '"reject")))
         "mp-lthan"    '(lambda (x) (cond ((string=? x "=")     '"mp-lequal")
                                          ((string=? x ">")     '"mp-nequal")
                                          (else                 '"reject")))
         "mp-lequal"   '(lambda (x)                             '"reject")
         "mp-nequal"   '(lambda (x)                             '"reject"))
   '("mp-lthan" "mp-lequal" "mp-nequal")))

(define mp-lparen-dfa
  (list
   (hash "q0"          '(lambda (x) (cond ((string=? x "(")     '"mp-lparen")
                                          (else                 '"reject")))
         "mp-lparen"   '(lambda (x)                             '"reject"))
   '("mp-lparen")))

(define mp-rparen-dfa
  (list
   (hash "q0"          '(lambda (x) (cond ((string=? x ")")     '"mp-rparen")
                                          (else                 '"reject")))
         "mp-rparen"   '(lambda (x)                             '"reject"))
   '("mp-rparen")))

(define mp-minus-dfa
  (list
   (hash "q0"         '(lambda (x) (cond ((string=? x "-")     '"mp-minus")
                                         (else                 '"reject")))
         "mp-minus"   '(lambda (x)                             '"reject"))
   '("mp-minus")))

(define mp-plus-dfa
  (list
   (hash "q0"        '(lambda (x) (cond ((string=? x "+")     '"mp-plus")
                                        (else                 '"reject")))
         "mp-plus"   '(lambda (x)                             '"reject"))
   '("mp-plus")))


(define mp-times-dfa
  (list
   (hash "q0"         '(lambda (x) (cond ((string=? x "*")    '"mp-times")
                                         (else                '"reject")))
         "mp-times"   '(lambda (x)                            '"reject"))
   '("mp-times")))

(define mp-period-dfa
  (list
   (hash "q0"          '(lambda (x) (cond ((string=? x ".")   '"mp-period")
                                          (else               '"reject")))
         "mp-period"   '(lambda (x)                           '"reject"))
   '("mp-period")))

(define mp-scolon-dfa
  (list
   (hash "q0"          '(lambda (x) (cond ((string=? x ";")   '"mp-scolon")
                                          (else               '"reject")))
         "mp-scolon"   '(lambda (x)                           '"reject"))
   '("mp-scolon")))



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
        (last-token  '())
        (fp-offset   0))
    
    ;; step through a given DFA and return the resulting token-lexeme pair
    (define (run-dfa dfa current-state char)
      (let ((next-state    ((eval (hash-ref (car dfa) current-state)
                                  (make-base-namespace))
                            (string char)))
            (final-states  (cadr dfa)))

        (cond ((eq? char #\newline) (set! linum (+ linum 1))))
        
        (cond ((eof-object? char)
               (cond ((eq? dfa mp-comment-dfa)     (list "mp-run-comment" "run on comment encountered"))
                     ((eq? dfa mp-string-lit-dfa)  (list "mp-run-string" "run on string encountered"))
                     (else                         (printf "unexpected eof char\n"))))

              ((eq? next-state "reject")
               (cond ((member current-state final-states)   (list current-state lexeme))
                     
                     (else                                  (file-position (- fp fp-offset))
                                                            (set! linum    (cadr last-token))
                                                            (set! colnum   (caadr last-token))
                                                            (car last-token))))
              
              (else
               (cond ((member next-state final-states)      (set! last-token  (list lexeme linum colnum))
                                                            (set! fp-offset   0)
                                                            (set! lexeme      (string-append lexeme (string char)))
                                                            (set! colnum      (+ colnum 1))
                                                            (read-char fp)
                                                            (run-dfa dfa next-state (peek-char fp)))
                                                         
                     (else                                  (set! fp-offset   (+ fp-offset 1))
                                                            (set! lexeme      (string-append lexeme (string char)))
                                                            (set! colnum      (+ colnum 1))
                                                            (read-char fp)
                                                            (run-dfa dfa next-state (peek-char fp)))))
              ))) ;; end run-dfa

    
    (cond ((eof-object? next-char) (list "mp-eof" "<eof>"))

          ;; sort out identifiers and reservered words from each other
          ((regexp-match? #rx"[a-zA-Z]"  (string next-char))
           (let ((id-found (run-dfa mp-letter-start-dfa '"q0" next-char)))
             (cond ((string=? (car id-found) "word")
                    (list
                     (hash-ref mp-keyword-table (string-downcase (cadr id-found)) '"mp-identifier")
                     (cadr id-found)))
                   ((string=? (car id-found) "uscore")
                    (list "mp-identifier" (cadr id-found)))
                   (else id-found))))
          
          ((regexp-match? #rx"[0-9]"     (string next-char))    (run-dfa mp-digit-start-dfa    '"q0" next-char))

          ((string=?      "'"            (string next-char))    (run-dfa mp-string-lit-dfa     '"q0" next-char))

          ((string=?      "{"            (string next-char))    (run-dfa mp-comment-dfa        '"q0" next-char))

          ((string=?      ":"            (string next-char))    (run-dfa mp-colon-start-dfa    '"q0" next-char))

          ((string=?      ";"            (string next-char))    (run-dfa mp-scolon-dfa         '"q0" next-char))

          ((string=?      ","            (string next-char))    (run-dfa mp-comma-dfa          '"q0" next-char))

          ((string=?      "."            (string next-char))    (run-dfa mp-period-dfa         '"q0" next-char))

          ((string=?      "="            (string next-char))    (run-dfa mp-equal-dfa          '"q0" next-char))

          ((string=?      ">"            (string next-char))    (run-dfa mp-greater-start-dfa  '"q0" next-char))

          ((string=?      "<"            (string next-char))    (run-dfa mp-less-start-dfa     '"q0" next-char))

          ((string=?      "("            (string next-char))    (run-dfa mp-lparen-dfa         '"q0" next-char))

          ((string=?      ")"            (string next-char))    (run-dfa mp-rparen-dfa         '"q0" next-char))

          ((string=?      "+"            (string next-char))    (run-dfa mp-plus-dfa           '"q0" next-char))

          ((string=?      "-"            (string next-char))    (run-dfa mp-minus-dfa          '"q0" next-char))

          ((string=?      "*"            (string next-char))    (run-dfa mp-times-dfa          '"q0" next-char))

          ((string=?      "/"            (string next-char))    (run-dfa mp-float-divide-dfa   '"q0" next-char))
          
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
                                                                 "mp-error"
                                                                 (string-append
                                                                  '"["
                                                                  (number->string linum)
                                                                  ":"
                                                                  (number->string colnum)
                                                                  "]")
                                                                 (string (read-char fp))
                                                                 ))))) ;; end get-next-token




;; test the functionality of the scanner
(define (print-token-list)
  (let ((token (get-next-token)))
    (cond ((eq? token '())             (print-token-list))
          
          ((eq? (car token) "mp-eof")  (printf "~a~%" token))
          
          (else                        (printf "~a~%" token)
                                       (print-token-list)))))

(print-token-list)



;; - flag for logging - finished executing scanner
(printf "~%<-- Finished Scanning -->~%~%")


;;---------------------------------------------------------;;
;;                                                         ;;
;;                        EOF                              ;;
;;                                                         ;;
;;---------------------------------------------------------;;
