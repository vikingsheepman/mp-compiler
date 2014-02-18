#! /usr/bin/env guile
!#

;;---------------------------------------------------------;;
;;                                                         ;;
;; CSCI-468 Compilers Project                              ;;
;; Phase 1: Scanner                                        ;;
;;                                                         ;;
;; Last Modified: 2014-02-15                               ;;
;;                                                         ;;
;; Author: Killian Smith                                   ;;
;;                                                         ;;
;;---------------------------------------------------------;;
;;                                                         ;;
;; The purpose of this program is to convert a minimal     ;;
;; pascal grammar, called micro-pascal, into a token list. ;;
;; The program is written in scheme and makes heavy use    ;;
;; of deterministic finite automaton for single pass       ;;
;; pattern matching.                                       ;;
;;                                                         ;;
;; This program servers as a utility file for the          ;;
;; micro-pascal parser, and is not a stand alone           ;;
;; progam.                                                 ;;
;;                                                         ;;
;;---------------------------------------------------------;;


(use-modules (ice-9 regex))


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
;; *-transitions    -> hash table declarations for all DFA        ;;
;;                     transitions                                ;;
;;                                                                ;;
;;----------------------------------------------------------------;;

;; line and column numbers
(define linum 1)
(define colnum 0)

;; micro-pascal reserved word tokens table
(define mp-keyword-table (make-hash-table 63))

;; DFA transition hash-tables
(define digit-start-transitions (make-hash-table 13))
(define letter-start-transitions (make-hash-table 11))
(define comment-transitions (make-hash-table 7))
(define string-transitions (make-hash-table 7))
(define colon-start-transitions (make-hash-table 7))
(define scolon-transitions (make-hash-table 13))
(define comma-transitions (make-hash-table 5))
(define period-transitions (make-hash-table 5))
(define equals-transitions (make-hash-table 5))
(define greater-start-transitions (make-hash-table 11))
(define less-start-transitions (make-hash-table 11))
(define lparen-transitions (make-hash-table 5))
(define rparen-transitions (make-hash-table 5))
(define plus-transitions (make-hash-table 5))
(define minus-transitions (make-hash-table 5))
(define times-transitions (make-hash-table 5))
(define float-divide-transitions (make-hash-table 5))



;;---------------------------------------------------------;;
;;                                                         ;;
;; ---- Helper Functions ----                              ;;
;;                                                         ;;
;; Functions are used to help preform indirect operations  ;;
;; needed for scanner function.                            ;;
;;                                                         ;;
;;---------------------------------------------------------;;

;; fill a hash table from a list of key-value tuples
(define (fill-hash-table table list)
  (define (fill-row pair)
    (hash-set! table (car pair) (cadr pair)))
  (map fill-row list))

;; initialize dfa transition tables
(define (init-transition-tables)
  (define digit-start-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string-match "[0-9]" x) '"mp-integer-lit")
                               (else '"reject"))))
     '("mp-integer-lit" '(lambda (x) (cond ((string-match "[0-9]" x) '"mp-integer-lit")
                                           ((string=? "." x) '"digit-dot")
                                           ((string-match "e|E" x) '"e-char")
                                           (else '"reject"))))           
     '("digit-dot" '(lambda (x) (cond ((string-match "[0-9]" x) '"mp-fixed-lit")
                                      (else '"reject"))))         
     '("mp-fixed-lit" '(lambda (x) (cond ((string-match "[0-9]" x) '"mp-fixed-lit")
                                         ((string-match "e|E" x) '"e-char")
                                         (else '"reject"))))         
     '("e-char" '(lambda (x) (cond ((string-match "[0-9]" x) '"mp-float-lit")
                                   ((string-match "\\+|-" x) '"signed-char")
                                   (else '"reject"))))         
     '("signed-char" '(lambda (x) (cond ((string-match "[0-9]" x) '"mp-float-lit")
                                        (else '"reject"))))     
     '("mp-float-lit" '(lambda (x) (cond ((string-match "[0-9]" x) '"mp-float-lit")
                                         (else '"reject"))))))

  (define letter-start-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string-match "[a-zA-Z]" x) '"word")
                               (else '"reject"))))        
     '("word" '(lambda (x) (cond ((string-match "[a-zA-Z]" x) '"word")
                                 ((string-match "[0-9]" x) '"mp-identifier")
                                 ((string=? "_" x) '"uscore")
                                 (else '"reject"))))
     '("uscore" '(lambda (x) (cond ((string-match "[0-9a-zA-Z]" x) '"mp-identifier")
                                   (else '"reject"))))        
     '("mp-identifier" '(lambda (x) (cond ((string-match "[0-9a-zA-Z]" x) '"mp-identifier")
                                          ((string=? "_" x) '"uscore")
                                          (else '"reject"))))))
  
  (define comment-transitions-list
    (list
      '("q0" '(lambda (x) (cond ((string=? "{" x) '"unclosed")
                                (else '"reject"))))
      '("unclosed" '(lambda (x) (cond ((string=? "}" x) '"mp-comment")
                                      (else '"unclosed"))))
      '("mp-comment" '(lambda (x) '"reject"))))

  (define string-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string=? x "'") '"unclosed-str")
                               (else '"reject"))))    
     '("unclosed-str" '(lambda (x) (cond ((string=? x "'")  '"apos")
                                          (else '"unclosed-str"))))
     '("apos" '(lambda (x) (cond ((string=? x "'") '"unclosed-str")
                                 (else '"mp-string-lit"))))
     '("mp-string-lit" '(lambda (x) '"reject"))))

  (define colon-start-transitions-list
   (list
    '("q0" '(lambda (x) (cond ((string=? x ":") '"mp-colon")
                              (else '"reject"))))   
    '("mp-colon" '(lambda (x) (cond ((string=? x "=") '"mp-assign")
                                    (else '"reject"))))   
    '("mp-assign" '(lambda (x) '"reject"))))

  (define scolon-transitions-list
   (list
    '("q0" '(lambda (x) (cond ((string=? x ";") '"mp-scolon")
                              (else '"reject"))))
    '("mp-scolon" '(lambda (x) '"reject"))))

  (define comma-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string=? x ",") '"mp-comma")
                               (else '"reject"))))
     '("mp-comma" '(lambda (x) '"reject"))))

  (define period-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string=? x ".") '"mp-period")
                               (else '"reject"))))
     '("mp-period" '(lambda (x) '"reject"))))

  (define equals-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string=? x "=") '"mp-equal")
                               (else '"reject"))))
     '("mp-equal" '(lambda (x) '"reject"))))

  (define greater-start-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string=? x ">") '"mp-gthan")
                               (else '"reject"))))
     '("mp-gthan" '(lambda (x) (cond ((string=? x "=") '"mp-gequal")
                                     (else '"reject"))))
     '("mp-gequal" '(lambda (x) '"reject"))))

  (define less-start-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string=? x "<") '"mp-lthan")
                                          (else '"reject"))))
     '("mp-lthan" '(lambda (x) (cond ((string=? x "=") '"mp-lequal")
                                     ((string=? x ">") '"mp-nequal")
                                     (else '"reject"))))
     '("mp-lequal" '(lambda (x) '"reject"))
     '("mp-nequal" '(lambda (x) '"reject"))))

  (define lparen-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string=? x "(") '"mp-lparen")
                               (else '"reject"))))
     '("mp-lparen" '(lambda (x) '"reject"))))

  (define rparen-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string=? x ")") '"mp-rparen")
                               (else '"reject"))))
     '("mp-rparen" '(lambda (x) '"reject"))))

  (define plus-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string=? x "+") '"mp-plus")
                               (else '"reject"))))
     '("mp-plus" '(lambda (x) '"reject"))))

  (define minus-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string=? x "-") '"mp-minus")
                               (else '"reject"))))
     '("mp-minus" '(lambda (x) '"reject"))))

  (define times-transitions-list
   (list
    '("q0" '(lambda (x) (cond ((string=? x "*") '"mp-times")
                              (else '"reject"))))
    '("mp-times" '(lambda (x) '"reject"))))

  (define float-divide-transitions-list
    (list
     '("q0" '(lambda (x) (cond ((string=? x "/") '"mp-float-divide")
                               (else '"reject"))))
     '("mp-float-divide" '(lambda (x) '"reject"))))

  ;; micro-pascal reserved words
  (define reserved-words-list
    (list
     '("and"        "mp-and")
     '("begin"      "mp-begin")
     '("Boolean"    "mp-boolean")
     '("div"        "mp-div")
     '("do"         "mp-do")
     '("downto"     "mp-downto")
     '("else"       "mp-else")
     '("end"        "mp-end")
     '("false"      "mp-false")
     '("fixed"      "mp-fixed")
     '("float"      "mp-float")
     '("for"        "mp-for")
     '("function"   "mp-function")
     '("if"         "mp-if")
     '("integer"    "mp-integer")
     '("mod"        "mp-mod")
     '("not"        "mp-not")
     '("or"         "mp-or")
     '("procedure"  "mp-procedure")
     '("program"    "mp-program")
     '("read"       "mp-read")
     '("readln"     "mp-readln")
     '("repeat"     "mp-repeat")
     '("string"     "mp-string")
     '("then"       "mp-then")
     '("true"       "mp-true")
     '("to"         "mp-to")
     '("until"      "mp-until")
     '("var"        "mp-var")
     '("while"      "mp-while")
     '("write"      "mp-write")
     '("writeln"    "mp-writeln")))
  
  ;; fill the hash tables
  (fill-hash-table digit-start-transitions digit-start-transitions-list)
  (fill-hash-table letter-start-transitions letter-start-transitions-list)
  (fill-hash-table comment-transitions comment-transitions-list)
  (fill-hash-table string-transitions string-transitions-list)
  (fill-hash-table colon-start-transitions colon-start-transitions-list)
  (fill-hash-table scolon-transitions scolon-transitions-list)
  (fill-hash-table comma-transitions comma-transitions-list)
  (fill-hash-table period-transitions period-transitions-list)
  (fill-hash-table equals-transitions equals-transitions-list)
  (fill-hash-table greater-start-transitions greater-start-transitions-list)
  (fill-hash-table less-start-transitions less-start-transitions-list)
  (fill-hash-table lparen-transitions lparen-transitions-list)
  (fill-hash-table rparen-transitions rparen-transitions-list)
  (fill-hash-table plus-transitions plus-transitions-list)
  (fill-hash-table minus-transitions minus-transitions-list)
  (fill-hash-table times-transitions times-transitions-list)
  (fill-hash-table float-divide-transitions float-divide-transitions-list)

  ;; fill reserved words table
  (fill-hash-table mp-keyword-table reserved-words-list))



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
   digit-start-transitions '("mp-integer-lit" "mp-fixed-lit" "mp-float-lit")))

;; DFA identifies all tokens that begin with a letter
(define mp-letter-start-dfa
  (list
   letter-start-transitions '("word" "uscore" "mp-identifier")))

;; DFA identifies micro-pascal comments
(define mp-comment-dfa
  (list
   comment-transitions '("mp-comment")))

;; DFA identifies mp-string values
(define mp-string-lit-dfa
  (list
   string-transitions '("mp-string-lit")))

;; DFA identifies all tokens starting with a colon
(define mp-colon-start-dfa
  (list
   colon-start-transitions '("mp-colon" "mp-assign")))

;; DFA matches scolon token
(define mp-scolon-dfa
  (list
   scolon-transitions '("mp-scolon")))

;; matches comma token
(define mp-comma-dfa
  (list
   comma-transitions '("mp-comma")))

;; matches period token
(define mp-period-dfa
  (list
   period-transitions '("mp-period")))

;; matches tokens that start with equal
(define mp-equal-dfa
  (list
   equals-transitions '("mp-equal")))

;; matches tokens that start with greater than
(define mp-greater-start-dfa
  (list
   greater-start-transitions '("mp-gthan" "mp-gequal")))

;; matches tokens that start with less than
(define mp-less-start-dfa
  (list
   less-start-transitions '("mp-lthan" "mp-lequal" "mp-nequal")))

;; matches tokens that start with left paren
(define mp-lparen-dfa
  (list
   lparen-transitions '("mp-lparen")))

;; matches tokens that start with right paren
(define mp-rparen-dfa
  (list
   rparen-transitions '("mp-rparen")))

;; matches tokens that start with plus
(define mp-plus-dfa
  (list
   plus-transitions '("mp-plus")))

;; matches tokens that start with minus
(define mp-minus-dfa
  (list
   minus-transitions '("mp-minus")))

;; matches tokens that start with times
(define mp-times-dfa
  (list
   times-transitions '("mp-times")))

;; matches tokens that start with float divide
(define mp-float-divide-dfa
  (list
   float-divide-transitions '("mp-float-divide")))



;;---------------------------------------------------------;;
;;                                                         ;;
;; ---- Main function for Scanner ----                     ;;
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
      (cond
       ((eof-object? char)
        (cond ((eq? dfa mp-comment-dfa)     (list "mp-run-comment" "run-on-comment" linum colnum))
              ((eq? dfa mp-string-lit-dfa)  (list "mp-run-string" "run-on-string" linum colnum))))
       (else
        (let ((transition (eval (hash-ref (car dfa) current-state) (current-module))))
          (let ((next-state ((eval transition (current-module)) (string char)))
                (final-states (cadr dfa)))

           ; (cond ((string=? (string char) "\n") (set! linum (+ linum 1))))
            
            (cond ((string=? next-state "reject")
                   (cond ((member current-state final-states)    (list current-state lexeme linum colnum))
                   
                         (else                                   (file-position (- fp fp-offset))
                                                                 (set! linum    (caddr last-token))
                                                                 (set! colnum   (cadddr last-token))
                                                                 (last-token))))
              
                  (else
                   (cond ((member next-state final-states)      (set! lexeme      (string-append lexeme (string char)))
                                                                (set! last-token  (list next-state lexeme linum colnum))
                                                                (set! fp-offset   0)
                                                                (set! colnum      (+ colnum 1))
                                                                (read-char fp)
                                                                (run-dfa dfa next-state (peek-char fp)))
                                                         
                         (else                                  (set! lexeme      (string-append lexeme (string char)))
                                                                (set! fp-offset   (+ fp-offset 1))
                                                                (set! colnum      (+ colnum 1))
                                                                (read-char fp)
                                                                (run-dfa dfa next-state (peek-char fp)))))
                  )))))) ;; end run-dfa

    (cond
     ((eof-object? next-char) (list "mp-eof" "<eof>" linum colnum))

     ;; sort out identifiers and reservered words from each other
     ((string-match "[a-zA-Z]"  (string next-char))
      (let ((id-found (run-dfa mp-letter-start-dfa "q0" next-char)))
        (cond ((string=? (car id-found) "word")
               (list
                (hash-ref mp-keyword-table (string-downcase (cadr id-found)) '"mp-identifier")
                (cadr id-found) (caddr id-found) (cadddr id-found)))
              ((string=? (car id-found) "uscore")
               (list "mp-identifier" (cadr id-found) (caddr id-found) (cadddr id-found)))
              (else id-found))))
          
     ((string-match  "[0-9]"  (string next-char))    (run-dfa mp-digit-start-dfa    "q0" next-char))
     
     ((string=?      "'"      (string next-char))    (run-dfa mp-string-lit-dfa     "q0" next-char))
     
     ((string=?      "{"      (string next-char))    (run-dfa mp-comment-dfa        "q0" next-char))
     
     ((string=?      ":"      (string next-char))    (run-dfa mp-colon-start-dfa    "q0" next-char))
     
     ((string=?      ";"      (string next-char))    (run-dfa mp-scolon-dfa         "q0" next-char))
     
     ((string=?      ","      (string next-char))    (run-dfa mp-comma-dfa          "q0" next-char))
     
     ((string=?      "."      (string next-char))    (run-dfa mp-period-dfa         "q0" next-char))
     
     ((string=?      "="      (string next-char))    (run-dfa mp-equal-dfa          "q0" next-char))
     
     ((string=?      ">"      (string next-char))    (run-dfa mp-greater-start-dfa  "q0" next-char))
     
     ((string=?      "<"      (string next-char))    (run-dfa mp-less-start-dfa     "q0" next-char))
     
     ((string=?      "("      (string next-char))    (run-dfa mp-lparen-dfa         "q0" next-char))
     
     ((string=?      ")"      (string next-char))    (run-dfa mp-rparen-dfa         "q0" next-char))
     
     ((string=?      "+"      (string next-char))    (run-dfa mp-plus-dfa           "q0" next-char))
     
     ((string=?      "-"      (string next-char))    (run-dfa mp-minus-dfa          "q0" next-char))
     
     ((string=?      "*"      (string next-char))    (run-dfa mp-times-dfa          "q0" next-char))
     
     ((string=?      "/"      (string next-char))    (run-dfa mp-float-divide-dfa   "q0" next-char))
     
     ((string=?      "\n"     (string next-char))    (set! linum (+ linum 1))
                                                     (set! colnum 0)
                                                     (read-char fp)
                                                     '())
     
     ((string=?      " "      (string next-char))    (set! colnum (+ colnum 1))
                                                     (read-char fp)
                                                     '())
     
     ((string=?      "\t"     (string next-char))    (set! colnum (+ colnum 1))
                                                     (read-char fp)
                                                     '())
     
     
     ((string=?      "\r"     (string next-char))    (read-char fp)
                                                     '())
     
     (else                                           (list
                                                      "mp-error"
                                                      (string (read-char fp))
                                                      (number->string linum)
                                                      (number->string colnum)))
     ))) ;; end get-next-token



(define (get-token)
  (let ((token (get-next-token)))
    (cond ((or (eq? token '())
               (string=? (car token) "mp-comment"))
           (get-token))
          (else (format #t "  read -> ~a~%" token)
                token))))



;;---------------------------------------------------------;;
;;                                                         ;;
;;                        EOF                              ;;
;;                                                         ;;
;;---------------------------------------------------------;;
