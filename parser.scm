#!/usr/bin/env guile
!#

;;---------------------------------------------------------;;
;;                                                         ;;
;; CSCI-468 Compilers Project                              ;;
;; Phase 2: Parser                                         ;;
;;                                                         ;;
;; Last Modified: 2014-02-17                               ;;
;;                                                         ;;
;; Author: Killian Smith                                   ;;
;;                                                         ;;
;;---------------------------------------------------------;;
;;                                                         ;;
;; The purpose of this program is to check if the given    ;;
;; micro-pascal program will produce a valid parse tree    ;;
;; in accordance to the given LL(1) grammar.               ;;
;;                                                         ;;
;; To execute this program issue the command:              ;;
;;                                                         ;;
;;     ./parser.scm <file.pas>                             ;;
;;                                                         ;;
;; The program will verify the input file as a valid       ;;
;; micro-pascal progam, or will display any errors that    ;;
;; were encountered.                                       ;;
;;                                                         ;;
;;---------------------------------------------------------;;


(load "scanner.scm")
(define fp (open-input-file (cadr (command-line))))



;;---------------------------------------------------------;;
;;                                                         ;;
;; -- Helper Functions --                                  ;;
;;                                                         ;;
;; expect-token :                                          ;;
;;     Function returns an error message if the expected   ;;
;;     token is not encountered.                           ;;
;;                                                         ;;
;;---------------------------------------------------------;;

;; perform desired function if expected token is found
(define (expect-token expected token)
  (cond ((string=? (car token) expected) '())
        (else (format #t "~%parse error: expected ~a but found ~a at [~a:~a]~%~%"
                      expected (car token) (caddr token) (cadddr token))
              (exit))))

;; bind the current continuation to a variable
(define-syntax bind/cc
  (syntax-rules ()
    ((bind/cc var . body)
     (call/cc (lambda (var) . body)))))

;; token look-ahead
(define (peek-token)
  (bind/cc return
           (let ((token (get-token)))
             (return token))))


;;---------------------------------------------------------;;
;;                                                         ;;
;; -- Declare Non-terminal parser stubs --                 ;;
;;                                                         ;;
;; The following functions are translations of the given   ;;
;; micro-pascal grammar in EBNF form. The comments above   ;;
;; each function show the parse rule in its original form. ;; 
;;                                                         ;;
;;---------------------------------------------------------;;

;; <system-goal> -> <program> . EOF
(define (system-goal)
  (program)
  (expect-token '"mp-eof" (get-token))
  (format #t "program succesfully parsed~%"))

;; <program> -> <program-heading> . mp-scolon . <block> . mp-period
(define (program)
  (program-heading)
  (expect-token '"mp-scolon" (get-token))
  (block)
  (expect-token '"mp-period" (get-token)))

;; <program-heading> -> mp-program . <program-identifier>
(define (program-heading)
  (expect-token '"mp-program" (get-token))
  (program-identifier))

;; <block> -> <variable-declaration-part> . <procedure-and-function-declaration-part>
;;            . <statement-part>
(define (block)
  (variable-declaration-part)
  (procedure-and-function-declaration-part)
  (statement-part))

;; <variable-declaration-part> -> mp-var . <variable-declaration> . mp-scolon
;;                                . <variable-declaration-tail>
;;                             -> eps
(define (variable-declaration-part)
  (bind/cc return
           (cond ((string=? (car (get-token)) "mp-var")
                  (variable-declaration)
                  (expect-token '"mp-scolon" (get-token))
                  (bind/cc k (variable-declaration-tail k)))
                 (else (return '"")))))

;; <variable-declaration-tail> -> <variable-declaration> . mp-scolon
;;                                . <variable-declaration-tail>
;;                             -> eps
(define (variable-declaration-tail return)                                  ;-- need to implement peek function --;
  (bind/cc return
           (cond ((string=? (car (get-token)) "mp-identifier")
                  (variable-declaration)
                  (expect-token '"mp-scolon" (get-token))
                  (variable-declaration-tail))
                 (else (return '"")))))

;; <variable-declaration> -> <identifier-list> . mp-colon . <type>
(define (variable-declaration)
  (identifier-list)
  (expect-token '"mp-colon" (get-token))
  (type))

;; <type> -> mp-integer
;;        -> mp-float
;;        -> mp-string
;;        -> mp-boolean
(define (type)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-integer") '())
          ((string=? (car next-token) "mp-float") '())
          ((string=? (car next-token) "mp-string") '())
          ((string=? (car next-token) "mp-boolean") '())
          (else (format #t "~%parse error: expected a type but found ~a at [~a:~a]~%~%"
                        (car next-token) (caddr next-token) (cadddr next-token))
                (exit)))))

;; <procedure-and-function-declaration-part> -> <procedure-declaration>
;;                                              . <procedure-and-function-declaration-part>
;;                                           -> <function-declaration>
;;                                              . <procedure-and-function-declaration-part>
;;                                           -> eps
(define (procedure-and-function-declaration-part)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-procedure")
           (procedure-declaration)
           (procedure-and-function-declaration-part))
          ((string=? (car next-token) "mp-function")
           (function-declaration)
           (procedure-and-function-declaration-part))
          (else '""))))


;; <procedure-declaration> -> <procedure-heading> . mp-scolon . <block> . mp-colon
(define (procedure-declaration)
  (procedure-heading)
  (expect-token '"mp-scolon" (get-token))
  (block)
  (expect-token '"mp-scolon" (get-token)))

;; <function-declaration> -> <function-heading> . mp-scolon . <block> . mp-colon
(define (function-declaration)
  (function-heading)
  (expect-token '"mp-scolon" (get-token))
  (block)
  (expect-token '"mp-scolon" (get-token)))

;; <procedure-heading> -> mp-procedure . <procedure-identifier>
;;                            . <optional-formal-parameter-list>
(define (procedure-heading)
  (procedure-identifier)
  (optional-formal-parameter-list))

;; <function-heading> -> mp-function . <function-identifier>
;;                            . <optional-formal-parameter-list> . <type>
(define (function-heading)
  (function-identifier)
  (optional-formal-parameter-list)
  (type))

;; <optional-formal-parameter-list> -> mp-lparen . <formal-parameter-section>
;;                                         . <formal-parameter-section-tail>
;;                                         . mp-rparen
;;                                  -> eps
(define (optional-formal-parameter-list)
  (bind/cc return
           (begin
             (cond ((string=? (car (get-token)) "mp-lparen")
                    (formal-parameter-section)
                    (formal-parameter-section-tail)
                    (expect-token "mp-rparen" (get-token)))
                   (else (return return))))))

;; <formal-parameter-section-tail> -> mp-scolon . <formal-parameter-section>
;;                                        . <formal-parameter-section-tail>
;;                                 -> eps
(define (formal-parameter-section-tail)
  (bind/cc return
           (begin
             (let ((next-token (get-token)))
               (cond ((string=? (car next-token) "mp-scolon")
                      (formal-parameter-section)
                      (formal-parameter-section-tail))
                     (else (return return)))))))

;; <formal-parameter-section> -> <value-parameter-section>
;;                            -> <variable-parameter-section>
(define (formal-parameter-section)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-identifier")
           (value-parameter-section))
          ((string=? (car next-token) "mp-var")
           (variable-parameter-section))
          (else
           (format #t "parse error: expected a variable or value but found ~a at [~a:~a]~%"
                   (car next-token) (caddr next-token) (cadddr next-token))
           (exit)))))

;; <value-parameter-section> -> <identifier-list> . mp-colon . <type>
(define (value-parameter-section)
  (identifier-list)
  (expect-token "mp-colon" (get-token))
  (type))

;; <variable-parameter-section> -> mp-var . <identifier-list> . mp-colon . <type>
(define (variable-parameter-section)
  (identifier-list)
  (expect-token "mp-colon" (get-token))
  (type))

;; <statement-part> -> <compound-statement> 
(define (statement-part)
  (compound-statement))

;; <compound-statement> -> mp-begin . <statement-sequence> . mp-end
(define (compound-statement)
  (expect-token "mp-begin" (get-token))
  (statement-sequence)
  (expect-token "mp-end" (get-token)))

;; <statement-sequence> -> <statement> . <statement-tail>
(define (statement-sequence)
  (statement)
  (statement-tail))

;; <statement-tail> -> mp-scolon . <statement> . <statement-tail>
;;                  -> eps
(define (statement-tail)
  (bind/cc return
           (begin
             (cond ((string=? (car (get-token)) "mp-scolon") 
                    (statement)
                    (statement-tail))
                   (else (return return))))))

;; <statement> -> <empty-statement>
;;             -> <compound-statement>
;;             -> <read-statement>
;;             -> <write-statement>
;;             -> <assign-statement>
;;             -> <if-statement>
;;             -> <while-statement>
;;             -> <repeat-statement>
;;             -> <for-statement>
;;             -> <procedure-statement>
(define (statement)
  (bind/cc return
           (begin
             (let ((next-token (get-token)))
               (cond ((string=? (car next-token) "mp-begin") (compound-statement))
                     ((string=? (car next-token) "mp-read") (read-statement))
                     ((string=? (car next-token) "mp-readln") (read-statement))
                     ((string=? (car next-token) "mp-write") (write-statement))
                     ((string=? (car next-token) "mp-writeln") (write-statement))
                     ((string=? (car next-token) "mp-if") (if-statement))
                     ((string=? (car next-token) "mp-while") (while-statement))
                     ((string=? (car next-token) "mp-repeat") (repeat-statement))
                     ((string=? (car next-token) "mp-for") (for-statement))
                     ((string=? (car next-token) "mp-identifier")
                      (cond ((string=? (car (get-token)) "mp-assignment") (assignment-statement))
                            (else (procedure-statement))))
                     (else (return return)))))))

;; <empty-statement> -> eps
(define (empty-statement)
  '"")

;; <read-statement> -> mp-read . mp-lparen . <read-parameter>
;;                          . <read-parameter-tail> . mp-rparen
(define (read-statement)
  (expect-token "mp-lparen" (get-token))
  (read-parameter)
  (read-parameter-tail)
  (expect-token "mp-rparen" (get-token)))

;; <read-parameter-tail> -> mp-comma . <read-parameter> . <read-parameter-tail>
;;                       -> eps
(define (read-parameter-tail)
  (bind/cc return
           (begin
             (cond ((string=? (car (get-token)) "mp-comma")
                    (read-parameter)
                    (read-parameter-tail))
                   (else (return return))))))

;; <read-parameter> -> <variable-identifier>
(define (read-parameter)
  (variable-identifier))

;; <write-statement> -> mp-write . mp-lparen . <write-parameter>
;;                          . <write-parameter-tail> . mp-rparen
;;                   -> mp-writeln . mp-lparen . <write-parameter>
;;                          . <write-parameter-tail> . mp-rparen
(define (write-statement)
  (expect-token "mp-lparen" (get-token))
  (read-parameter)
  (read-parameter-tail)
  (expect-token "mp-rparen" (get-token)))

;; <write-parameter-tail> -> mp-comma . <write-parameter> . <write-parameter-tail>
;;                        -> eps
(define (write-parameter-tail)
  (bind/cc return
           (begin
             (cond ((string=? (car (get-token)) "mp-comma")
                    (write-parameter)
                    (write-parameter-tail))
                   (else (return return))))))

;; <write-parameter> -> <ordinal-expression>
(define (write-parameter)
  (ordinal-expression))

;; <assignment-statement> -> <variable-identifier> . mp-assignment . <expression>
;;                        -> <function-identifier> . mp-assignment . <expression>
(define (assignment-statement)
  (expression))

;; <if-statement> -> mp-if . <boolean-expression> . mp-then
;;                   . <statement> . <optional-else-part>
(define (if-statement)
  (boolean-expression)
  (expect-token "mp-then" (get-token))
  (statement)
  (optional-else-part))

;;<optional-else-part> -> mp-else . <statement>
;;                     -> eps
(define (optional-else-part)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-else") (statement))
          (else '""))))

;; <repeat-statement> -> mp-repeat . <statement-sequence> . mp-until
;;                           . <boolean-expression>
(define (repeat-statement)
  (statement-sequence)
  (expect-token "mp-until" (get-token))
  (boolean-expression))

;; <while-statement> -> mp-while . <boolean-expression> . mp-do . <statement>
(define (while-statement)
  (boolean-expression)
  (expect-token "mp-do" (get-token))
  (statement))

;; <for-statement> -> mp-for . <control-variable> . mp-assigment . <initial-value>
;;                        . <step-value> . <final-value> . mp-do . <statement>
(define (for-statement)
  (control-variable)
  (expect-token "mp-assignment" (get-token))
  (initial-value)
  (step-value)
  (final-value)
  (expect-token "mp-do" (get-token))
  (statement))

;; <control-variable> -> <variable-identifier>
(define (control-variable)
  (variable-identifier))

;; <initial-value> -> <ordinal-expression>
(define (initial-value)
  (ordinal-expression))

;; <step-value> -> mp-to
;;              -> mp-downto
(define (step-value)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-to") '())
          ((string=? (car next-token) "mp-downto") '())
          (else (format #t "parse error: expected a step-value but found ~a at [~a:~a]~%"
                        (car next-token) (caddr next-token) (cadddr next-token))
                (exit)))))

;; <final-value> -> <ordinal-expression>
(define (final-value)
  (ordinal-expression))

;; <procedure-statement> -> <procedure-identifier> . <optional-actual-parameter-list>
(define (procedure-statement)
  (procedure-identifier)
  (optional-actual-parameter-list))

;; <optional-actual-parameter-list> -> mp-lparen . <actual-parameter> . <actual-parameter-tail>
;;                                     . mp-rparen
;;                                  -> eps
(define (optional-actual-parameter-list)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-lparen")
           (actual-parameter)
           (actual-parameter-tail)
           (expect-token "mp-rparen" (get-token )))
          (else '""))))


;; <actual-parameter-tail> -> mp-comma . <actual-parameter> . <actual-parameter-tail>
;;                         -> eps
(define (actual-parameter-tail)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-comma")
           (actual-parameter)
           (actual-parameter-tail))
          (else '""))))

;; <actual-parameter> -> <ordinal-expression>
(define (actual-parameter)
  (ordinal-expression))

;; <expression> -> <simple-expression> . <optional-relation-part>
(define (expression)
  (simple-expression)
  (optional-relation-part))

;; <optional-relation-part> -> <relational-operator> . <optional-relational-part>
;;                          -> eps
(define (optional-relation-part)
  (let ((next-token (get-token)))
    (cond ((or (string=? (car next-token) "mp-equal")
               (string=? (car next-token) "mp-lthan")
               (string=? (car next-token) "mp-gthan")
               (string=? (car next-token) "mp-lequal")
               (string=? (car next-token) "mp-gequal")
               (string=? (car next-token) "mp-nequal"))
           (optional-relation-part))
          (else '""))))

;; <relational-operator> -> mp-equal
;;                       -> mp-lthan
;;                       -> mp-gthan
;;                       -> mp-lequal
;;                       -> mp-gequal
;;                       -> mp-nequal
(define (relational-operator)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-equal") '())
          ((string=? (car next-token) "mp-lthan") '())
          ((string=? (car next-token) "mp-gthan") '())
          ((string=? (car next-token) "mp-lequal") '())
          ((string=? (car next-token) "mp-gequal") '())
          ((string=? (car next-token) "mp-nequal") '())
          (else (format #t "parse error: expected a relation but found ~a at [~a:~a]~%"
                        (car next-token) (caddr next-token) (cadddr next-token))
                (exit)))))

;; <simple-expression> -> <optional-sign> . <term> . <term-tail>
(define (simple-expression)
  (optional-sign)
  (term)
  (term-tail))

;; <term-tail> -> <adding-operator> . <term> . <term-tail>
;;             -> eps
(define (term-tail)
  (let ((next-token (get-token)))
    (cond ((or (string=? (car next-token) "mp-plus")
               (string=? (car next-token) "mp-minus")
               (string=? (car next-token) "mp-or"))
           (adding-operator)
           (term)
           (term-tail))
          (else '""))))

;; <optional-sign> -> mp-plus
;;                 -> mp-minus
;;                 -> eps
(define (optional-sign)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-plus") '())
          ((string=? (car next-token) "mp-minus") '())
          (else '""))))

;; <adding-operator> -> mp-plus
;;                   -> mp-minus
;;                   -> mp-or
(define (adding-operator)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-plus") '())
          ((string=? (car next-token) "mp-minus") '())
          ((string=? (car next-token) "mp-or") '())
          (else (format #t "parse error: expected an adding operator but found ~a at [~a:~a]~%"
                        (car next-token) (caddr next-token) (cadddr next-token))
                (exit)))))

;; <term> -> <factor> . <factor-tail>
(define (term)
  (factor)
  (factor-tail))

;; <factor-tail> -> <multiplying-operator> . <factor> . <factor-tail>
;;               -> eps
(define (factor-tail)
  (let ((next-token (get-token)))
    (cond ((or (string=? (car next-token) "mp-times")
               (string=? (car next-token) "mp-float-divide")
               (string=? (car next-token) "mp-div")
               (string=? (car next-token) "mp-mod")
               (string=? (car next-token) "mp-and"))
           (factor)
           (factor-tail))
          (else '""))))
  
  
;; <multiplying-operator> -> mp-times
;;                        -> mp-float-divide
;;                        -> mp-div
;;                        -> mp-mod
;;                        -> mp-and
(define (multiplying-operator)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-times") '())
          ((string=? (car next-token) "mp-float-divide") '())
          ((string=? (car next-token) "mp-div") '())
          ((string=? (car next-token) "mp-mod") '())
          ((string=? (car next-token) "mp-and") '())
          (else (format #t "parse error: expected a multiplying operator but found ~a at [~a:~a]~%"
                        (car next-token) (caddr next-token) (cadddr next-token))
                (exit)))))

;; <factor> -> mp-integer-lit
;;          -> mp-float-lit
;;          -> mp-string-lit
;;          -> mp-true
;;          -> mp-false
;;          -> mp-not . <factor>
;;          -> mp-lparen . <expression> . mp-rparen
(define (factor)
  (let ((next-token (get-token)))
    (cond ((or (string=? (car next-token) "mp-integer-lit")
               (string=? (car next-token) "mp-float-lit")
               (string=? (car next-token) "mp-string-lit")
               (string=? (car next-token) "mp-true")
               (string=? (car next-token) "mp-false"))
           '())
          ((string=? (car next-token) "mp-not")
           (factor))
          ((string=? (car next-token) "mp-lparen")
           (expression)
           (expect-token "mp-rparen" (get-token )))
          (else (format #t "parse error: expected a factor but found ~a at [~a:~a]~%"
                        (car next-token) (caddr next-token) (cadddr next-token))
                (exit)))))

;; <program-identifier> -> mp-identifier
(define (program-identifier)
  (expect-token "mp-identifier" (get-token)))

;; <variable-identifier> -> mp-identifier
(define (variable-identifier)
  (expect-token "mp-identifier" (get-token)))

;; <procedure-identifier> -> mp-identifier
(define (procedure-identifier)
  (expect-token "mp-identifier" (get-token)))

;; <function-identifier> -> mp-identifier
(define (function-identifier)
  (expect-token "mp-identifier" (get-token)))

;; <boolean-expression> -> <expression>
(define (boolean-expression)
  (expression))

;; <ordinal-expression> -> <expression>
(define (ordinal-expression)
  (expression))

;; <identifier-list> -> mp-identifier . <identifier-tail>
(define (identifier-list)
  (expect-token "mp-identifier" (get-token ))
  (identifier-tail))

;; <identifier-tail> -> mp-comma . <identifier> . <identifier-tail>
;;                   -> eps
(define (identifier-tail)
  (bind/cc return
           (begin 
             (let ((next-token (get-token)))
               (cond ((string=? (car (get-token)) "mp-comma")
                      (expect-token "mp-identifier" (get-token))
                      (identifier-tail))
                     (else (return return)))))))


;;---------------------------------------------------------;;
;;                                                         ;;
;; -- Program Main --                                      ;;
;;                                                         ;;
;; This function serves as the entry point to the          ;;
;; micro-pascal compiler project. No functions will be     ;;
;; called outside of this driver function.                 ;;
;;                                                         ;;
;;---------------------------------------------------------;;

(define (main)
  (let ((fp (open-input-file (cadr (command-line)))))
    (init-transition-tables)
    (format #t "Begin program parse -->~%~%")
    (system-goal)
    (format #t "~%")
    ))(main)


;;---------------------------------------------------------;;
;;                                                         ;;
;; -- EOF --                                               ;;
;;                                                         ;;
;;---------------------------------------------------------;;
