#!/usr/bin/env guile
!#

;;------------------------------------------------------;;
;;                                                      ;;
;; -- Initialize Program --                             ;;
;;                                                      ;;
;;------------------------------------------------------;;

(load "scanner.scm")

(init-transition-tables)


;;------------------------------------------------------;;
;;				       			                                  ;;
;; -- Helper Functions --                               ;;
;;                                                      ;;
;;------------------------------------------------------;;

;; preform desired function if expected token is found
(define (expect-token expected token)
  (cond ((string=? (car token) expected) '())
        (else (format #t "parse error: expected ~a but found ~a at [~a:~a]~%"
                      expected (car token) (caadr token) (caaadr token))
              (exit))))


;;------------------------------------------------------;;
;;				       			                                  ;;
;; -- Declare Non-terminal parser stubs --              ;;
;;                                                      ;;
;;------------------------------------------------------;;

;; <system-goal> -> <program> . EOF
(define (system-goal)
  (program)
  (expect-token '"mp-eof" (get-token) (format #t "program succesfully parsed~%")))

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
;;                . <statement-part>
(define (block)
  (variable-declaration-part)
  (procedure-and-function-declaration-part)
  (statement))

;; <variable-declaration-part> -> mp-var . <variable-declaration> . mp-scolon
;;                                    . <variable-declaration-tail>
;;                             -> eps
(define (variable-declaration-part)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-var") 
           (variable-declaration)
           (expect-token '"mp-scolon" (get-token))
           (variable-declaration-tail))
          (else '""))))

;; <variable-declaration-tail> -> <variable-declaration> . mp-scolon
;;                                    . <variable-declaration-tail>
;;                             -> eps
(define (variable-declaration-tail)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-identifier")
           (variable-declaration)
           (expect-token '"mp-scolon" (get-token))
           (variable-declaration-tail))
          (else '"" ))))

;; <variable-declaration> -> <identifier-list> . mp-colon . <type>
(define (variable-declaration)
  (identifer-list)
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
          (else (format #t "parse error: expected a type but found ~a at [~a:~a]~%"
                        (car token) (caadr token) (caaadr token))
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
(define (optional-formal-paramater-list)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-lparen")
           (formal-parameter-section)
           (formal-parameter-section-tail)
           (expect-token "mp-rparen" (get-token)))
          (else '""))))

;; <formal-parameter-section-tail> -> mp-scolon . <formal-parameter-section>
;;                                        . <formal-parameter-section-tail>
;;                                 -> eps
(define (formal-parameter-section-tail)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-scolon")
           (formal-parameter-section)
           (formal-parameter-section-tail))
          (else '""))))

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
                   (car next-token) (caadr next-token) (caaadr next-token))
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
(define (statement-tail)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-scolon") 
           (statement)
           (statement-tail))
          (else '""))))

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
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-begin") (compound-statement))
          ((string=? (car next-token) "mp-read") (read-statement))
          ((string=? (car next-token) "mp-write") (write-statement))
          ((string=? (car next-token) "mp-writeln") (write-statement))
          ((string=? (car next-token) "mp-if") (if-statement))
          ((string=? (car next-token) "mp-while") (while-statement))
          ((string=? (car next-token) "mp-repeat") (repeat-statement))
          ((string=? (car next-token) "mp-for") (for-statement))
          ((string=? (car next-token) "mp-identifier")
           (cond ((string=? (car (get-token)) "mp-assignment") (assignment-statement))
                 (else (procedure-statement))))
          (else (empty-statement)))))

;; <empty-statement> -> eps
(define (empty-statement)
  '"")

;; <read-statememne> -> mp-read . mp-lparen . <read-parameter>
;;                          . <read-parameter-tail> . mp-rparen
(define (read-statement)
  (expect-token "mp-lparen" (get-token))
  (read-parameter)
  (read-parameter-tail)
  (expect-token "mp-rparen" (get-token)))

;; <read-parameter-tail> -> mp-comma . <read-parameter> . <read-parameter-tail>
;;                       -> eps
(define (read-parameter-tail)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-comma")
           (read-parameter)
           (read-parameter-tail))
          (else '""))))

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
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-comma")
           (write-parameter)
           (write-parameter-tail))
          (else '""))))

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
                        (car token) (caadr token) (caaadr token))
                (exit)))))

;; <final-value> -> <ordinal-expression>
(define (final-value)
  (ordinal-expression))

;; <procedure-statement> ->
(define (procedure-statement)
  )

;; <optional-actual-parameter-list> ->
(define (optional-actual-parameter-list)
  )

;; <actual-parameter-tail> ->
(define (actual-parameter-tail)
  )

;; <actual-parameter> ->
(define (actual-parameter)
  )

;; <expression> ->
(define (expression)
  )

;; <optional-relation-part> ->
(define (optional-relation-part)
  )

;; <relational-operator> ->
(define (relational-operator)
  )

;; <simple-expression> ->
(define (simple-expression)
  )

;; <term-tail> ->
(define (term-tail)
  )

;; <optional-sign> ->
(define (optional-sign)
  )

;; <adding-operator> ->
(define (adding-operator)
  )

;; <term> ->
(define (term)
  )

;; <factor-tail> ->
(define (factor-tail)
  )

;; <multiplying-operator> ->
(define (multiplying-operator)
  )

;; <factor> ->
(define (factor)
  )

;; <program-identifier> ->
(define (program-identifier)
  )

;; <variable-identifier> ->
(define (variable-identifier)
  )

;; <procedure-identifier> ->
(define (procedure-identifier)
  )

;; <function-identifier> ->
(define (function-identifier)
  )

;; <boolean-expression> ->
(define (boolean-expression)
  )

;; <ordinal-expression> ->
(define (ordinal-expression)
  )

;; <identifier-list> ->
(define (identifier-list)
  )

;; <identifier-tail> ->
(define (identifier-tail)
  )

;; main function
(define (main)
  
  )(main)
