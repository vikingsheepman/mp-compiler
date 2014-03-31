#!/usr/bin/env guile
!#

;;---------------------------------------------------------;;
;;                                                         ;;
;; CSCI-468 Compilers Project                              ;;
;; Phase 2: Parser / Driver                                ;;
;;                                                         ;;
;; Last Modified: 2014-03-05                               ;;
;;                                                         ;;
;; Author: Killian Smith                                   ;;
;;                                                         ;;
;;---------------------------------------------------------;;
;;                                                         ;;
;; The purpose of this program is to check if the given    ;;
;; micro-pascal program will produce a valid parse tree    ;;
;; in accordance to the given LL(1) grammar, and if valid, ;;
;; translate into the equivelant bytecode.                 ;;
;;                                                         ;;
;; To execute this program issue the command:              ;;
;;                                                         ;;
;;     ./mp-compile.scm <filename.pas>                     ;;
;;                                                         ;;
;; The program will verify the input file as a valid       ;;
;; micro-pascal progam, or will display any errors that    ;;
;; were encountered.                                       ;;
;;                                                         ;;
;;---------------------------------------------------------;;

;; load modules
(add-to-load-path ".")
(use-modules (srfi srfi-1)
             (ice-9 regex)
             (scanner)
             (semantic-analyzer))



;;---------------------------------------------------------;;
;;                                                         ;;
;; -- Helper Functions --                                  ;;
;;                                                         ;;
;; token-error ->                                          ;;
;;     Prints an error message to the user displaying what ;;
;;     the grammar expected and what was recieved.         ;;
;;     Additionally, the line and column numbers of the    ;;
;;     end of the lexeme are displayed.                    ;;
;;                                                         ;;
;; expect-token ->                                         ;;
;;     Function returns an error message if the expected   ;;
;;     token is not encountered.                           ;;
;;                                                         ;;
;; eps -> Synonym for epsilon (empty string)               ;;
;;                                                         ;;
;; intro-msg -> Displays initial compiler info screen.     ;;
;;                                                         ;;
;;---------------------------------------------------------;;

;; define what to do when an error occurs
(define (token-error expected recieved)
  (format #t "~%parse error: expected ~a but found ~a at [~a:~a]~%~%"
          expected (car recieved) (caddr recieved) (cadddr recieved)))

;; check if expected token is found
(define (expect-token expected token)
  (cond ((string=? (car token) expected) '())
        (else (token-error expected token)
              (exit))))

;; epsilon definition
(define eps "")

;; global variable to hold current operation
(define operation "")

;; welcome mesage
(define (intro-msg)
  (format #t "~%Micro-Pascal Compiler~%")
  (format #t "Version: 1.0~%")
  (format #t "Author: Killian Smith~%")
  (format #t "Last Modified: March 2015~%~%"))



;;---------------------------------------------------------;;
;;                                                         ;;
;; -- Declare Non-terminal parser stubs --                 ;;
;;                                                         ;;
;; The following functions are translations of the given   ;;
;; micro-pascal grammar in EBNF form. The comments above   ;;
;; each function show the parse rule in its original form. ;; 
;;                                                         ;;
;; During the program parse, records are added to the set  ;;
;; of program symbol tables in the form of a list:         ;;
;;                                                         ;;
;;     -> ( lexeme,                                        ;;
;;          kind,                                          ;;
;;          type,                                          ;;
;;          size,                                          ;;
;;          offset                                         ;;
;;          label )                                        ;;
;;                                                         ;;
;;---------------------------------------------------------;;

;; <system-goal> -> <program> . EOF
(define (system-goal)
  ;; setup output bytecode
  (write-init)

  ;; start program parse
  (program)
  (expect-token "mp-eof" (get-token))
  (format #t "The program has parsed successfully!~%~%"))


;; <program> -> <program-heading> . mp-scolon . <block> . mp-period
(define (program)
  ;-- construct new symbol table (begin scope)
  ;-- this will be the root level scope
  (make-table)
  ;-- grammar rule
  (program-heading)
  (expect-token "mp-scolon" (get-token))
  (block)
  ;-- pop the symbol table (end of scope)
  (pop-table)
  ;-- finish grammar rule
  (expect-token "mp-period" (get-token)))


;; <program-heading> -> mp-program . <program-identifier>
(define (program-heading)
  (expect-token "mp-program" (get-token))
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
  (cond ((string=? (car (peek-token)) "mp-var")
         (get-token)
         (variable-declaration)
         (expect-token "mp-scolon" (get-token))
         (variable-declaration-tail))
        (else eps)))


;; <variable-declaration-tail> -> <variable-declaration> . mp-scolon
;;                                . <variable-declaration-tail>
;;                             -> eps
(define (variable-declaration-tail)
  (cond ((string=? (car (peek-token)) "mp-identifier")
         (get-token)
         (variable-declaration)
         (expect-token "mp-scolon" (get-token))
         (variable-declaration-tail))
        (else eps)))


;; <variable-declaration> -> <identifier-list> . mp-colon . <type>
(define (variable-declaration)
  ;; fill in vars in symbol table as grammar is parsed
  (let ((id-list (identifier-list)))
    (expect-token "mp-colon" (get-token))
    (let ((var-type (type)))
      (map (lambda (id)
             (insert-symbol '(id "var" (car var-type) (cadr var-type))))
           id-list)))
  
  ;; semantic call to allocate var definitions to stack
  (write-static-vars)
  
  (display-table))


;; <type> -> mp-integer
;;        -> mp-float
;;        -> mp-string
;;        -> mp-boolean
(define (type)
  (let ((next-token (peek-token)))
    ;; there is no 'char' type in given grammar
    ;; returns tuple of type and size of type
    (cond ((string=? (car next-token) "mp-integer") (get-token) '("int" 4))
          ((string=? (car next-token) "mp-float") (get-token) '("float" 8))
          ((string=? (car next-token) "mp-string") (get-token) '("string" '()))
          ((string=? (car next-token) "mp-boolean") (get-token) '("bool" 1))
          (else (token-error "type" next-token)
                (exit)))))


;; <procedure-and-function-declaration-part> -> <procedure-declaration>
;;                                              . <procedure-and-function-declaration-part>
;;                                           -> <function-declaration>
;;                                              . <procedure-and-function-declaration-part>
;;                                           -> eps
(define (procedure-and-function-declaration-part)
  (let ((next-token (peek-token)))
    (cond ((string=? (car next-token) "mp-procedure")
           (get-token)
           (procedure-declaration)
           (procedure-and-function-declaration-part))
          ((string=? (car next-token) "mp-function")
           (get-token)
           (function-declaration)
           (procedure-and-function-declaration-part))
          (else eps))))


;; <procedure-declaration> -> <procedure-heading> . mp-scolon . <block> . mp-colon
(define (procedure-declaration)
  ;-- construct new symbol table (begin scope)
  (make-table)
  ;-- grammar rule
  (procedure-heading)
  (expect-token "mp-scolon" (get-token))
  (block)
  ;-- pop the symbol table (end of scope)
  (pop-table)
  ;-- finish grammar rule
  (expect-token "mp-scolon" (get-token)))


;; <function-declaration> -> <function-heading> . mp-scolon . <block> . mp-colon
(define (function-declaration)
  ;-- construct new symbol table (begin scope)
  (make-table)
  ;-- grammar rule
  (function-heading)
  (expect-token "mp-scolon" (get-token))
  (block)
  ;-- pop the symbol table (end of scope)
  (pop-table)
  ;-- finish grammar rule
  (expect-token "mp-scolon" (get-token)))


;; <procedure-heading> -> mp-procedure . <procedure-identifier>
;;                        . <optional-formal-parameter-list>
(define (procedure-heading)
  (procedure-identifier)
  (optional-formal-parameter-list))


;; <function-heading> -> mp-function . <function-identifier>
;;                       . <optional-formal-parameter-list>
;;                       . mp-colon . <type>
(define (function-heading)
  (function-identifier)
  (optional-formal-parameter-list)
  (expect-token "mp-colon" (get-token))
  (type))


;; <optional-formal-parameter-list> -> mp-lparen . <formal-parameter-section>
;;                                     . <formal-parameter-section-tail>
;;                                     . mp-rparen
;;                                  -> eps
(define (optional-formal-parameter-list)
  (cond ((string=? (car (peek-token)) "mp-lparen")
         (get-token)
         (formal-parameter-section)
         (formal-parameter-section-tail)
         (expect-token "mp-rparen" (get-token)))
        (else eps)))


;; <formal-parameter-section-tail> -> mp-scolon . <formal-parameter-section>
;;                                    . <formal-parameter-section-tail>
;;                                 -> eps
(define (formal-parameter-section-tail)
  (let ((next-token (peek-token)))
    (cond ((string=? (car next-token) "mp-scolon")
           (get-token)
           (formal-parameter-section)
           (formal-parameter-section-tail))
          (else eps))))


;; <formal-parameter-section> -> <value-parameter-section>
;;                            -> <variable-parameter-section>
(define (formal-parameter-section)
  (let ((next-token (peek-token)))
    (cond ((string=? (car next-token) "mp-identifier")
           (value-parameter-section))
          ((string=? (car next-token) "mp-var")
           (get-token)
           (variable-parameter-section))
          (else
           (token-error "variable or type" next-token)
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
  (cond ((string=? (car (peek-token)) "mp-scolon")
         (get-token)
         (statement)
         (statement-tail))
        (else eps)))


;; <statement> -> <empty-statement>
;;             -> <compound-statement>
;;             -> <read-statement>
;;             -> <write-statement>
;;             -> <writeln-statement>
;;             -> <assign-statement>
;;             -> <if-statement>
;;             -> <while-statement>
;;             -> <repeat-statement>
;;             -> <for-statement>
;;             -> <procedure-statement>
(define (statement)
  (let ((next-token (peek-token))
        (tmp-token '()))
    (cond ((string=? (car next-token) "mp-begin") (compound-statement))
          ((string=? (car next-token) "mp-read") (get-token) (read-statement))
          ((string=? (car next-token) "mp-readln") (get-token) (read-statement))
          ((string=? (car next-token) "mp-write") (get-token) (write-statement))
          ((string=? (car next-token) "mp-writeln") (get-token) eps)
          ((string=? (car next-token) "mp-if") (get-token) (if-statement))
          ((string=? (car next-token) "mp-while") (get-token) (while-statement))
          ((string=? (car next-token) "mp-repeat") (get-token) (repeat-statement))
          ((string=? (car next-token) "mp-for") (get-token) (for-statement))
          ((string=? (car next-token) "mp-identifier")
           (set! tmp-token (get-token))
           (cond ((string=? (car (peek-token)) "mp-assign") (get-token) (assignment-statement))
                 (else (backtrack-token tmp-token) (procedure-statement))))
          (else (empty-statement)))))


;; <empty-statement> -> eps
(define (empty-statement) eps)


;; <read-statement> -> mp-read . mp-lparen . <read-parameter>
;;                     . <read-parameter-tail> . mp-rparen
(define (read-statement)
  (expect-token "mp-lparen" (get-token))
  (read-parameter)
  (read-parameter-tail)
  (expect-token "mp-rparen" (get-token)))


;; <read-parameter-tail> -> mp-comma . <read-parameter> . <read-parameter-tail>
;;                       -> eps
(define (read-parameter-tail)
  (cond ((string=? (car (peek-token)) "mp-comma")
         (get-token)
         (read-parameter)
         (read-parameter-tail))
        (else eps)))


;; <read-parameter> -> <variable-identifier>
(define (read-parameter) (variable-identifier))


;; <write-statement> -> mp-write . mp-lparen . <write-parameter>
;;                      . <write-parameter-tail> . mp-rparen
;;                   -> mp-writeln . mp-lparen . <write-parameter>
;;                      . <write-parameter-tail> . mp-rparen
(define (write-statement)
  (expect-token "mp-lparen" (get-token))
  (write-parameter)
  (write-parameter-tail)
  (expect-token "mp-rparen" (get-token)))


;; <write-parameter-tail> -> mp-comma . <write-parameter> . <write-parameter-tail>
;;                        -> eps
(define (write-parameter-tail)
  (cond ((string=? (car (peek-token)) "mp-comma")
         (get-token)
         (write-parameter)
         (write-parameter-tail))
        (else eps)))


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
  (cond ((string=? (car peek-token) "mp-else") (get-token) (statement))
        (else eps)))


;; <repeat-statement> -> mp-repeat . <statement-sequence> . mp-until
;;                       . <boolean-expression>
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
;;                    . <step-value> . <final-value> . mp-do . <statement>
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
    (cond ((string=? (car next-token) "mp-to") eps)
          ((string=? (car next-token) "mp-downto") eps)
          (else (token-error "step-value" next-token)
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
  (let ((next-token (peek-token)))
    (cond ((string=? (car next-token) "mp-lparen")
           (get-token)
           (actual-parameter)
           (actual-parameter-tail)
           (expect-token "mp-rparen" (get-token )))
          (else eps))))


;; <actual-parameter-tail> -> mp-comma . <actual-parameter> . <actual-parameter-tail>
;;                         -> eps
(define (actual-parameter-tail)
  (let ((next-token (peek-token)))
    (cond ((string=? (car next-token) "mp-comma")
           (get-token)
           (actual-parameter)
           (actual-parameter-tail))
          (else eps))))


;; <actual-parameter> -> <ordinal-expression>
(define (actual-parameter)
  (ordinal-expression))


;; <expression> -> <simple-expression> . <optional-relation-part>
(define (expression)
  (simple-expression)
  (optional-relation-part))


;; <optional-relation-part> -> <relational-operator> . <simple-expression>
;;                          -> eps
(define (optional-relation-part)
  (let ((next-token (peek-token)))
    (cond ((or (string=? (car next-token) "mp-equal")
               (string=? (car next-token) "mp-lthan")
               (string=? (car next-token) "mp-gthan")
               (string=? (car next-token) "mp-lequal")
               (string=? (car next-token) "mp-gequal")
               (string=? (car next-token) "mp-nequal"))
           (relational-operator)
           (simple-expression))
          (else eps))))


;; <relational-operator> -> mp-equal
;;                       -> mp-lthan
;;                       -> mp-gthan
;;                       -> mp-lequal
;;                       -> mp-gequal
;;                       -> mp-nequal
(define (relational-operator)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-equal") eps)
          ((string=? (car next-token) "mp-lthan") eps)
          ((string=? (car next-token) "mp-gthan") eps)
          ((string=? (car next-token) "mp-lequal") eps)
          ((string=? (car next-token) "mp-gequal") eps)
          ((string=? (car next-token) "mp-nequal") eps)
          (else (token-error "relational operator" next-token)
                (exit)))))


;; <simple-expression> -> <optional-sign> . <term> . <term-tail>
(define (simple-expression)
  (optional-sign)
  (term)
  (term-tail))


;; <term-tail> -> <adding-operator> . <term> . <term-tail>
;;             -> eps
(define (term-tail)
  (let ((next-token (peek-token)))
    (cond ((or (string=? (car next-token) "mp-plus")
               (string=? (car next-token) "mp-minus")
               (string=? (car next-token) "mp-or"))
           (adding-operator)
           (term)
           (term-tail))
          (else eps))))


;; <optional-sign> -> mp-plus
;;                 -> mp-minus
;;                 -> eps
(define (optional-sign)
  (let ((next-token (peek-token)))
    (cond ((string=? (car next-token) "mp-plus") (get-token) eps)
          ((string=? (car next-token) "mp-minus") (get-token) eps)
          (else eps))))


;; <adding-operator> -> mp-plus
;;                   -> mp-minus
;;                   -> mp-or
(define (adding-operator)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-plus") eps)
          ((string=? (car next-token) "mp-minus") eps)
          ((string=? (car next-token) "mp-or") eps)
          (else (token-error "adding operator" next-token)
                (exit)))))


;; <term> -> <factor> . <factor-tail>
(define (term)
  (factor)
  (factor-tail))


;; <factor-tail> -> <multiplying-operator> . <factor> . <factor-tail>
;;               -> eps
(define (factor-tail)
  (let ((next-token (peek-token)))
    (cond ((or (string=? (car next-token) "mp-times")
               (string=? (car next-token) "mp-float-divide")
               (string=? (car next-token) "mp-div")
               (string=? (car next-token) "mp-mod")
               (string=? (car next-token) "mp-and"))
           (multiplying-operator)
           (factor)
           (factor-tail))
          (else eps))))
  
  
;; <multiplying-operator> -> mp-times
;;                        -> mp-float-divide
;;                        -> mp-div
;;                        -> mp-mod
;;                        -> mp-and
(define (multiplying-operator)
  (let ((next-token (get-token)))
    (cond ((string=? (car next-token) "mp-times") eps)
          ((string=? (car next-token) "mp-float-divide") eps)
          ((string=? (car next-token) "mp-div") eps)
          ((string=? (car next-token) "mp-mod") eps)
          ((string=? (car next-token) "mp-and") eps)
          (else (token-error "multiplying operator" next-token)
                (exit)))))


;; <factor> -> mp-integer-lit
;;          -> mp-float-lit
;;          -> mp-string-lit
;;          -> mp-true
;;          -> mp-false
;;          -> mp-not . <factor>
;;          -> mp-lparen . <expression> . mp-rparen
;;          -> <function-identifier> . <optional-actual-parameter-list>
(define (factor)
  (let ((next-token (peek-token)))
    (cond ((or (string=? (car next-token) "mp-integer-lit")
               (string=? (car next-token) "mp-float-lit")
               (string=? (car next-token) "mp-string-lit")
               (string=? (car next-token) "mp-true")
               (string=? (car next-token) "mp-false"))
           (get-token) eps)
          ((string=? (car next-token) "mp-not")
           (get-token)
           (factor))
          ((string=? (car next-token) "mp-lparen")
           (get-token)
           (expression)
           (expect-token "mp-rparen" (get-token )))
          ((string=? (car next-token) "mp-identifier")
           (function-identifier)
           (optional-actual-parameter-list))
          (else (token-error "factor" next-token)
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
  (let ((token (get-token))
        (tmp-id-list '()))
    (expect-token "mp-identifier" token)

    ;; append the identifier to the temperary
    ;; identifier table holding list
    (append tmp-id-list (cadr token))

    (identifier-tail tmp-id-list)

    ;; return the list of identifiers
    tmp-id-list))


;; <identifier-tail> -> mp-comma . <identifier> . <identifier-tail>
;;                   -> eps
(define (identifier-tail tmp-id-list)
  (let ((next-token (peek-token)))
    (cond ((string=? (car next-token) "mp-comma")
           (get-token)
           (expect-token "mp-identifier" (get-token))

           ;; append the identifier to the temperary
           ;; identifier table holding list
           (append tmp-id-list (cadr next-token))

           (identifier-tail tmp-id-list))
          (else eps))))



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
  (init-scanner)
  (intro-msg)
  (format #t "Compiling Mico-Pascal Program -->~%~%~%")
  (system-goal)
  (format #t "~%Finished Process~%")
  (format #t "~%"))(main)


;;---------------------------------------------------------;;
;;                                                         ;;
;; -- EOF --                                               ;;
;;                                                         ;;
;;---------------------------------------------------------;;
