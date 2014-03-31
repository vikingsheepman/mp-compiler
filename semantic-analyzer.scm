#!/usr/bin/env guile
!#

;;---------------------------------------------------------;;
;;                                                         ;;
;; CSCI-468 Compilers Project                              ;;
;; Phase 3: Semantic-Analyzer                              ;;
;;                                                         ;;
;; Last Modified: 2014-03-15                               ;;
;;                                                         ;;
;; Author: Killian Smith                                   ;;
;;                                                         ;;
;;---------------------------------------------------------;;
;;                                                         ;;
;; The purpose of this program is to provide the utilities ;;
;; to compile a micro-pascal program into the target byte  ;;
;; code.                                                   ;;
;;                                                         ;;
;; This program servers as a utility file for the          ;;
;; micro-pascal parser, and is not a stand alone           ;;
;; progam.                                                 ;;
;;                                                         ;;
;;---------------------------------------------------------;;

(define-module (semantic-analyzer)
  #:use-module (srfi srfi-1)
  #:export (
            ;; symbol table functions
            make-table
            pop-table
            insert-symbol
            lookup-symbol

            ;; semantic functions
            write-init
            write-static-vars

            ;;debug
            display-prog
            display-table
            ))



;;---------------------------------------------------------;;
;;                                                         ;;
;; -- Symbol Table Operations --                           ;;
;;                                                         ;;
;;                                                         ;;
;; table-list -> Contains a complete listing of currently  ;;
;;               active symbol tables.                     ;;
;;                                                         ;;
;; make-table -> Generate a new symbol table.              ;;
;;                                                         ;;
;; pop-table -> Remove the current symbol table.           ;;
;;                                                         ;;
;; insert-symbol -> Insert an entry into the current       ;;
;;                  symbol table.                          ;;
;;                                                         ;;
;; lookup-symbol -> Attempt to find a given symbol inside  ;;
;;                  of any of the active symbol tables.    ;;
;;                                                         ;;
;;---------------------------------------------------------;;

;; global symbol table
(define table-list '())

;; holding cell for current stack offset
(define offset 0)

;; holding cell for current nesting level
(define nesting-level -1)


(define (make-table)
  (set! offset 0)
  (set! nesting-level (+ nesting-level 1))
  (set! table-list (append (list '()) table-list)))

(define (pop-table)
  (set! nesting-level (- nesting-level 1))
  (set! table-list (cdr table-list)))

(define (insert-symbol symbol)
  (let ((current-table (car table-list)))
    (set! table-list
          (cons (cons (append symbol offset) current-table) (cdr table-list))))
  (set! offset (+ offset (cadddr symbol))))

(define (lookup-symbol symbol)
  (define (find val lst)
    (cond ((eq? (cdr lst) '())
           (if (string=? (caar lst) val)
               (car lst)
                '()))
          (else
           (if (string=? (caar lst) val)
               (car lst)
               (find val (cdr lst))))))
  
  (define (_lookup-symbol symbol table)
    (let ((current-table (car table)))
      (let ((symbol-found? (find symbol current-table)))
        (cond ((eq? (cdr table) '())
               (if (eq? symbol-found? '())
                   (begin
                     (format #t "~%symbol '~a' is undeclared~%" symbol)
                     '())
                   symbol-found?))
              (else
               (if (eq? symbol-found? '())
                   (_lookup-symbol symbol (cdr table))
                   symbol-found?))))))
 
  (_lookup-symbol symbol table-list))



;;---------------------------------------------------------;;
;;                                                         ;;
;; -- Pascal Operatives Translations --                    ;;
;;                                                         ;;
;; The below functions provide translations for micro      ;;
;; pascal expressions to micro-machine byte code.          ;;
;;                                                         ;;
;;---------------------------------------------------------;;

;; string holding the translated program
(define prog "")

;; holding cell for current stack offset
(define offset 0)

;; add an instruction to the program
(define (addprog instruction)
  (set! prog
        (cond ((list? instruction)
               (string-append prog (string-join instruction " ")))
              (else
               (string-append prog (string-append instruction "\n"))))))

;; initialize the stack
(define (write-init)
  (addprog "store D0 SP")
  (addprog "add SP 4 SP")
  (addprog "load D0 SP"))

;; reserve space on the stack for static variables,
;; and fill these slots from the symbol table
(define (write-static-vars)
  (let ((tsize (reduce + 0 (map cadddr (car table-list)))))
    (addprog (list "add SP" (number->string tsize) "SP"))))




;; FOR DEBUG
(define (display-prog)
  (format #t "~a~%" prog))

(define (display-table)
  (format #t "~a~%" table-list))

;;---------------------------------------------------------;;
;;                                                         ;;
;; -- EOF --                                               ;;
;;                                                         ;;
;;---------------------------------------------------------;;
