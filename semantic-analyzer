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
  #:export (make-table destroy-table insert-symbol lookup-symbol))



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
;; destroy-table -> Remove the current symbol table.       ;;
;;                                                         ;;
;; insert-symbol -> Insert an entry into the current       ;;
;;                  symbol table.                          ;;
;;                                                         ;;
;; lookup-symbol -> Attempt to find a given symbol inside  ;;
;;                  of any of the active symbol tables.    ;;
;;                                                         ;;
;;---------------------------------------------------------;;

(define table-list '())

(define (make-table)
  (set! table-list (append (list '()) table-list)))

(define (destroy-table)
  (set! table-list (cdr table-list)))

(define (insert-symbol symbol)
  (let ((current-table (car table-list)))
    (set! table-list
          (cons (cons symbol current-table) (cdr table-list)))))

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
;; -- EOF --                                               ;;
;;                                                         ;;
;;---------------------------------------------------------;;
