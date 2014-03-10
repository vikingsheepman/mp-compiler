#!/usr/bin/env guile
!#

;;---------------------------------------------------------;;
;;                                                         ;;
;; CSCI-468 Compilers Project                              ;;
;; Phase 3: Symantic-Analyzer                              ;;
;;                                                         ;;
;; Last Modified: 2014-03-06                               ;;
;;                                                         ;;
;; Author: Killian Smith                                   ;;
;;                                                         ;;
;;---------------------------------------------------------;;
;;                                                         ;;
;; The purpose of this program is to provide the utilities ;;
;; to compile a micro-pascal program into the target byte  ;;
;; code.                                                   ;;
;;                                                         ;;
;;---------------------------------------------------------;;

(define-module (symbol-table)
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

(define (make-table name)
  (set! table-list (append (list (list name '())) table-list)))

(define (destroy-table)
  (set! table-list (cdr table-list)))

(define (insert-symbol symbol)
  (let ((current-table (car table-list)))
    (append symbol (cadr current-table))))

(define (lookup-symbol symbol)
  '())



;; tests
(make-table "a")
(format #t "~a~%" table-list)

(make-table "b")
(format #t "~a~%" table-list)

(destroy-table)
(format #t "~a~%" table-list)

(insert-symbol '("bc"))
(format #t "~a~%" table-list)
