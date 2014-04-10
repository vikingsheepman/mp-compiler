#!/usr/bin/env guile
!#

;;---------------------------------------------------------;;
;;                                                         ;;
;; CSCI-468 Compilers Project                              ;;
;; Phase 3: Semantic-Analyzer                              ;;
;;                                                         ;;
;; Last Modified: 2014-04-08                               ;;
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
            ;; utilities
            write-il-file
            display-prog
            
            ;; symbol table functions
            make-table
            pop-table
            insert-symbol
            insert-proc
            lookup-symbol

            ;; semantic functions
            write-jmp-to-main
            write-label
            write-proc-setup
            write-call
            write-proc-clean
            write-return
            
            write-read
            write-write
            write-wrtln

            write-negop
            write-addop
            write-subop
            write-mulop
            write-divop
            write-modop
            
            write-fnegop
            write-faddop            
            write-fsubop
            write-fmulop
            write-fdivop
            
            write-andop
            write-orop
            write-notop
            
            write-push val
            write-pop val
            
            write-var-space
            write-terminate
           
            ;; for debuging
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

;; holding cell for current available lable
(define label 0)

;; alist that holds data type sizes
;; (does not include string)
(define type-size '(("int" . 4)
                    ("float" . 8)
                    ("bool" . 1)))

;; holding cell for current nesting level
(define nesting-level -1)

;; write a unique label
(define current-label -1)
(define (get-label)
  (string-append "L" (number->string (+ current-label 1))))


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
          (cons (cons (append symbol (list offset)) current-table) (cdr table-list))))
  (set! offset (+ offset (assoc-ref type-size (caddr symbol)))))

(define (insert-proc proc)
  (let ((current-table (car table-list)))
    (set! table-list
          (cons
           (cons
            (append
             (append proc
                     (list -1 -1))
             (list (get-label)))
            current-table)
           (cdr table-list)))))

(define (lookup-symbol symbol)
  (define level 0)
  (define (find val lst)
    (cond ((eq? (cdr lst) '())
           (if (string=? (caar lst) val)
               (list (car lst) level)
               (begin
                 (set! level (+ level 1))
                 '())))
          (else
           (if (string=? (caar lst) val)
               (list (car lst) level)
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

;; return symbol offset
(define (get-offset symbol)
  (cadddr symbol))



;;---------------------------------------------------------;;
;;                                                         ;;
;; -- Pascal Operatives Translations --                    ;;
;;                                                         ;;
;; The below functions provide translations for micro      ;;
;; pascal expressions to micro-machine byte code.          ;;
;;                                                         ;;
;;---------------------------------------------------------;;


;;-- UTILS --

;; string holding the translated program in string format
(define prog "")

;; gets the register of the current scope
(define (get-reg)
  (string-join (list "D" (number->string (- (length table-list) 1)))
               ""))

;; gets the register of the next scope
(define (get-reg+1)
  (string-join (list "D" (number->string (length table-list)))
               ""))

;; add an instruction to the program
(define (addprog instruction)
  (set! prog
        (cond ((list? instruction)
               (string-append prog
                              (string-append (string-join instruction " ")
                                             "\n")))
              (else
               (string-append prog (string-append instruction "\n"))))))




;;-- TRANSLATED CODE --

;; jump to main -- first line in any program
(define (write-jmp-to-main)
  (addprog "br MAIN"))

;; operations
(define (write-negop)
  (addprog "negs"))

(define (write-addop)
  (addprog "adds"))

(define (write-subop)
  (addprog "subs"))

(define (write-mulop)
  (addprog "muls"))

(define (write-divop)
  (addprog "divs"))

(define (write-modop)
  (addprog "mods"))

(define (write-fnegop)
  (addprog "negsf"))

(define (write-faddop)
  (addprog "addsf"))

(define (write-fsubop)
  (addprog "subsf"))

(define (write-fmulop)
  (addprog "mulsf"))

(define (write-fdivop)
  (addprog "divsf"))

(define (write-andop)
  (addprog "ands"))

(define (write-orop)
  (addprog "ors"))

(define (write-notop)
  (addprog "nots"))

;; define push
(define (write-push val)
  (addprog (list "push"
                 val)))

;; define pop
(define (write-pop val)
  (addprog (list "pop"
                 val)))

;; write code for read statement
(define (write-read params)
  (define (read-val val)
    (let ((sym (lookup-symbol val)))
      (addprog (list "rd"
                     (string-join
                      (list (number->string (get-offset (car sym)))
                            "("
                            "D"
                            (cadr sym)
                            ")")
                      "")))))
  (map read-val params))

;; write code for write statement
(define (write-write)
  (addprog "wrts"))

;; write writeln code
(define (write-wrtln)
  (addprog "wrtln #\"\""))

;; reserve space for vars on stack
(define (write-var-space)
  (let ((last-var (car (remove (lambda (x)
                                    (eq? (cadddr x) -1))
                                  (car table-list)))))
    (let ((tsize (+ (cadddr last-var)
                    (assoc-ref type-size (caddr last-var)))))
      (addprog (list "add"
                     (string-append "#" (number->string tsize))
                     "SP SP")))))

;; define steps to setup a call
(define (write-proc-setup)
  (addprog "add #8 SP SP")
  (addprog (list "mov"
                 (get-reg+1)
                 "-4(SP)"))
  (addprog (list "mov SP"
                 (get-reg+1))))

;; define what should hapen at
;; begining of function call
(define (write-proc-head)
  (addprog (string-append "pop"
                          (string-join (list "-8("
                                             (get-reg)
                                             ")") ""))))

;; write a label
(define (write-label proc)
  (if (string=? proc "MAIN")
      (addprog "MAIN:")
      (addprog (string-append (list-ref (car (lookup-symbol proc)) 5)
                              ":"))))

;; cleanup a call
(define (write-proc-clean)
  (addprog (list "mov"
                 (get-reg)
                 "SP"))
  (addprog (list "pop"
                 (get-reg))))

;; write return
(define (write-return)
  (addprog "ret"))

;; write call
(define (write-call proc)
  (addprog (string-append "call "
                          (list-ref (car (lookup-symbol proc)) 5))))


;; terminate program
(define (write-terminate)
  (addprog "hlt"))




;;-- UTILS --

(define (display-prog)
  (format #t "~%~a~%" prog))

(define (display-table)
  (format #t "~%~a~%" table-list))

(define (write-il-file)
  (with-output-to-file "out.il"
    (lambda () (display-prog))))

;;---------------------------------------------------------;;
;;                                                         ;;
;; -- EOF --                                               ;;
;;                                                         ;;
;;---------------------------------------------------------;;
