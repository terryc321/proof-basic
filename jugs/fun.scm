

(import scheme)
(import (chicken format))
(import (chicken sort))

(import procedural-macros)
(import regex)

(import simple-md5)

(import srfi-69)
;; hash-table-ref  hash key thunk
;; hash-table-set! hash key val

;; sudo chicken-install srfi-178
(import srfi-178)
;; srfi-178 provides bit-vectors


;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))


(import sequences)

(import srfi-1)

(import matchable)

;;------------------------- code -----------------------------------

;; (define input (call-with-input-file "day21/input"
;; 		(lambda (port)
;; 		  (read port))))

;; 
;;(iter)


#|

fill small jug from tap
fill big jug from tap
empty small jug completely
empty big jug completely
transfer contents small jug to big jug
transfer contents big jug to small jug

win state : a = 4 \/ b = 4
since a can only hold 3 must be
win state : b = 4
all win states fulfill requirement to measure out 4 gallons
(0,4) (1,4) (2,4) (3,4)


|#

(define (states)
  (let ((counter 0)
	(res '()))
    (letrec ((foo (lambda (i j)
		    (cond
		     ((> i 3) (foo 0 (+ j 1)))
		     ((> j 5) counter)
		     (#t
		      (set! counter (+ counter 1))
		      (set! res (cons `(,i ,j) res))
		      ;;(format #t "jug state ~a : (~a , ~a)~%" counter i j)
		      (foo (+ i 1) j))))))
      (foo 0 0)
      res)))




;; why is the amount liquid in a jug a whole integer ?
;; proof ?

;; what is the size of the state space for this problem ?
;; guess is 24
;; small jug can be 0 1 2 3 - 4 different states
;; big jug can be 0 1 2 3 4 5  - 6 different states  

(define (fill-small s)
  (match s
    ((a b) `(3 ,b))))

(define (fill-big s)
  (match s
    ((a b) `(,a 5))))

(define (empty-small s)
  (match s
    ((a b) `(0 ,b))))
  
(define (empty-big s)
  (match s
    ((a b) `(,a 0))))

;; most put into big is min { liquid in small , empty capacity in big }
(define (small-to-big s)
  (match s
    ((a b) (let* ((empty-space-big (- 5 b))
		  (liquid-in-small a)
		  (transfer-amount (min liquid-in-small empty-space-big))
		  (leftover-a (- a transfer-amount))
		  (new-b (+ b transfer-amount)))
	     `(,leftover-a ,new-b)))))


;; most put into small is min { liquid in big , empty capacity in small }
(define (big-to-small s)
  (match s
    ((a b) (let* ((empty-space-small (- 3 a))
		  (liquid-in-big b)
		  (transfer-amount (min liquid-in-big empty-space-small))
		  (leftover-b (- b transfer-amount))
		  (new-a (+ a transfer-amount)))
	     `(,new-a ,leftover-b)))))


;; longest traversal across state space without hitting somewhere already been ?
;; how many solutions
;; shortest solutions ?
;; longest solutions ?

;; reachable states - 16 of them
;; 8 unreachable states -- see unreachable below
;; total of 24 states in total
(define reachable '())
;;  (SMALL-JUG BIG-JUG)
;; ((0 4) (3 1) (0 1) (1 0) (1 5) (3 3) (0 3) (3 4) (2 5)
;;  (2 0) (0 2) (3 2) (0 5) (3 5) (3 0) (0 0))

(define solutions '())

(define (search2 s path)
  (let ((have-solution #f))
    (format #t "state ~a : ~a ~%" s path)
    
    (match s  ;; pattern match soluton 
      ((a b) (when (= b 4)
	       (set! have-solution #t)
	       (let ((sol-path (reverse (cons s path))))
		 (format #t "solution !! ~a ~%" sol-path)
		 (set! solutions (cons sol-path solutions))))))
    ;;
    (cond ;; reachability
     ((member s reachable) #f)
     (#t (set! reachable (cons s reachable))))
    ;;
    (cond ;; next state 
     ((member s path) #f) ;; prevent infinite loop
     (have-solution #f) ;; no further investigation needed
     (#t (search2 (fill-small s) (cons s path))
	 (search2 (fill-big s) (cons s path))
	 (search2 (empty-small s) (cons s path))
	 (search2 (empty-big s) (cons s path))
	 (search2 (big-to-small s) (cons s path))
	 (search2 (small-to-big s) (cons s path))))))

(define (search)
  (set! reachable '())
  (set! solutions '())
  (search2 '(0 0) '())
  (set! solutions (sort solutions
			(lambda (x y)(< (length x)(length y))))))


(define (unreachable)
  (search)
  (filter (lambda (s) (not (member s reachable)))
	  (states)))
;; 2 jugs these states are un-reachable
;; ((2 4) (1 4) (2 3) (1 3) (2 2) (1 2) (2 1) (1 1))

;; begin ... #t wrap is simply to silence map 
(define (show-solutions)
  (begin
    (map (lambda (x) (format #t "solution ~a ~%" x)) solutions)
    #t))


#|

fastest jug solution

fill 5 gallon
pour 5 gallon into 3 gallon
empty 3 gallon
pour 5 gallon contents into 3 gallon (2 gallon)
fill 5 gallon
pour 5 gallon into 3 gallon (take 1 gallon to fill 3 gallon)
leaving 4 gallon in 5 gallon jug

solution ((0 0) (0 5) (3 2) (0 2) (2 0) (2 5) (3 4)) 

solution ((0 0) (3 0) (0 3) (0 5) (3 2) (0 2) (2 0) (2 5) (3 4)) 
solution ((0 0) (3 0) (0 3) (3 3) (1 5) (1 0) (0 1) (3 1) (0 4)) 
solution ((0 0) (3 0) (3 5) (0 5) (3 2) (0 2) (2 0) (2 5) (3 4)) 
solution ((0 0) (0 5) (3 2) (3 0) (0 3) (3 3) (1 5) (1 0) (0 1) (3 1) (0 4)) 
solution ((0 0) (0 5) (3 5) (3 0) (0 3) (3 3) (1 5) (1 0) (0 1) (3 1) (0 4)) 
solution ((0 0) (3 0) (0 3) (3 3) (1 5) (0 5) (3 2) (0 2) (2 0) (2 5) (3 4)) 
solution ((0 0) (3 0) (0 3) (3 3) (3 5) (0 5) (3 2) (0 2) (2 0) (2 5) (3 4)) 
solution ((0 0) (0 5) (3 2) (3 5) (3 0) (0 3) (3 3) (1 5) (1 0) (0 1) (3 1) (0 4)) 
solution ((0 0) (3 0) (0 3) (3 3) (1 5) (3 5) (0 5) (3 2) (0 2) (2 0) (2 5) (3 4)) 
solution ((0 0) (0 5) (3 2) (0 2) (2 0) (3 0) (0 3) (3 3) (1 5) (1 0) (0 1) (3 1) (0 4)) 
solution ((0 0) (3 0) (0 3) (3 3) (1 5) (1 0) (0 1) (0 5) (3 2) (0 2) (2 0) (2 5) (3 4)) 
solution ((0 0) (0 5) (3 2) (0 2) (2 0) (2 5) (3 5) (3 0) (0 3) (3 3) (1 5) (1 0) (0 1) (3 1) (0 4)) 
solution ((0 0) (3 0) (0 3) (3 3) (1 5) (1 0) (0 1) (3 1) (3 5) (0 5) (3 2) (0 2) (2 0) (2 5) (3 4)) 
#t


|#
