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

(import simple-loops)
;; simple loops gives us do-list 

(import sequences)

(import srfi-1)

(import matchable)

;;------------------------- code -----------------------------------

(define (foo big small)
  (- (+ big small) 3))

(define (bar big small)
  (- small (- 3 big)))

(define (check)
  (do-list (small '(0 1 2 3))
	   (do-list (big '(0 1 2 3 4 5))
		    (assert (= (foo big small)
			       (bar big small))))))


