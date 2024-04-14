#lang racket
;;Name: Ayush Misra
;;Pledge: I pledge my honor that I have abided by the Stevens Honor System
;;---------------------------------------------------------------------
;; In this assignment you will implement sieve of Eratosthenes, a method
;; generating all prime numbers not exceeding a given integer n.
;;
;;
;; The first function simply generates the list of integers 2 <= x <= n.
;; Examples:
;; (list-integers 10) -> '(2 3 4 5 6 7 8 9 10)
;; (list-integers 30) -> '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30)
;; (list-integers 2)  -> '(2)

(define (list-integers n)
  (if (= n 2)
      (list 2)
      (append (list-integers (- n 1)) (list n))))


;; The second function, remove-multiples-of takes as an argument a number, a, and a list of integers, lst.
;; It removes from the given list all *non-trivial* multiples of a. That is, if 2a, 3a, 4a, …, are present in lst,
;; then they are removed, but if a is present, it remains.
;;
;; Examples.
;; (remove-multiples-of 2 '(2 3 4 5 6 7 8 9 10 11 12 13 14)) -> '(2 3 5 7 9 11 13)
;; (remove-multiples-of 3 (list-integers 20)) -> '(2 3 4 5 7 8 10 11 13 14 16 17 19 20)
;; (remove-multiples-of 3 (remove-multiples-of 2 (list-integers 20))) -> '(2 3 5 7 11 13 17 19)

;; Hint. To implement this function, you can take an element of the list, divide it by a and check whether the result
;; is an integer >= 2 (racket has a function integer?)

(define (remove-multiples-of a lst)
  (cond
    [(null? lst) '()]
    [(and (integer? (/ (car lst) a)) (not (= (car lst) a))) 
     (remove-multiples-of a (cdr lst))]
    [else (cons (car lst) (remove-multiples-of a (cdr lst)))]))


;; This function implements Eratosthenes’ algorithm, and it could be challenging to setup a recursive process for it.
;; What can help is to use racket’s “let” construct which allows defining local variables.
;; eratosthenes-sieve on a given integer-list does the following:
;; 1)	runs remove-multiples-of with the *first* element of the given integer-list as the initial argument
;; 2)	a new integer-list, L, is produced in which all non-trivial multiples of the first element have been removed
;; 3)	run remove-multiples-of on the list, L, from 2) but give it the *second* element of L as the functions’ initial argument
;; 4)	a new integer-list, LL, is produced in which there is no non-trivial multiples of the first and second elements.
;; 5)	run remove-multiples-of on the list, LL, from 4) but give it the *third* element of LL as the functions’ initial argument
;; 6)	and so on …

;; You are allowed to implement this function without using the above approach.
;; (eratosthenes-sieve '(9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30)) -> '(9 10 11 12 13 14 15 16 17 19 21 23 25 29)
;; (eratosthenes-sieve (list-integers 20)) -> '(2 3 5 7 11 13 17 19)
;; (eratosthenes-sieve '(6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90)) -> '(6 9 15 21 33 39 51 57 69 87)
;; (eratosthenes-sieve (list-integers 50)) -> '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)

(define (eratosthenes-sieve integer-list)
  (cond
    [(null? integer-list) '()]
    [else (cons (car (remove-multiples-of (car integer-list) integer-list))
                (eratosthenes-sieve (cdr (remove-multiples-of (car integer-list) integer-list))))]))


;; This function should use services of the functions list-integers and eratosthenes-sieve and produce
;; the list of all primes <= than the given number n.
;;
;; Examples.
;; (primes<= 15) -> '(2 3 5 7 11 13)
;; (primes<= 30) -> '(2 3 5 7 11 13 17 19 23 29)
;; (primes<= 50) -> '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)

(define (primes<= n)
  (eratosthenes-sieve (list-integers n)))

;; num-primes<= should simply provide the number of primes <= than the given argument n. 
;; (this should be very easy now)
;;
;; Examples.
;; (num-primes<= 50)    -> 15
;; (num-primes<= 100)   -> 25
;; (num-primes<= 500)   -> 95
;; (num-primes<= 1000)  -> 168
;; (num-primes<= 5000)  -> 669
;; (num-primes<= 10000) -> 1229
;; (num-primes<= 50000) -> 5133

(define (num-primes<= n)
  (length (primes<= n)))
 
;; The below function should simply apply the approximation found by a
;; French mathematician Legendre: # of primes <= n  is approximately equal to floor( n/[ln(n) – 1.08366] )
;;
;; Examples.
;; (prime-num-approx 50)   -> 17
;; (prime-num-approx 100)  -> 28
;; (prime-num-approx 500)  -> 97
;; (prime-num-approx 1000) -> 171
;; (prime-num-approx 5000) -> 672
;; (prime-num-approx 10000)-> 1230
;; (prime-num-approx 50000)-> 5135

(define (prime-num-approx n)
  (floor (/ n (- (log n) 1.08366))))


;;The next approximation was essentially conjectured by K. F. Gauss (although the proof was found only recently).
;; The formula to use has three terms:
;; floor ( n/ln(n) + n/(ln(n)^2) + 2n/(ln(n)^3) )
;;
;; Examples.
;; (prime-num-approx2 50)  ->  17
;; (prime-num-approx2 100) ->  28
;; (prime-num-approx2 500) ->  97
;; (prime-num-approx2 1000) -> 171
;; (prime-num-approx2 5000) -> 672
;; (prime-num-approx2 10000)-> 1229
;; (prime-num-approx2 50000)-> 5127

(define (prime-num-approx2 n)
  (let ((ln-n (log n)))
    (floor (+ (/ n ln-n) (/ n (expt ln-n 2)) (* 2 (/ n (expt ln-n 3)))))))






  