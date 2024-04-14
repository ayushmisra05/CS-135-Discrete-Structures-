#lang scheme

;;-------------------------------------------------------------------------------
;; Name: Ayush Misra
;; Pledge: I pledge my honor that I have abided by the Stevens Honor System.
;;-------------------------------------------------------------------------------

;; In this lab, you'll implement the Chinese Remainder Theorem
;;   by breaking up the algorithm into several small functions.
;;
;; We'll represent systems of linear congruences with lists of integer pairs,
;;   where the pair (a b) represents the congruence x ≡ a (mod b).
;; For example, the "cong-sys" '((10 11) (4 12) (12 13)) represents the system:
;;   x ≡ 10 (mod 11)
;;   x ≡  4 (mod 12)
;;   x ≡ 12 (mod 13)
;;
;; For any cong-sys passed through to a function in this lab, you may assume:
;;  - The cong-sys is not empty (it contains at least one integer pair).
;;  - All of the moduli are positive (no need to check for modulo 0).


;;-------------------------------------------------------------------------------
;;                               GCD FUNCTIONS
;;-------------------------------------------------------------------------------

;; First, here's a function provided to you which
;;   performs the pulverizer process (Euclid's extended algorithm).
;; This will come in handy for the subsequent functions you write for CRT.
;; Given non-negative integers a and b,
;;   it returns the list '(g s t) where g = gcd(a,b) = s*a + t*b.
;; So, what this function returns is the gcd of a and b, along with
;;   the coefficients of the smallest positive linear combination of a and b.
;;
;; Type Signature: (pulverize int int) -> list
(define (pulverize a b)
  (if (zero? a)
      (list b 0 1)
      (let ([p (pulverize (modulo b a) a)])
        (list (car p)
              (- (caddr p)
                 (* (quotient b a) (cadr p)))
              (cadr p)))))


;; Even though (pulverizer a b) does compute gcd(a,b),
;;   it does so expensively because it also has to
;;   compute the linear combination of a and b.
;; So we should also have a function which only computes the GCD.
;;
;; Implement euclid-gcd, which accepts two integers
;;   and returns their greatest common divisor.
;; Don't use the built-in "gcd" function, or the "pulverize" helper function;
;;   instead, use Euclid's algorithm to efficiently compute the GCD!
;; You may assume the inputs are non-negative.
;;
;; Remember, the "modulo" function in EOPL computes mod!
;;
;; Examples:
;; (euclid-gcd 0 0) -> 0
;; (euclid-gcd 1 0) -> 1
;; (euclid-gcd 5 2) -> 1
;; (euclid-gcd 4 6) -> 2
;; (euclid-gcd 12 9) -> 3
;; (euclid-gcd 30 45) -> 15
;;
;; Type Signature: (euclid-gcd int int) -> int
(define (euclid-gcd a b)
  (if (= b 0)
      a
      (euclid-gcd b (modulo a b)))
  )

;; Now you technically have two functions which compute gcd,
;;   but one is more efficient than the other.
;; To write efficient code, you should only use "pulverize"
;;   when you need the linear combination coefficients;
;;   otherwise use "euclid-gcd" when you need to compute GCD.


;; Before we write the functions to compute CRT,
;;   let's figure out whether CRT can even be computed for a given input.
;; Implement "CRT-exists?" to accept a system of linear congruences
;;   and return a boolean stating if CRT is possible with this system.
;; CRT is possible iff all moduli in the system are pairwise relatively prime.
;; Recall that two numbers are relatively prime if their GCD is 1.
;;
;; For example, the cong-sys '((2 3) (3 5) (2 6))) has moduli 3, 5, and 6,
;;   which are not pairwise relatively prime. So CRT isn't possible.
;; By contrast, the moduli of '((10 11) (4 12) (12 13))) are 11, 12, and 13,
;;   which are pairwise relatively prime. So CRT is possible!
;;
;; You'll have to compare every modulo with every other modulo in the system.
;;
;; Note that in order to do that you can use one of the functions implemented
;; in one of the previous labs. Namely, the function which generates all 2-element
;; subsets of the set of all moduli. For convenience, this function, (subsets k lst)
;; is provided at the end.

;; Examples:
;; (CRT-exists? '((2 3) (3 5) (2 6))) -> #f
;; (CRT-exists? '((10 11) (4 12) (12 13))) -> #t
;; (CRT-exists? '((1 5) (2 14) (5 23) (26 28))) -> #f
;; (CRT-exists? '((1 2) (1 3) (1 5) (1 7) (1 11) (1 13))) -> #t
;;
;; Type Signature: (CRT-exists? cong-sys) -> boolean

(define (CRT-exists? cong-sys)
  (define (pairwise-relatively-prime? lst)
    (cond
      ((null? lst) #t)
      ((null? (cdr lst)) #t)
      (else
       (let* ([moduli (map cadr lst)]
              [pairs (subsets 2 moduli)]
              [gcds (map (lambda (pair) (euclid-gcd (car pair) (cadr pair))) pairs)])
         (andmap (lambda (x) (= x 1)) gcds)
         )
       )
      )
    )
  (pairwise-relatively-prime? cong-sys)
  )


;;-------------------------------------------------------------------------------
;;                               CRT FUNCTIONS
;;-------------------------------------------------------------------------------

;; Now we're ready to make the CRT calculator!


;; First, implement "mul-inv", which accepts non-negative integers a and b,
;;   and returns integer x such that a*x ≡ 1 (mod b).
;; In other words, it returns the modular multiplicative inverse of a (mod b).
;; You may assume that a and b are relatively prime, and that b is not 0.
;;
;; Hint: what process do we use to compute modular multiplicative inverses by hand?
;; What helper function do we have for this?
;; Important: your function should return a positive number
;;
;; Examples:
;; (mult-inv 31 76) -> 27
;; (mult-inv 127 555) -> 118
;; (mult-inv 1234 4321) -> 3239

;; Type Signature: (mul-inv int int) -> int
(define (mult-inv a b)
  (let ([coeffs (pulverize a b)])
    (modulo (cadr coeffs) b)
    )
  )

;; Implement "m", which accepts a system of linear congruences
;;   and returns the value of "m" in the CRT process,
;;   which is the product of all moduli in the system.
;; Hint: look into EOPL's "apply" and "map" functions to make this really easy!
;;
;; Examples:
;; (m '((2 3) (3 5) (2 7))) -> 105
;; (m '((10 11) (4 12) (12 13))) -> 1716
;; (m '((1 5) (2 14) (5 23) (26 27))) -> 43470
;;
;; Type Signature: (m crt-list) -> int
(define (m crt-list)
  (define (multiply-moduli moduli)
    (if (null? moduli)
        1
        (* (cadr (car moduli)) (multiply-moduli (cdr moduli))))
    )
  (multiply-moduli crt-list)
  )


;; Implement CRT-helper, which accepts a valid cong-sys (one where CRT exists)
;;   and m (the product of all the moduli),
;;   and returns the solution to the system via CRT without simplifying.
;;
;; To do this, you need to summate the values of ai*Mi*yi for each pair in the system.
;; For the ith congruence x ≡ ai (mod bi), represented by the pair (ai bi) in the cong-sys:
;;   Mi = m / bi.
;;   yi = the multiplicative inverse of Mi (mod bi).
;;
;; Examples:
;; (CRT-helper '((10 11) (4 12) (12 13)) 1716) -> 26740
;; (CRT-helper '((2 3) (3 5) (2 7)) 105) -> 233
;; (CRT-helper '((1 2) (2 3) (3 5) (4 7) (5 11) (6 13)) 30030) -> 299513
;;
;; Type Signature: (CRT-helper cong-sys int) -> int
(define (CRT-helper cong-sys m)
  (define (compute-yi mi bi)
    (mult-inv mi bi))

  (define (summate-values pairs)
    (foldl (lambda (x acc) (+ (* (car x) (cadr x) (caddr x)) acc)) 0 pairs))

  (let* ((mi-values (map (lambda (pair) (/ m (cadr pair))) cong-sys))
         (yi-values (map (lambda (pair mi bi) (compute-yi mi bi)) cong-sys mi-values (map cadr cong-sys)))
         (ai-values (map car cong-sys))
         (ai-mi-yi-pairs (map list ai-values mi-values yi-values)))
    (summate-values ai-mi-yi-pairs)))


;; Now we'll bring everything together and write the function
;;   to calculate CRT from start to finish.
;;
;; Implement CRT, which accepts a cong-sys which may or may not be valid.
;; First, check if CRT is possible with the given cong-sys.
;; If CRT isn't possible, return -1.
;; If CRT is possible, find the unsimplified solution X to the system with CRT-helper,
;;   then return the simplified solution, which is the smallest positive integer
;;   congruent to X (mod m).
;;
;; Examples:
;; (CRT '((10 11) (4 12) (12 13))) -> 1000
;; (CRT '((2 3) (3 5) (2 7))) -> 23
;; (CRT '((1 2) (2 3) (3 5) (4 7) (5 11) (6 13))) -> 29243
;; (CRT '((1 2) (4 8) (8 9))) -> -1
;;
;; Type Signature: (CRT cong-sys) -> int
(define (CRT cong-sys)
  (if (CRT-exists? cong-sys)
      (let* ((m-value (m cong-sys))
             (solution (CRT-helper cong-sys m-value)))
        (modulo solution m-value))
      -1)
  )



;--------------------------- HELPER FUNCTIONS: DO NOT MODIFY ----------------------------;

;; Returns e ∈ L.
;; Type signature: (element? item list) -> boolean
(define (element? e L)
  (member e L))

;; Returns L as a set (removes duplicates).
;; Type signature: (make-set list) -> set
(define (make-set L)
  (cond [(null? L) '()]
        [(member (car L) (cdr L)) (make-set (cdr L))]
        [else (cons (car L) (make-set (cdr L)))]))

(define (distinct-elems? tuple)
     (= (length (make-set tuple)) (length tuple))
    )

(define (repeat k lst)

   [ if (= k 0)
        '()
         (append (list lst) (repeat (- k 1) lst))
       ]
  )

(define (rcons x object)
      (if (list? object)
           (cons x object)
           (list x object))
  )

(define (pair-up x lst)

   [if (eq? lst '())
       '()
       (append (list (rcons x (car lst))) (pair-up x (cdr lst)))
       ]
         
  )

(define (cartesian-product2 lst1 lst2)

   [if (eq? lst1 '())
        '()
        (append (pair-up (car lst1) lst2) (cartesian-product2 (cdr lst1) lst2))
        ]
  )

(define (cartesian-product list-of-lists)

      [if  (< (length list-of-lists) 2)
           '()
           [if (= (length list-of-lists) 2)
                (cartesian-product2 (car list-of-lists) (cadr list-of-lists))
                ( let [ (l1 (car list-of-lists)) (other-lists (cdr list-of-lists))]
                        (cartesian-product2 l1 (cartesian-product other-lists))
                )
            ]
       ]
  )

;; For simplicity, assume below that k is a positive integer >= 2,  and that the list lst contains only distinct numbers.
;; The "subsets" function should return the list of all possible k-element subsets of the given set (represented
;; as a list).
;;
;; (subsets 2 '(1 2 3 4)) -> '((1 2) (1 3) (2 3) (1 4) (2 4) (3 4))
;; (subsets 2 '(1 2 3)) -> '((1 2) (1 3) (2 3))
;; (subsets 4 '(1 2 3 4)) -> '((1 2 3 4))

(define (subsets k lst)
  (define list-of-lists (repeat k lst))
  (define cart-prod (cartesian-product list-of-lists))
  (define subsets-with-permutations (filter distinct-elems? cart-prod))
  (define (order tuple) (sort tuple <))
  (define repeated-tuples (map order subsets-with-permutations))
  (make-set repeated-tuples)
)
