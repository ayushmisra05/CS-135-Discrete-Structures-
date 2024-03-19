#lang racket
;;-------------------------------------------------------------------------------
;; Name: Ayush Misra
;; Pledge: I pledge my honor that I have abided by the Stevens Honor System
;;-------------------------------------------------------------------------------

;; This lab will build off of the previous lab.
;; At the bottom of this file, you'll find many helper functions,
;;   all from the previous lab, to help you complete this one.

;; Again, like in last lab, we'll use lists to represent sets,
;;   but we won't let there be duplicate elements
;;   and we won't care about the element order.
;; If a function you write outputs a list
;;   and your output doesn't have the same order as the examples, that's okay.
;;
;; Our goal here is to build up from ground up a framework of funcions which
;; allow to generate Cartesian products and lists of all k-element subsets of
;; a given set.
;;
;; We'll start with small functions.

;; distinct-elems? checks whether a tuple's elements are distinct
;; You can use the make-set function to check whether after applying
;; it to a tuple the length of the tuple does not change. When the tuple
;; has less than 2 elements return #t
;;
;; (distinct-elems? '(1 1 3 5)) -> #f
;; (distinct-elems? '(2 7 12)) -> #t
;; (distinct-elems? '(1)) -> #t
;; (distinct-elems? '()) -> #t

(define (distinct-elems? tuple)
  (= (length tuple) (cardinality tuple)))



;; Given a positive integer k and a list, return a list
;; whose elements are k copies of the original list
;;
;; (repeat 3 '(7 10 6)) -> '((7 10 6) (7 10 6) (7 10 6))
;;  (repeat 1 '(2 3 9)) -> '((2 3 9))
;; (repeat 4 '(1)) -> '((1) (1) (1) (1))

(define (repeat k lst)
  (make-list k lst))

;; Write a "robust" version of the cons function. Namely, when the "object" argument
;; below is a list, then the function returns that list with the pre-pended x (hence
;; in this case rcons is indistinguishable from cons. 
;; If the "object" argument is not a list then rcons should return the list (x object)
;;
;; (rcons 4 '(7 9 5)) -> '(4 7 9 5)
;; (rcons 4 2) -> '(4 2)
;; (rcons '(1 2) '(7 8 9)) -> '((1 2) 7 8 9)
;;

(define (rcons x object)
  (if (list? object)
      (append (list x) object)
      (list x object)))


;; The below function shold take an object x as an argument and a list lst = (e1 ..., en).
;; pair-up should create a list of pairs ((x e1) (x e2) ... (x en)) except when
;; an element ei is a list, (rcons x e1) is called and x is put as the first
;; element of ei.
;;
;; (pair-up 3 '(10 11 12)) -> '((3 10) (3 11) (3 12))
;; (pair-up 3 '( (1 2) (7 6) (11 12))) -> '((3 1 2) (3 7 6) (3 11 12))
;; (pair-up 3 '( (1 2) 33 (11 12))) -> '((3 1 2) (3 33) (3 11 12))


(define (pair-up x lst)
  (if (null? lst)
      '()
      (cons (pair-up-helper x (car lst))
            (pair-up x (cdr lst)))))

(define (pair-up-helper x elem)
  (if (list? elem)
      (rcons x elem)
      (list x elem)))

;; The below function should take as arguments two lists and return their
;; Cartesian product ("2" means here that only 2 lists will be worked with).
;; It is very usefull to use the above pair-up function when implementing
;; cartesian-product2. Cartesian product involving an empty list should return
;; an empty list.
;;
;; (cartesian-product2 '(1 2 3) '(a b)) -> '((1 a) (1 b) (2 a) (2 b) (3 a) (3 b))
;; (cartesian-product2 '() '(10 11 12)) -> '()
;; (cartesian-product2 '(a b c) '(10 11 12)) -> '((a 10) (a 11) (a 12) (b 10) (b 11) (b 12) (c 10) (c 11) (c 12))

(define (cartesian-product2 lst1 lst2)
  (cond
    ((null? lst1) '())
    ((null? lst2) '())
    (else
     (append (pair-up (car lst1) lst2)
             (cartesian-product2 (cdr lst1) lst2)))))


;; The below function should return Cartesian product of several lists.
;; The first argument is a list of lists whos product will be calculated. When the
;; number of given lists is < 2 return '(). One possible way of implementing this
;; function is to 1) return cartesian-product2 when the number of lists is 2, and
;; 2) when list-of-lists = (l1 l2 ... ln), return
;;        (cartesian-product2 l1 (cartesian-product (l2 ...ln)))
;;
;; The function should return '() when any of the lists is empty.
;;
;; (cartesian-product '( (1 2 3) (x y) (a b))) ->  '((1 x a) (1 x b) (1 y a) (1 y b) (2 x a) (2 x b) (2 y a) (2 y b) (3 x a) (3 x b) (3 y a) (3 y b))
;; (cartesian-product '( (1 2 3) (x y) ())) -> '()
;; (cartesian-product '( ("A") (x y) ("C"))) -> '(("A" x "C") ("A" y "C"))
;;
;;
(define (cartesian-product list-of-lists)
  (cond ((< (length list-of-lists) 2) '())
        ((null? (car list-of-lists)) '())
        (else (cartesian-product-helper list-of-lists))))

(define (cartesian-product-helper lists)
  (if (null? (cdr lists))
      (car lists)
      (cartesian-product2 (car lists) (cartesian-product-helper (cdr lists)))))

;; For simplicity, assume below that k is a positive integer >= 2,  and that the list lst contains only distinct numbers.
;; The "subsets" function should return the list of all possible k-element subsets of the given set (represented
;; as a list).
;;
;; (subsets 2 '(1 2 3 4)) -> '((1 2) (1 3) (2 3) (1 4) (2 4) (3 4))
;; (subsets 2 '(1 2 3)) -> '((1 2) (1 3) (2 3))
;; (subsets 4 '(1 2 3 4)) -> '((1 2 3 4))
;;
;; You can use the below hints or implement the function in your own way.

(define (subsets k lst)
  ; 1) Generate the list consisting of k copies of the given list
  (define list-of-lists  (repeat k lst))
  
  ; 2) Generate the cartesian product of the lists
  (define cart-prod  (cartesian-product list-of-lists))

  ; 3) Discard the tuples in which there is a pair of repeated elements.
  ; What will be left after this step are the tuples consisting of distinct elements
  ; but they will appear in all possible orders (permutations)
  ; You can use the filter function (hence you must use #lang racket in the top line of
  ; this file)
  (define subsets-with-permutations
    (filter distinct-elems? cart-prod))

  
  ; 4) Let's define a quick nested function which sorts tuples
  ; in the ascending order
  (define (sort-tuple tuple)
    (sort tuple <))

  ; 5) sort each tuple so that, for example, (1 2 3) and (3 1 2) become
  ; (1 2 3) and (1 2 3). Doing that will make all tuples representing a single
  ; set the same. You can use the map function h
  (define repeated-tuples
    (map sort-tuple subsets-with-permutations))

  ; 6) Now eliminate all repeated tuples by using the make-set functionj
     (define subsets-no-repeats
    (make-set repeated-tuples))
  subsets-no-repeats)


;;__________________________________________________________________________

;; Below are helper functions you may utilize for the functions you write!


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

;; Returns the set of LA unioned with the set of LB.
;; Type signature: (union list list) -> set
(define (union LA LB)
  (make-set (append LA LB)))

;; Returns the set of LA intersected with the set of LB.
;; Type signature: (intersection list list) -> set
(define (intersection LA LB)
  (make-set (intersection-helper LA LB)))
(define (intersection-helper LA LB)
  (cond [(null? LA) '()]
        [(element? (car LA) LB)
         (cons (car LA) (intersection-helper (cdr LA) LB))]
        [else (intersection-helper (cdr LA) LB)]))

;; Returns SA ⊆ SB.
;; Type signature: (subset? set set) -> boolean
(define (subset? SA SB)
  (cond [(null? SA) #t]
        [(element? (car SA) SB)
         (subset? (cdr SA) SB)]
        [else #f]))

;; Returns whether SA and SB contain the same elements.
;; Type signature: (set-equal? set set) -> boolean
(define (set-equal? SA SB)
  (and (subset? SA SB)
       (subset? SB SA)))

;; Returns the difference of LA as a set and LB as a set.
;; Type signature: (set-difference list list) -> set
(define (set-difference LA LB)
  (make-set (set-difference-helper LA LB)))
(define (set-difference-helper LA LB)
  (cond [(null? LA) '()]
        [(element? (car LA) LB)
         (set-difference-helper (cdr LA) LB)]
        [else (cons (car LA)
                    (set-difference-helper (cdr LA) LB))]))

;; Returns the symmetric difference of LA as a set and LB as a set.
;; Type signature: (sym-diff list list) -> set
(define (sym-diff LA LB)
  (union (set-difference LA LB)
         (set-difference LB LA)))

;; Returns the cardinality of L as a set.
;; Type signature: (cardinality list) -> int
(define (cardinality L)
  (length (make-set L)))

;; Returns whether sets SA and SB are disjoint.
;; Type signature: (disjoint? set set) -> boolean
(define (disjoint? SA SB)
  (null? (intersection SA SB)))

;; Returns SA ⊇ SB.
;; Type signature: (superset? set set) -> boolean
(define (superset? SA SB)
  (subset? SB SA))

;; Returns the set of L, with e added to it.
;; Type signature: (insert element list) -> set
(define (insert e L)
  (make-set (cons e L)))

;; Returns set S without element e.
;; Type signature: (remove element set) -> set
(define (remove e S)
  (set-difference S (list e)))



;; Created January 2018 by Samuel Kraus and Edward Minnix
;; Updated February 2020 by Jared Pincus
;; Added the Cartesian product part: February 2022 Jacek Ossowski




