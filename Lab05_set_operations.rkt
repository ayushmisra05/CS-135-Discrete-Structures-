#lang eopl

;;-------------------------------------------------------------------------------
;; Name: Ayush Misra
;; Pledge: "I pledge my honor that I have abided by the Stevens Honor System."
;;-------------------------------------------------------------------------------


;; In this lab, you'll write some basic functions which operate on sets.
;; Scheme doesn't have a built-in "set" datatype,
;;   so we'll use the list datatype as a stand-in for sets.
;; We'll make sure these lists don't contain duplicates,
;;   and we won't care about the order of elements in the lists,
;;   so for all intents and purposes they'll be sets!

;; So, in this lab, we can treat sets as lists,
;;   but we can't treat regular lists as sets.
;; Here's a helper function which converts a list
;;   to a set by removing duplicate elements.
;; It'll be useful for the subsequent functions you implement.

;; Type signature: (make-set list) -> set
(define (make-set L)
  (cond [(null? L) '()]
        [(member (car L) (cdr L)) (make-set (cdr L))]
        [else (cons (car L) (make-set (cdr L)))]))

;;_______________________________________________________________________________


;; As you complete the following functions,
;;   see if you can utilize them in the subsequent ones!
;; Take particular note of the inputs and outputs of each function
;;   to confirm whether the expected types are lists or sets.
;; For all of the functions whose outputs are sets,
;;   your outputs will be considered correct if they contain the correct elements,
;;   regardless of their order.




;; Define element?
;; Given an item e and a list of items L,
;;   returns #t if e is in L, #f otherwise.
;; Hint: to compare equality of any object type, use "equal?".

;; Examples:
;; (element? 0 '()) => #f
;; (element? 8 '(7 8 9)) => #t
;; (element? 7 '(1 2 3 4)) => #f
;; (element? 'saw '(the man saw a dog)) => #t
;; (element? 'sa '(the man saw a dog)) => #f

;; Type signature: (element? item list) -> boolean
(define (element? e L)
  (cond [(null? L) #f]
        [(equal? e (car L)) #t]
        [else (element? e (cdr L))]))





;; Define union
;; Given two lists, returns a set containing all of the elements from either list.
;; Remember: for the functions that return a set,
;;   the order of the elements in the output doesn't need to exactly match the examples.

;; Examples:
;; (union '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6) 
;; (union '(1 2 3) '(0 1 2 3)) => '(0 1 2 3)
;; (union '(1 1 1) '()) => '(1)

;; Type signature: (union list list) -> set
(define (union LA LB)
  (make-set (append LA LB)))





;; Define intersection
;; Given two lists A and B, returns the set
;;   containing all elements in both A and B.

;; Examples:
;; (intersection '(1 2 3 4) '(2 4 5)) => '(2 4)
;; (intersection '(s a n d e e p) '(b h a t t)) => '(a)
;; (intersection '(c c c) '(c a c)) => '(c)
;; (intersection '(a a a) '()) => '()

;; Type signature: (intersection list list) -> set
(define (intersection LA LB)
  (define (element-in-set? elem set)
    (element? elem set))

  (define (intersection-helper source result)
    (cond [(null? source) (make-set result)]
          [(element-in-set? (car source) LB)
           (intersection-helper (cdr source) (if (element? (car source) result)
                                                result
                                                (cons (car source) result)))]
          [else
           (intersection-helper (cdr source) result)]))

  (intersection-helper (reverse LA) '()))


;; Define subset?
;; Given two sets A and B, returns whether A is a subset of B
;;   (every element in A is also in B).

;; Examples:
;; (subset? '() '()) => #t
;; (subset? '(1 2 3) '(1 4 2 5 3)) => #t
;; (subset? '(115 284 385) '(115 146 284 135 385 392)) => #t
;; (subset? '(-2 0 2) '(-1 1 3)) => #f
;; (subset? '(-1 1 2) '(-1 1 3 5 7)) => #f
;; (subset? '(1 3 2) '(3 2 1)) => #t

;; Type signature: (subset? set set) -> boolean
(define (subset? SA SB)
  (null? (set-difference SA SB)))






;; Define set-equal?
;; Given two sets A and B, returns whether A = B
;;   (i.e. every element in A is in B and every element in B is in A).
;; NOTE: Since the order of elements in the sets may be different,
;;   you can't simply use (equal? A B).

;; Examples:
;; (set-equal? '() '()) => #t
;; (set-equal? '(a b c) '(a b c)) => #t
;; (set-equal? '(1 2 3 4) '(4 3 1 2)) => #t
;; (set-equal? '(1 2 3) '(1 2 4)) => #f
;; (set-equal? '(5 5 5 5) '(5)) => #t

;; Type signature: (set-equal? set set) -> boolean
(define (set-equal? SA SB)
  (and (null? (set-difference SA SB))
       (null? (set-difference SB SA))))






;; Define set-difference
;; Given lists A and B, returns the set of A - B
;;   (i.e. every element in A which is not in B).

;; Examples:
;; (set-difference '(1 2 3) '(2 3 4)) => '(1)
;; (set-difference '(1 2 3) '(1 2 3)) => '()
;; (set-difference '(1 2 3) '(4 5 6)) => '(1 2 3)
;; (set-difference '() '(1 2 3))      => '()
;; (set-difference '(1 1 2 3 3) '())  => '(1 2 3)

;; Type signature: (set-difference list list) -> set
(define (set-difference LA LB)
  (define (not-in-LB? elem LB)
    (cond [(null? LB) #t]
          [(equal? elem (car LB)) #f]
          [else (not-in-LB? elem (cdr LB))]))

  (define (insert-ascending elem sorted)
    (cond [(null? sorted) (list elem)]
          [(<= elem (car sorted)) (cons elem sorted)]
          [else (cons (car sorted) (insert-ascending elem (cdr sorted)))]))

  (define (insertion-sort lst)
    (define (insertion-sort-helper source sorted)
      (if (null? source)
          sorted
          (insertion-sort-helper (cdr source) (insert-ascending (car source) sorted))))
    
    (insertion-sort-helper lst '()))

  (define sorted-LA (insertion-sort LA))

  (define (set-difference-helper source LB result)
    (cond [(null? source) (make-set result)]
          [(not-in-LB? (car source) LB)
           (set-difference-helper (cdr source) LB (insert-ascending (car source) result))]
          [else
           (set-difference-helper (cdr source) LB result)]))

  (set-difference-helper sorted-LA LB '()))






;; Define sym-diff
;; Given two lists A and B, returns the symmetric difference of A and B as sets,
;;   which equals the union of A - B and B - A (i.e. every element in exactly one of the lists).

;; Examples:
;; (sym-diff '(1 2 3) '(3 4 5)) => '(1 2 4 5)
;; (sym-diff '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6)
;; (sym-diff '(1 2 3) '(1 2 3)) => '()
;; (sym-diff '(1 2) '(1 2 3 4)) => '(3 4)
;; (sym-diff '(1 1 1) '()) => '(1)

;; Type signature: (sym-diff list list) -> set
;; Examples:
;; (sym-diff '(1 2 3) '(3 4 5)) => '(1 2 4 5)
;; (sym-diff '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6)
;; (sym-diff '(1 2 3) '(1 2 3)) => '()
;; (sym-diff '(1 2) '(1 2 3 4)) => '(3 4)
;; (sym-diff '(1 1 1) '()) => '(1)

;; Type signature: (sym-diff list list) -> set
(define (sym-diff LA LB)
  (define (element-in-set? elem set)
    (cond [(null? set) #f]
          [(equal? elem (car set)) #t]
          [else (element-in-set? elem (cdr set))]))

  (define (sym-diff-helper set1 set2)
    (cond [(null? set1) set2]
          [(element-in-set? (car set1) set2)
           (sym-diff-helper (cdr set1) (remove (car set1) set2))]
          [else
           (sym-diff-helper (cdr set1) (cons (car set1) set2))]))

  (make-set (sym-diff-helper (reverse LA) LB)))




;; Define cardinality
;; Given a list L, returns |L|,
;;   the number of unique elements in L.

;; Examples:
;; (cardinality '(1 2 3)) => 3
;; (cardinality '(1 1 2 3 3)) => 3
;; (cardinality '(5 5 5 5 5)) => 1
;; (cardinality '()) => 0

;; Type signature: (cardinality list) -> int
(define (cardinality L)
  (length (make-set L)))





;; Define disjoint
;; Given two sets, returns if the sets are disjoint
;;   (i.e. they have no elements in common).

;; Examples:
;; (disjoint? '(1 2 3) '()) => #t
;; (disjoint? '(1 2 3) '(1)) => #f
;; (disjoint? '(1 2 3) '(4 5 6)) => #t

;; Type signature: (disjoint? set set) -> boolean
(define (disjoint? SA SB)
  (null? (intersection SA SB)))





;; Define superset?
;; Given sets A and B, returns whether A is a superset of B
;;   (i.e. every element in B is in A).

;; Examples:
;; (superset? '() '()) => #t
;; (superset? '(1 2 3 4 5) '(1 2 3)) => #t
;; (superset? '(-1 1 3) '(-2 0 2)) => #f

;; Type signature: (superset? set set) -> boolean
(define (superset? SA SB)
  (subset? SB SA))





;; Define insert
;; Given a list L and an item e,
;;   returns the set of L with e added to the set.
;; Remember, if the new element was already in the set,
;;   the resultant set does not change.

;; Examples:
;; (insert 0 '(1 2 3)) => '(0 1 2 3)
;; (insert 1 '(1 2 3)) => '(1 2 3)
;; (insert 0 '(0 0 0)) => '(0)

;; Type signature: (insert element list) -> set
(define (insert e L)
  (make-set (if (element? e L) L (cons e L))))





;; Define remove
;; Given a set S and an item e,
;;   returns S with e removed from it.
;; If e was not in S to begin with,
;;   the function should return S unchanged.

;; (remove 2 '(1 2 3)) => '(1 3)
;; (remove 3 '(3))     => '()
;; (remove 4 '(1 2 3)) => '(1 2 3)

;; Type signature: (remove element set) -> set
(define (remove e S)
  (define (remove-helper source result)
    (cond [(null? source) (reverse result)]
          [(equal? (car source) e)
           (remove-helper (cdr source) result)]
          [else
           (remove-helper (cdr source) (cons (car source) result))]))

  (make-set (remove-helper S '())))







;; Created January 2018 by Samuel Kraus and Edward Minnix
;; Updated February 2020 by Jared Pincus