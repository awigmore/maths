;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mergesort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; split-before : [List X] Nat -> [List X]
;; The first n elements of a list
;; n <= (length x)
(define (split-before x n)
  (cond [(= n 0) empty]
        [else (cons (first x) (split-before (rest x) (sub1 n)))]))
(check-expect (split-before '(0 1 2 3 4) 2) '(0 1))
(check-expect (split-before '(0 1 2 3 4) 0) empty)
(check-expect (split-before empty 0) empty)
(check-expect (split-before '(0 1 2 3 4) 5) '(0 1 2 3 4))

;; split-before : [List X] Nat -> [List X]
;; The last (length alist) - n elements
;; n <= (length x)
(define (split-after x n)
  (cond [(= n 0) x]
        [else (split-after (rest x) (sub1 n))]))
(check-expect (split-after '(0 1 2 3 4) 2) '(2 3 4))
(check-expect (split-after '(0 1 2 3 4) 0) '(0 1 2 3 4))
(check-expect (split-after '(0 1 2 3 4) 5) '())

;; merge : [List X] x [List X] [X x X -> Boolean] -> [List X]
;; merge two lists by comp who are already sorted by comp
(define (merge a b comp)
  (cond [(empty? a) b]
        [(empty? b) a]
        [(comp (first a) (first b)) (cons (first a) (merge (rest a) b comp))]
        [else (cons (first b) (merge a (rest b) comp))]))
(check-expect (merge '(234 2 1) '(2 1 1) >=) '(234 2 2 1 1 1))
(check-expect (merge empty '(2 1 1) >=) '(2 1 1))
(check-expect (merge '(234 2 1) empty >=) '(234 2 1))

;; mergesort : [List X] [X x X -> Boolean] -> [List X]
;; Run mergesort
(define (mergesort x comp)
  (cond [(or (empty? x) (empty? (rest x))) x]
        [else (local ([define m (quotient (length x) 2)])
                (merge 
                 (mergesort (split-before x m) comp)
                 (mergesort (split-after x m) comp)
                 comp))]))
(check-expect (mergesort empty >) empty)
(check-expect (mergesort '(1) >) '(1))
(check-expect (mergesort '(1 234 435 23 90324 -0213 -02) >) '(90324 435 234 23 1 -02 -0213 ))

;; mergesort2 : [List X] [X x X -> Boolean] -> [List X]
;; Run mergesort using an accumulator to split the list,
;; but definitely not in place
(define (mergesort2 x comp)
  (cond [(or (empty? x) (empty? (rest x))) x]
        [else (local ([define m (quotient (length x) 2)]
                      [define (acc i a b)
                        (cond [(= i m) (merge (mergesort2 a comp)
                                              (mergesort2 b comp)
                                              comp)]
                              [else (acc (add1 i) (rest a) (cons (first a) b))])])
                (acc 0 x empty))]))
(check-expect (mergesort2 empty >) empty)
(check-expect (mergesort2 '(1) >) '(1))
(check-expect (mergesort2 '(1 234 435 23 90324 -0213 -02) >) '(90324 435 234 23 1 -02 -0213 ))
(define x (build-list 100000 (λ (n) (random 10000))))
;; Time tests indicate mergesort2 is more efficient
(time (first (mergesort x >)))
(time (first (reverse (mergesort2 x >))))