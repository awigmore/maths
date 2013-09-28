;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |majority voting research|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Majority voting research

;; A World is a list of Voters
;; A Voter is a (make-voter Nat Nat Boolean)
;; Where the two numbers do not equal each other,
;; nor the voter's place in the list,
;; and neither exceeds the length of the world - 1
(define-struct voter (n1 n2 s))
;; World-Maker : Pos -> World
;; Makes a world with n many players
;; N >= 3
(define (world-maker n)
  (local ((define max  n)
          (define (world-maker n)
            (cond [(zero? n) empty]
                  [else (cons (make-a-voter (- max n) max) (world-maker (sub1 n)))])))
    (world-maker n)))
(define make-world world-maker)

;; make-a-voter : Nat x Nat -> Voter
;; Makes a voter where neither number
;; is the first number
;; nor exceeds the first number
;; with random initial state
(define (make-a-voter place max)
  (local ((define random1 (random  max))
          (define random2 (random max)))
    (cond [(or (= random1 random2) (= random1 place) (= random2 place)) (make-a-voter place max)]
          [else (make-voter random1 random2 (even? (random 2)))])))

;; Majority : Boolean x Boolean x Boolean -> Boolean
;; Returns the majority of the booleans
(define (majority b1 b2 b3)
  (or (and b1 b2 b3)
      (and b1 b2)
      (and b1 b3)
      (and b2 b3)))
(check-expect (majority true true true) true)
(check-expect (majority true true false) true)
(check-expect (majority true false true) true)
(check-expect (majority false true true) true)
(check-expect (majority false false true) false)
(check-expect (majority false true false) false)
(check-expect (majority true false false) false)
(check-expect (majority false false false) false)

;; Next-World : World -> World
;; Generates the next World 
(define (next-world world)
  (local ((define old-world world)
          (define (next-world world)
            (cond [(empty? world) empty]
                  [else (cons (update (first world) old-world) (next-world (rest world)))])))
    (next-world world)))
(check-expect (next-world (list (make-voter 4 3 true) (make-voter 4 0 false) (make-voter 3 0 true) (make-voter 1 2 false) (make-voter 0 1 false)))
              (list (make-voter 4 3 false) (make-voter 4 0 false) (make-voter 3 0 true) (make-voter 1 2 false) (make-voter 0 1 false)))
;; Update : Voter World -> Voter
;; Updates the voter into its next state
(define (update voter world)
  (make-voter (voter-n1 voter) (voter-n2 voter) (majority (voter-s (list-ref world (voter-n1 voter)))
                                                          (voter-s (list-ref world (voter-n2 voter)))
                                                          (voter-s voter))))
(check-expect (update (make-voter 4 3 true) (list (make-voter 4 3 true) (make-voter 4 0 false) (make-voter 3 0 true) (make-voter 1 2 false) (make-voter 0 1 false)))
              (make-voter 4 3 false))

;; Reverse-Ref : List X -> Nat
;; Determines the location of X in a list
(define (reverse-ref alist x)
  (local ((define (reverse-ref alist n)
            (cond [(empty? alist) (error 'not-here)]
                  [(equal? (first alist) x) n]
                  [else (reverse-ref (rest alist) (add1 n))])))
    (reverse-ref alist 0)))
(check-expect (build-list 10 (lambda (y) (reverse-ref (build-list 10 (lambda (x) x)) y)))
              (build-list 10 (lambda (x) x)))
;; Alternate : Nat -> World
;; Creates a world with period 2
;; Nat must be greater than 3
(define (alternate n)
  (local ((define amount (cond [(even? n) n]
                               [else (- n 1)]))
          (define (alternate n)
            (cond [(zero? n) empty]
                  [else (cons (make-voter (modulo (sub1 (- amount n)) amount) (modulo (add1 (- amount n))  amount) (even? n))
                              (alternate (sub1 n)))])))
    (cond [(even? n) (alternate n)]
          [else (append (alternate (- n 1)) (list (make-voter 0 (- n 2) (even? (random 2)))))])))
;; Period : World -> Nat
;; Given an initial world, determine the period that results
(define (period world)
  (local ((define (determine-period oldworlds currentworld)
            (cond [(member? currentworld oldworlds) (add1 (reverse-ref oldworlds currentworld))]
                  [else (determine-period (cons currentworld oldworlds) (next-world currentworld))])))
    (determine-period empty world)))

(define alternating (alternate 2))
(check-expect (period alternating) 2)
(check-expect (period (make-world 3)) 1)

;;Force-Period : Nat -> World
;; Force a world of size n that has a period > 1
;; n > 3
(define (force-period n)
  (local ((define world (make-world n)))
    (cond [(> (period world) 1) world]
          [else (force-period n)])))

;; Turns-Before-Period : World -> Nat
;; After how many updates
;; Has the period begun?
(define (turns-before-period world)
  (local ((define (determine-turns oldworlds currentworld)
            (cond [(member? currentworld oldworlds)   (reverse-ref (reverse oldworlds) currentworld)]
                  [else (determine-turns (cons currentworld oldworlds) (next-world currentworld))])))
    (determine-turns empty world)))
(check-expect (turns-before-period alternating) 0)
(check-expect (member? (turns-before-period (make-world 3)) (list 0 1)) true)
;; Total-Possible-Worlds : Nat -> Nat
;; How many possible worlds are there of a given size?
(define (total-possible-worlds n)
  (expt (* (sub1 n) (- n 2)) n))
;; How-Many-Till-Period>1 : Nat -> Nat
;; How many times of making a world of size n does
;; it take until a period if >1 is made?
;; n > 3
(define (how-many-till-period>1 n)
  (local ((define (acc ticker)
            (cond [(> (period (make-world n)) 1) ticker]
                  [else (acc (+ 1 ticker))])))
    (acc 1)))
;; Mean : Listof Number-> Number
;; Mean of a list of numbers
(define (avg alist)
  (/ (apply + alist) (length alist)))
;(time (build-list 10000 (lambda (n) (how-many-till-period>1 4)))) ; cpu time: 26523 real time: 26813 gc time: 329

;(time (build-list 10000 (lambda (n) (how-many-till-period>1 5)))) ; cpu time: 57059 real time: 57572 gc time: 648

; (avg (build-list 10000 (lambda (n) (how-many-till-period>1 6)))) 
; (avg (build-list 10000 (lambda (n) (how-many-till-period>1 7)))) 
; (avg (build-list 10000 (lambda (n) (how-many-till-period>1 8))))
; (avg (build-list 10000 (lambda (n) (how-many-till-period>1 9))))
#|(define results (build-list 100000 (lambda (n) (period (make-world 4)))))
(define themax (apply max results))
(define (counter alist amax)
  (cond [(= amax 0) empty]
        [else (cons (length (filter (lambda (n) (= n amax)) results)) (counter alist (sub1 amax)))]))
(reverse (counter results themax))|#

#|(define (make-all-4rs p)
  (local ((define tflist
             (list (list true true true true)
                   (list true true true false)
                   (list true true false true)
                   (list true true false false)
                   (list true false true true)
                   (list true false true false)
                   (list true false false true)
                   (list true false false false)
                   (list false true true true)
                   (list false true true false)
                   (list false true false true)
                   (list false true false false)
                   (list false false true true)
                   (list false false true false)
                   (list false false false true)
                   (list false false false false)))
          (define choicesfor0 (list
                               (list 1 2)
                               (list 1 3)
                               (list 2 3)))
          (define choicesfor1 (list
                               (list 0 2)
                               (list 0 3)
                               (list 2 3)))
          (define choicesfor2 (list
                               (list 0 1)
                               (list 0 3)
                               (list 1 3)))
          (define choicesfor3 (list
                               (list 0 1)
                               (list 0 2)
                               (list 1 2)))
    (define (make-all-4rs tfcounter 0counter 1counter 2counter 3counter results)
      (cond [(= tfcounter 16) results]
            [(= 3counter 3) (make-all-4rs tfcounter 0counter 1counter (+ 1 2counter) 0 results)]
            [(= 2counter 3) (make-all-4rs tfcounter 0counter (+ 1 1counter) 0 0 results)]
            [(= 1counter 3) (make-all-4rs tfcounter (+ 1 0counter) 0 0 0 results)]
            [(= 0counter 3) (make-all-4rs (+ 1 tfcounter) 0 0 0 0 results)]
            [else (make-all-4rs tfcounter 0counter 1counter 2counter (add1 3counter)
                                (cons 
                                 (list (make-voter 
                                        (first (list-ref choicesfor0 0counter))
                                        (second (list-ref choicesfor0 0counter))
                                        (list-ref (list-ref tflist tfcounter) 0))
                                       (make-voter 
                                        (first (list-ref choicesfor1 1counter))
                                        (second (list-ref choicesfor1 1counter))
                                        (list-ref (list-ref tflist tfcounter) 1))
                                       (make-voter 
                                        (first (list-ref choicesfor2  2counter))
                                        (second (list-ref choicesfor2 2counter))
                                        (list-ref (list-ref tflist tfcounter) 2))
                                       (make-voter 
                                        (first (list-ref choicesfor3 3counter))
                                        (second (list-ref choicesfor3 3counter))
                                        (list-ref (list-ref tflist tfcounter) 3)))
                                 
                                 results))])))
    (make-all-4rs 0 0 0 0 0 empty )))

(define (make-all-5rs p)
  (local ((define tflist
             (list (list true true true true true)
                   (list true true true true false)
                   (list true true true false true)
                   (list true true true false false)
                   (list true true false true true)
                   (list true true false true false)
                   (list true true false false true)
                   (list true true false false false)
                   (list true false true true true)
                   (list true false true true false)
                   (list true false true false true)
                   (list true false true false false)
                   (list true false false true true)
                   (list true false false true false)
                   (list true false false false true)
                   (list true false false false false)
                   (list false true true true true)
                   (list false true true true false)
                   (list false true true false true)
                   (list false true true false false)
                   (list false true false true true)
                   (list false true false true false)
                   (list false true false false true)
                   (list false true false false false)
                   (list false false true true true)
                   (list false false true true false)
                   (list false false true false true)
                   (list false false true false false)
                   (list false false false true true)
                   (list false false false true false)
                   (list false false false false true)
                   (list false false false false false)))
          (define choicesfor0 (list
                               (list 1 2)
                               (list 1 3)
                               (list 1 4)
                               (list 2 3)
                               (list 2 4)
                               (list 3 4)))
          (define choicesfor1 (list
                               (list 0 2)
                               (list 0 3)
                               (list 0 4)
                               (list 2 3)
                               (list 2 4)
                               (list 3 4)))
          (define choicesfor2 (list
                               (list 0 1)
                               (list 0 3)
                               (list 0 4)
                               (list 1 3)
                               (list 1 4)
                               (list 3 4)))
          (define choicesfor3 (list
                               (list 0 1)
                               (list 0 2)
                               (list 0 4)
                               (list 1 2)
                               (list 1 4)
                               (list 2 4)))
            (define choicesfor4 (list
                               (list 0 1)
                               (list 0 2)
                               (list 0 3)
                               (list 1 2)
                               (list 1 3)
                               (list 2 3)))
    (define (make-all-5s tfcounter 0counter 1counter 2counter 3counter 4counter results)
      (cond [(= 32 tfcounter) results]
            [(= 4counter 6) (make-all-5s tfcounter 0counter 1counter 2counter (+ 1 3counter) 0 results)]
            [(= 3counter 6) (make-all-5s tfcounter 0counter 1counter (+ 1 2counter) 0 0 results)]
            [(= 2counter 6) (make-all-5s tfcounter 0counter (+ 1 1counter) 0 0 0 results)]
            [(= 1counter 6) (make-all-5s tfcounter (+ 1 0counter) 0 0 0 0 results )]
            [(= 0counter 6) (make-all-5s (+ 1 tfcounter) 0 0 0 0 0 results)]
            [else (make-all-5s tfcounter 0counter 1counter 2counter  3counter (add1 4counter)
                                (cons 
                                 (list (make-voter 
                                        (first (list-ref choicesfor0 0counter))
                                        (second (list-ref choicesfor0 0counter))
                                        (list-ref (list-ref tflist tfcounter) 0))
                                       (make-voter 
                                        (first (list-ref choicesfor1 1counter))
                                        (second (list-ref choicesfor1 1counter))
                                        (list-ref (list-ref tflist tfcounter) 1))
                                       (make-voter 
                                        (first (list-ref choicesfor2  2counter))
                                        (second (list-ref choicesfor2 2counter))
                                        (list-ref (list-ref tflist tfcounter) 2))
                                       (make-voter 
                                        (first (list-ref choicesfor3 3counter))
                                        (second (list-ref choicesfor3 3counter))
                                        (list-ref (list-ref tflist tfcounter) 3))
                                       (make-voter 
                                        (first (list-ref choicesfor4 4counter))
                                        (second (list-ref choicesfor4 4counter ))
                                        (list-ref (list-ref tflist tfcounter) 4)))
                                 
                                 results)
                                )])))
    (make-all-5s 0 0 0 0 0 0 empty)))


(define results4 (map period (make-all-4rs 1)))
(define results5 (map period (make-all-5rs 1)))

(define themax4 (apply max results4))
(define themax5 (apply max results5))

(define (counter alist amax)
  (cond [(= amax 0) empty]
        [else (cons (length (filter (lambda (n) (= n amax)) alist)) (counter alist (sub1 amax)))]))
(check-expect (reverse (counter results4 themax4)) (list 1266 6 0 24))
(check-expect (reverse (counter results5 themax5)) (list 245472 480 0 2640 240))

;; Question: Does any period p > k exist for any world?

(define (conjecture timestotry)
  (local ((define num (+ (random 26) 7))
          (define world (make-world num)))
    (cond [(= timestotry 0) false]
          [(> (period world) num) world]
          [else (conjecture (sub1 timestotry))])))
(check-expect (conjecture 1000000) false)|#

;; Question: Does a period p<=k exist for a world k?
(define (find-period n p)
  (local ((define (find-period k)
            (cond ((= k 10000000000000) 'nope)
                  ((= p (period (make-world n))) 'found)
                  (else (find-period (add1 k))))))
    (find-period 0)))
(define (find-period-requirement n requirement)
  (local ((define world (make-world n)))
    (cond [(requirement world) world]
          [else (find-period-requirement n requirement)])))

(define (produce-period n p)
  (local ((define testworld (make-world n)))
    (cond [(= p (period testworld)) testworld]
          [else (produce-period n p)])))
(define list6
  (list (list (make-voter 4 1 false) (make-voter 4 2 true) (make-voter 0 3 false) (make-voter 2 4 false) (make-voter 2 3 true) (make-voter 0 3 false))
        (list (make-voter 3 4 true) (make-voter 4 3 false) (make-voter 4 3 false) (make-voter 1 2 true) (make-voter 2 1 true) (make-voter 2 3 false))
        (list (make-voter 3 4 false) (make-voter 0 2 true) (make-voter 3 1 false) (make-voter 0 5 true) (make-voter 1 5 true) (make-voter 4 2 false))
        (list (make-voter 4 3 true) (make-voter 5 3 true) (make-voter 0 3 false) (make-voter 5 2 true) (make-voter 2 1 false) (make-voter 2 1 false))
        (list (make-voter 1 3 true) (make-voter 4 2 false) (make-voter 0 3 false) (make-voter 4 1 true) (make-voter 2 0 false) (make-voter 0 4 false))
        (list (make-voter 5 3 false) (make-voter 0 5 false) (make-voter 1 3 true) (make-voter 4 1 true) (make-voter 5 1 false) (make-voter 4 2 true))))
(check-expect (map period list6) '(1 2 3 4 5 6))
(check-expect (map length list6) (make-list 6 6))

;; Question: Does a period p<=k exist for a world k?
;; Known: In a world of 6, there exists a period of 1-6
;; N.T.S. : A single follower can be placed into a world
;; and the period will remain the same. If it can be shown
;; all periods have pairs that always remain the same as the other
;; this can be shown.
;; Also need to show all worlds of size k have period of size k.
;; FOR EVENS:

#;(list
   (make-voter 9 1 true) (make-voter 8 0 false)
   (make-voter 9 0 true) (make-voter 8 1 false)
   (make-voter 0 2 true) (make-voter 1 3 false)
   (make-voter 0 4 true) (make-voter 1 5 false)
   (make-voter 0 6 true) (make-voter 1 7 false))
;; n >= 6 and is even
(define (make-even-period n)
  (local ((define firstfour (list (make-voter (sub1 n) 1 true)
                                  (make-voter (- n 2) 0 false)
                                  (make-voter (sub1 n) 0 true)
                                  (make-voter (- n 2) 1 false)))
          (define (others k)
            (cond [(= k (- n 4)) empty]
                  [else (cons (make-voter (if (even? k) 0 1) (+ 2 k) (even? k)) (others (add1 k)))])))
    (append firstfour (others 0))))
(define (make-odd-period n)
  (local ((define (make-odd-period k)
            (cond [(= k n) empty]
                  [else (cons (make-voter (modulo (add1 k) n) (modulo (+ (- n 2) k) n) (even? k)) (make-odd-period (add1 k)))])))
    (make-odd-period 0)))
(define (testing-followers n)
  (local ((define world (make-world (+ 3 (random 20)))))
    (cond [(= n 0) 'yay]
          [(not (= (period world) (period (append world (list (make-voter (random (length world)) (random (length world)) (even? (random 2)))))))) world]
          [else (testing-followers (- n 1))])))
(define (make-period n)
  (if (even? n) (make-even-period n) (make-odd-period n)))
(check-expect (testing-followers 500) 'yay)
(define (test-periods max)
  (cond [(= max 3) 'yay]
        [(= max (period (make-period max))) (test-periods (sub1 max))]
        [else max]))
(check-expect (test-periods 50) 'yay)
(define (make-odd-period-wrong n)
  (local ((define (make-odd-period k)
            (cond [(= k n) empty]
                  [else (cons (make-voter (modulo (add1 k) n) (modulo (+ 3 k) n) (even? k)) (make-odd-period (add1 k)))])))
    (make-odd-period 0)))
#;(list
 (make-voter 8 5 false)
 (make-voter 5 3 false)
 (make-voter 1 5 true)
 (make-voter 1 2 true)
 (make-voter 5 7 false)
 (make-voter 3 2 false)
 (make-voter 0 3 true)
 (make-voter 2 6 true)
 (make-voter 7 4 false))