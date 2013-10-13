;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname repeaters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A repeater is a function that
;; Takes a natural number
;; And creates a function that applies
;; A function to an input n times
;; Repeater :  Nat -> [[X -> X] -> [X -> X]]
(define (repeater n)
  (if (= n 0) (λ (f) (λ (x) x))
      (λ (f) (λ (x) (f (((repeater (sub1 n)) f) x))))))
(check-expect (((repeater 12) add1) 4) 16)
(check-expect (((repeater 0) add1) 4) 4)
;; square: Nat -> Nat
;; Squares a number
(define (square x)
  (((repeater x) (λ (n) (+ x n))) 0))
(check-expect (square 3) 9)
;; xtothex : Nat -> Nat
;; x^x
(define (xtothex x)
  (((repeater x) (λ (n) (* x n))) 1))
(check-expect (xtothex 3) 27)
;; succ : Repeater -> Repeater
;; Repeats one more time
;; succ is short for successor
(define (succ r1)
  (λ (f) (λ (x) (f ((r1 f) x)))))
(check-expect (((succ (repeater 2)) add1) 5) 8)
;; add : Repeater x Repeater -> Repeater
;; Adds the two repeaters together
(define (add r1 r2)
  (λ (f) (λ (x) ((r2 f) ((r1 f) x)))))
(check-expect (((add (repeater 2) (repeater 10)) add1) 5) 17)
;; times : Repeater x Repeater -> Repeater
;; Multiplies the two repeaters together
(define (times r1 r2)
  (λ (f) (λ (x) ((r2 (r1 f)) x))))
(check-expect (((times (repeater 2) (repeater 10)) add1) 5) 25)
;; ^ : Repeater x Repeater -> Repeater
;; first repeater ^ second repeater
(define (^ r1 r2)
  (r2 r1))
(check-expect (((^ (repeater 2) (repeater 3)) add1) 5) 13)
;; r->n : Repeater -> Nat
;; Determines how many times this
;; repeater repeats
(define (r->n r)
  ((r add1) 0))
(define double (repeater 2))
(check-expect ((double add1) 0) (r->n double))
(check-expect (r->n (times (repeater 5) (repeater 3))) 15)
(check-expect (r->n (^ (repeater 5) (repeater 3))) 125)
(check-expect (((^ (repeater 0) (repeater 0)) sub1) 0) -1)
(check-expect (r->n (succ double)) 3)

;; ! : Nat -> Nat
;; x!
(define (! x)
  (((repeater (add1 x))
    (λ (q) (if (zero? (first q)) (second q) (list (sub1 (first q)) (* (first q) (second q)))))) 
   (list x 1)))
(check-expect (map ! '(0 1 2 3 4 5)) '(1 1 2 6 24 120))