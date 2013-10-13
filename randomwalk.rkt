;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname randomwalk) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; random-walk: Nat -> PositiveInteger
;; Determines how many steps it takes to return to the origin in n dimensions
;; Note: You probably don't want to run this for n > 2, there's no guarantee it
;; will ever return to the origin
(define (random-walk n)
  (local ((define (update-list number alist)
            (cond [(= number 0) (cons (add1 (first alist)) (rest alist))]
                  [(= number 1) (cons (sub1 (first alist)) (rest alist))]
                  [else (cons (first alist) (update-list (- number 2) (rest alist)))]))
          (define (step alist k)
            (cond [(andmap zero? alist) k]
                  [else (step (update-list (random (* 2 n)) alist) (add1 k))])))
    (step (update-list (random (* 2 n)) (make-list n 0)) 1)))
;; self-avoiding-walk : Nat -> PositiveInteger
;; Determines how many steps it takes to return to the origin in n dimensions
;; while never stepping in the same place twice
;; Note: You probably don't want to run this in any dimension
(define (self-avoiding-walk n)
  (local ((define (update-list number alist)
            (cond [(= number 0) (cons (add1 (first alist)) (rest alist))]
                  [(= number 1) (cons (sub1 (first alist)) (rest alist))]
                  [else (cons (first alist) (update-list (- number 2) (rest alist)))]))
          (define (step alist pastlocations k)
            (local ((define nextplace (update-list (random (* 2 n)) alist)))
              (cond [(andmap zero? alist) k]
                    [(member? nextplace pastlocations) (step alist pastlocations k)]
                    [else (step nextplace (cons alist pastlocations) (add1 k))]))))
    (step (update-list (random (* 2 n)) (make-list n 0)) empty 1)))
