;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |random walk|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)
(define (random-walk n)
  (local ((define (update-list number alist)
            (cond [(= number 0) (cons (add1 (first alist)) (rest alist))]
                  [(= number 1) (cons (sub1 (first alist)) (rest alist))]
                  [else (cons (first alist) (update-list (- number 2) (rest alist)))]))
          (define (step alist k)
            (cond [(andmap zero? alist) k]
                  [else (step (update-list (random (* 2 n)) alist) (add1 k))])))
    (step (update-list (random (* 2 n)) (make-list n 0)) 1)))

(define (self-avoiding-walk n)
  (local ((define (update-list number alist)
            (cond [(= number 0) (cons (add1 (first alist)) (rest alist))]
                  [(= number 1) (cons (sub1 (first alist)) (rest alist))]
                  [else (cons (first alist) (update-list (- number 2) (rest alist)))]))
          (define (step alist pastlocations k)
            (local ((define nextplace (update-list (random (* 2 n)) alist)))
              (cond [(andmap zero? alist) (cons alist pastlocations)]
                    [(member? nextplace pastlocations) (step alist pastlocations k)]
                    [else (step nextplace (cons alist pastlocations) (add1 k))]))))
    (step (update-list (random (* 2 n)) (make-list n 0)) empty 1)))

(define (avoid-last-step n)
  (local ((define (update-list number alist)
            (cond [(= number 0) (cons (add1 (first alist)) (rest alist))]
                  [(= number 1) (cons (sub1 (first alist)) (rest alist))]
                  [else (cons (first alist) (update-list (- number 2) (rest alist)))]))
          (define (step alist lastlocation k)
            (local ((define nextplace (update-list (random (* 2 n)) alist)))
              (cond [(andmap zero? alist) k]
                    [(equal? nextplace lastlocation) (step alist lastlocation k)]
                    [else (step nextplace alist (add1 k))]))))
    (step (update-list (random (* 2 n)) (make-list n 0)) (make-list n 0) 1)))
(define (fake-list x)
  (make-list x x))
(define-struct ball (p position))
(define-struct game (ball1 ball2 finishline))
;; A winner is either '1, '2, or 't
;; 1 > (ball-p (game-ball1 game)) > (ball-p (game-ball2 game)) > 0
;; p is the probability the ball will go up
(define (winner game)
  (cond [(= (game-finishline game) (ball-position (game-ball1 game)) (ball-position (game-ball2 game)))
         'tie]
        [(= (game-finishline game) (ball-position (game-ball1 game))) 'one]
        [(= (game-finishline game) (ball-position (game-ball2 game))) 'two]
        [else (winner (make-game (bounce (game-ball1 game)) (bounce (game-ball2 game)) (game-finishline game)))]))
(define (bounce ball)
  (local ((define p (ball-p ball))
          (define n (numerator p))
          (define d (denominator p))
          (define up? (< (random d) n)))
    (cond [(= 0 (ball-position ball)) (make-ball (ball-p ball) 1)]
          [up? (make-ball (ball-p ball) (add1 (ball-position ball)))]
          [else (make-ball (ball-p ball) (sub1 (ball-position ball)))])))
(define b1 (make-ball .9 0))
(define b2 (make-ball .8 0))
(define g1 (make-game b1 b2 100))
(define (test n f)
  (cond [(= n 0) empty]
        [else (cons (f n) (test (sub1 n) f))]))
(define races (test 1000 (λ (x) (winner g1))))
(length (filter (λ (s) (symbol=? s 'one)) races))
(length (filter (λ (s) (symbol=? s 'two)) races))
(length (filter (λ (s) (symbol=? s 'tie)) races))
(define (ball->image ball)
  (overlay (text (number->string (ball-p ball)) 10 'black)
           (circle 10 'solid 'red)))
(define (draw game)
  (add-line (place-image (ball->image (game-ball1 game)) (ball-position (game-ball1 game))
               25
               (place-image (ball->image (game-ball2 game)) (+ 10 (ball-position (game-ball2 game)))
                            50
                            (empty-scene (+ (game-finishline game) 20) (+ (game-finishline game) 20))))
            (+ 10 (game-finishline game)) 0
            (+ 10 (game-finishline game)) (+ 20 (game-finishline game)) 'black))
(define (update g)
  (make-game (bounce (game-ball1 g)) (bounce (game-ball2 g)) (game-finishline g)))
(define (stop game)
  (or (= (game-finishline game) (ball-position (game-ball1 game))) 
      (= (game-finishline game) (ball-position (game-ball2 game)))))
(define (animation game)
          (big-bang game
                    (to-draw draw)
                    (stop-when stop)
                    (on-tick update)))
(animation (make-game (make-ball .5 0) (make-ball .5 0) 100))