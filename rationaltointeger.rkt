;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rationaltointeger) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(define (factorization n)
  (local ((define (factorization up down others)
            (cond [(= down 1) others]
                  [(zero? (modulo down up))
                   (factorization up (/ down up) (cons up others))]
                  [else (factorization (add1 up) down others)])))
    (if (= n 1) (list 1) (factorization 2 n empty))))
(define (f->pf f)
  (local ((define (count f factor)
            (cond 
              [(or (empty? f) (not (= (first f) factor))) 0]
              [else (add1 (count (rest f) factor))])) 
          (define (remove-head f factor)
            (cond [(or (empty? f) (not (= (first f) factor))) f]
                  [else (remove-head (rest f) factor)])))
    (cond [(empty? f) empty]
          [else (cons (list (first f) (count f (first f)))
                      (f->pf (remove-head f (first f))))])))
(define (rational->integer r)
  (cond [(= r 0) 0]
        [(> 0 r) (- (rational->integer (- r)))]
        [else (* 
               (apply * (map (λ (double)
                               (expt (first double) (second double)))
                             (map 
                              (λ (double) (list (first double) (* 2 (second double))))
                              (f->pf (factorization (numerator r))))))
               (apply * (map (λ (double)
                               (expt (first double) (second double)))
                             (map 
                              (λ (double) (list (first double) (sub1 (* 2 (second double)))))
                              (f->pf (factorization (denominator r)))))))]))
(define (integer->rational i)
  (cond [(= i 0) 0]
        [(> 0 i) (- (integer->rational (- i)))]
        [else (apply * (map 
                        (λ (double)
                          (expt (first double) (second double)))
                        (map (λ (double) (if (even? (second double))
                                             (list (first double)
                                                   (/ (second double) 2))
                                             (list (first double)
                                                   (- (/ (add1 (second double)) 2)))))
                             (f->pf (factorization i)))))]))
(check-expect (rational->integer 3.14) 6162250)
(check-expect (integer->rational 6162250) 3.14)
(check-expect (rational->integer 1) 1)
(check-expect (integer->rational 1) 1)
(check-expect (rational->integer 0) 0)
(check-expect (integer->rational 0) 0)
(check-expect (rational->integer -3.14) -6162250)
(check-expect (integer->rational -6162250) -3.14)
(define x (random 9999))
(define y (integer->rational x))
(check-expect (rational->integer y) x)
(define (plot xs ys width height)
  (local ((define minx (apply min xs))
          (define maxx (apply max xs))
          (define miny (apply min ys))
          (define maxy (apply max ys))
          (define xrange (- maxx minx))
          (define yrange (- maxy miny))
          (define xpercentage (λ (x) (/ (- x minx) xrange)))
          (define ypercentage (λ (y) (/ (- y miny) yrange)))
          (define (plot xs ys pic)
            (if (empty? xs) pic
            (plot (rest xs)
                  (rest ys)
                  (place-image (circle 2 'solid 'red)
                         (* width (xpercentage (first xs)))
                         (- height (* height (ypercentage (first ys))))
                         pic)))))
    (plot xs ys (empty-scene width height))))
;; integer->rational is bounded above by |i|^1/2
;; bounded below by |i|^-1 
;; (plot (build-list 500 add1) (build-list 500 integer->rational) 500 500)
