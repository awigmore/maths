;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname josephus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; k > 1
(define (j n k)
  (local ((define startlist (build-list n (λ (x) x)))
          (define (turn remaining k)
            (cond [(= k 1) (rest remaining)]
                  [else (turn (append (rest remaining) (list (first remaining))) (sub1 k))]))
          (define (josephus alist)
            (cond [(empty? (rest alist)) (first alist)]
                  [else (josephus (turn alist k))])))
    (josephus startlist)))

(define (g n k)
  (cond [(= n 1) 0]
        [else (modulo (+ (g (sub1 n) k) k) n)]))

(define (testing count)
  (cond [(= count 0) 'yay]
        [else (local ((define n (add1 (random count)))
                      (define k (add1 (random count))))
                (if (= (j n k) (g n k)) (testing (sub1 count)) 'boo))]))
(testing 20)