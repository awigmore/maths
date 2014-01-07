#lang racket
;; Returns all prime numbers up to and including n
;; n >= 2
(define (sieve n)
  (local ((define m (add1 n))
          ;; A bit-vector going from [0 n]
          ;; 0 and 1 included for convinicne's sake, ignored at the end
          (define tf (make-vector m true))
          ;; Convert all multiples of the prime to false, starting at the prime
          ;; squared (this works because all smaller multiples of the prime 
          ;; will be  crossed off by primes smaller than it)
          (define (modify-vector tf prime)
            (local ((define (acc composite)
                      (if (>= composite m) tf 
                          (begin (vector-set! tf composite false) 
                                 (acc (+ composite prime))))))
              (acc (* prime prime))))
          ;; Returns the next biggest prime, or a symbol if there isn't one
          (define (next-prime prime)
            (local ((define (acc i)
                      (if (= i m) 'BOOM 
                          (if (vector-ref tf i) i (acc (add1 i))))))
              (acc (add1 prime))))
          ;; Modify tf until there are no more primes to modify
          (define (loop prime)
            (if (or (symbol? prime) (>= (sqr prime) m)) tf 
                (begin (modify-vector tf prime) (loop (next-prime prime)))))
          ;; Use tf to make the final list
          (define (f i)
            (if (= i m) empty 
                (if (vector-ref tf i) (cons i (f (add1 i))) (f (add1 i))))))
    (begin (loop 2) (f 2))))
(equal? (sieve 20) '(2 3 5 7 11 13 17 19))
(time (length (sieve 15485863))) ;; result is 1000000
;; cpu time: 8156 real time: 8189 gc time: 2098