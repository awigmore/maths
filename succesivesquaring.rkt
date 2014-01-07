;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname succesivesquaring) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; All tests regarding 7^327 mod 853 
;; come from the example in  
;; A Friendly Introduction To Number Theory e4
;; page 112

;; A BinaryExpansion is a Non-Empty [List {0, 1}]
;; And represents a decimal's binary expansion
;; dec->bin : Natural -> BinaryExpansion
(define (dec->bin d)
  (local ((define (dec->bin dec power)
            (cond [(= power -1) empty]
                  [(> (expt 2 power) dec) (cons 0 (dec->bin dec (sub1 power)))]
                  [else (cons 1 (dec->bin (- dec (expt 2 power)) (sub1 power)))])))
    (if (= d 0) 
        (list 0)
        (dec->bin d (floor (inexact->exact (/ (log d) (log 2))))))))
(check-expect (dec->bin 0) '(0))
(check-expect (dec->bin 1) '(1))
(check-expect (dec->bin 2) '(1 0))
(check-expect (dec->bin 3) '(1 1))
(check-expect (dec->bin 4) '(1 0 0))
(check-expect (dec->bin 5) '(1 0 1))
(check-expect (dec->bin 6) '(1 1 0))
(check-expect (dec->bin 7) '(1 1 1))

;; table-of-powers : Natural x PositiveInteger x PositiveInteger -> [List Natural]
;; Succesively squares a mod m, up until the largest power of 2 to in k's
;; binary expansion
(define (table-of-mod-a-powers a k m) 
  (local ((define maxpower (add1 (floor (inexact->exact (/ (log k) (log 2))))))
          (define (table-maker currentpower table)
            (cond [(> currentpower maxpower) table]
                  [else (table-maker (add1 currentpower)
                                     (cons (modulo (sqr (first table)) m) table))])))
    (table-maker 2 (cons (modulo a m) empty))))
(check-expect (table-of-mod-a-powers 7 327 853) (list 298 628 123 675 349 227 695 49 7))
(check-expect (table-of-mod-a-powers 1 327 853) (list 1 1 1 1 1 1 1 1 1))
(check-expect (table-of-mod-a-powers 0 327 853) (list 0 0 0 0 0 0 0 0 0))
(check-expect (table-of-mod-a-powers 7 1 853) (list 7))
(check-expect (table-of-mod-a-powers 7 2 853) (list 49 7))


;; laststep : [List Natural] x BinaryExpansion x Natural -> Natural
;; Given the table of succesive squares of a mod m, k's binary expansion, and
;; m, effeciently computes a^k mod m
(define (laststep table powers m)
  (foldr (λ (number modsofar) (modulo (* number modsofar) m)) 1 
         (map (λ (table powers) 
                (expt table powers)) ; x^0=1, is ignored in product, x^1=x, considered as is
              table powers)))
(check-expect (laststep (table-of-mod-a-powers 7 327 853) (dec->bin 327) 853) 286)

;; Succesive-Squaring : Natural x PositiveInteger x PositiveInteger -> Natural
;; computes a^k mod m at lightning speed
(define (succesive-squaring a k m)
  (local ((define step1 (dec->bin k))
          (define step2 (table-of-mod-a-powers a k m)))
    (laststep step2 step1 m)))
(check-expect (succesive-squaring 7 327 853) 286)
(check-expect (succesive-squaring 7 8 853) (modulo (expt 7 8) 853))
(check-expect (succesive-squaring 7 1 1) (modulo (expt 7 1) 1))
(check-expect (succesive-squaring 7 1 2) (modulo (expt 7 1) 2))
;; Just how fast is it?
(check-expect (time (succesive-squaring 
                     6847944682037444681162770672798288913850
                     6847944682037444681162770672798288913848 
                     6847944682037444681162770672798288913849)) ;; prime
              1)
;; 1 milisecond

;; extended : Natural x Natural -> (list Natural Natural)
;; The exnteded euclidian algorithm
;; finds x, y s.t. a*x+b*y=gcd(a,b)
(define (extended a b)
  (if (= b 0) (list 1 0)
      (local ((define q (quotient a b))
              (define r (remainder a b))
              (define x (extended b r)))
        (list (second x) (- (first x) (* q (second x)))))))
(check-expect (extended 15 4) (list -1 4))
(check-expect (extended 15 0) (list 1 0))


;; gcd : Natural x Natural -> Natural
(define (gcf a b)
  (if (= b 0) a (gcf b (modulo a b))))
(check-expect (gcf 15 7) 1)
(check-expect (gcf 5 0) 5)
(check-expect (gcf 10 15) 5)

;; korselt : Natural -> Boolean
;; Is this number a Carmichael number?
(define (korselt n)
  (local ((define (factor n)
            (local ((define (f factors divisor n)
                      (cond [(= n 1) factors]
                            [(integer? (/ n divisor)) 
                             (f (cons divisor factors) divisor (/ n divisor))]
                            [else (f factors (add1 divisor) n)])))
              (f empty 2 n)))
          (define factors (factor n))
          (define (square-free? factors)
            (or (empty? (rest factors))
                (and (not (= (first factors)
                             (first (rest factors))))
                     (square-free? (rest factors))))))
    (and 
     (odd? n)
     (> (length factors) 1) 
     (square-free? factors)
     (andmap (λ (p) (integer? (/ (sub1 n) (sub1 p)))) factors))))
(check-expect (korselt 3) false)
(check-expect (korselt 62745) true)

;; ranin-miller : Odds -> Boolean
;; Is this number composite?
(define (rabin-miller n)
  (local ((define (2^k x)
            (cond [(odd? x) 0]
                  [else (add1 (2^k (/ x 2)))]))
          (define k (2^k (sub1 n)))
          (define q (/ (sub1 n) (expt 2 k)))
          (define amax 100)
          (define (a-generators a as)
            (cond [(= (length as) amax) (reverse as)]
                  [(not (integer? (/ a n))) (a-generators (add1 a) (cons a as))]
                  [else (a-generators (add1 a) as)]))
          (define as (a-generators 2 empty)))
    (ormap (λ (a)
     (and (not (= 1 (succesive-squaring a q n)))
         (andmap (λ (i) (not (= (- n 1) (succesive-squaring a (* (expt 2 i) q) n))))
                 (build-list k identity)))) as)))
(check-expect (rabin-miller 25) true)
(check-expect (rabin-miller (* 3 5 7 11)) true)
(check-expect (rabin-miller 234723489) true)
(check-expect (rabin-miller 101) false)