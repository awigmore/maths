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
                     1629312893162931289316293128931629312893 
                     1239123010123912301012391230101239123010 
                     1283127348128312734812831273481283127348))
              347911021034791102103479110210347911021)
;; 1 milisecond
;; is this real life