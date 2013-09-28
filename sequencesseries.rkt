;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw 9|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; 21.1.3
;; An N is a natural number
;; An N is one of:
;; - 0
;; - (add1 N)
#;(define (N-temp N)
    (cond [(zero? N)...]
          [else...(N-temp (sub1 N))...]))
;; copy : N X  ->  (listof X)
;; to create a list that contains
;; obj n times
(define (copy n obj)
  (cond
    [(zero? n) empty]
    [else (cons obj 
                (copy (sub1 n) obj))]))
(check-expect (copy 3 'cat) (make-list 3 'cat))
(check-expect (copy 0 'cat) empty)
;; n-adder : N number  ->  number
;; to add n to x using
;; (+ 1 ...) only
(define (n-adder n x)
  (cond
    [(zero? n) x]
    [else (+ 1
             (n-adder (sub1 n) x))]))
(check-expect (n-adder 2 3) (+ 2 3))
(check-expect (n-adder 0 2) 2)
;; natural-f N Y X [X Y -> Y] -> Y
;; repeatedly does (f constant (f constant....) N times onto (f constant base)
(define (natural-f n base constant f)
  (cond [(zero? n) base]
        [else  (f constant (natural-f (sub1 n) base constant f))]))
(check-expect (natural-f 3 empty 'cat cons) (copy 3 'cat))
(check-expect (natural-f 0 empty 'cat cons) (copy 0 'cat))
(check-expect (natural-f 300 12 1 +) (n-adder 300 12))
(check-expect (natural-f 400 8 1 +) (n-adder 400 8))
(check-expect (natural-f 400 0 1 +) (n-adder 400 0))

;; 22.2.1
;; abstraction : [X -> Y] [Listof X] -> [Listof Y]
(define (abstraction op lox)
  (local [(define (newop x) (op x))]
    (map newop lox)))

;; 22.2.2?!?!?!
;; 23.1.1?!?!?!

;; for series check-expects, telescoping sums
;; and other mathematical principles are employed

;; make-even : N  ->  N[even]
;; to compute the i-th even number
(define (make-even i)
  (* 2 i))
(check-expect (make-even 50) 100)   
(check-expect (make-even 0) 0)            	
;; make-odd : N  ->  N[odd]
;; to compute the i-th odd number
(define (make-odd i)
  (+ (* 2 i) 1))
(check-expect (make-odd 50) 101)   
(check-expect (make-odd 0) 1)
;; series-even : N  ->  number
;; to sum up the first
;; n even numbers
(define (series-even n)
  (cond
    [(= n 0) (make-even n)]
    [else (+ (make-even n) 
             (series-even (- n 1)))]))
(check-expect (series-even 2000) (* 2 2000 (add1 2000) .5))
(check-expect (series-even 0) 0)     	
;; series-odd : N  ->  number
;; to sum up the first
;; n odd numbers
(define (series-odd n)
  (cond
    [(= n 0) (make-odd n)]
    [else (+ (make-odd n)
             (series-odd (- n 1)))]))
(check-expect (series-odd 2000) (+ (add1 2000) (* 2 2000 (add1 2000) .5)))
(check-expect (series-odd 0) 1)
;; series : N (N  ->  number)  ->  number
;; to sum up the first n numbers in the sequence a-term,
(define (series n a-term)
  (cond
    [(= n 0) (a-term n)]
    [else (+ (a-term n)
             (series (- n 1) a-term))]))
(check-expect (series 20 (λ (x) (expt 5 x)))(/(sub1(expt 5 (add1 20)))(sub1 5)))
(check-expect (series 0 (λ (x) (expt 5 x))) 1)

;; 23.2.1
;; a-fives : N -> N
;; Determines the number in the sequence
;; where a0 = 8, and a(n+1)=a(n)+5
;; but recursively.... 
;; why would anyone ever do this for such a simplistic sequence
(define (a-fives n)
  (cond [(zero? n) 8]
        [else (+ 5 (a-fives (sub1 n)))]))
(check-expect (a-fives 3) 23)
(check-expect (a-fives 0) 8)
(check-expect (build-list 4 a-fives) (list 8 13 18 23))


;; 23.2.2
;; a-fives-closed : N -> N
;; Determines the number in the sequence
;; where a0 = 8, and a(n+1)=a(n)+5
(define (a-fives-closed n)
  (+ 3 (* 5 (add1 n))))
(check-expect (a-fives-closed 3) 23)
(check-expect (build-list 4 a-fives-closed) (list 8 13 18 23)) 


;; 23.2.3
(check-expect (series 3 a-fives) (+ (* 8 (add1 3)) (* 5 (add1 3) 3 .5))) ;;62
(check-expect (series 7 a-fives) (+ (* 8 (add1 7)) (* 5 (add1 7) 7 .5))) ;;204
(check-expect (series 88 a-fives) 
              (+ (* 8 (add1 88)) (* 5 (add1 88) 88 .5))) ;;20292

;;an infinite arithmetic series cannot have a sum

;; 23.2.4
;; seq-a-fives : N -> [Listof numbers]
;; Builds the first n terms for a list for the sequence a-fives-closed
(define (seq-a-fives n)
  (build-list n a-fives-closed))
(check-expect (seq-a-fives 0) empty)
(check-expect (seq-a-fives 1) (list 8))
(check-expect (seq-a-fives 4) (list 8 13 18 23))

;; 23.2.5
;; arithmetic-sequence : number number -> [N -> number]
;; creates a function representing an arithmetic sequence
;; where a0=start+s and a(n+1)=a(n)+s
(define (arithmetic-sequence start s)
  (λ (n) (+ start (* s (add1 n)))))
(check-expect (build-list 20 (arithmetic-sequence 3 5)) (seq-a-fives 20))
(check-expect ((arithmetic-sequence 3 5) 0) 8)

;; 23.3.2
;; g-fives-closed : N -> N
;; determines the term in the sequence
;; where a0=3 and a(n+1)=a(n)*5
(define (g-fives-closed n)
  (* 3 (expt 5 n)))
(check-expect (build-list 5 g-fives-closed) (list 3 15 75 375 1875))
;; 23.3.3
;; seq-g-fives N -> [Listof N]
;; creates the sequence of the first n terms according to  g-fives-closed
(define (seq-g-fives n)
  (build-list n g-fives-closed))
(check-expect (seq-g-fives 0) empty)
(check-expect (seq-g-fives 1) (list 3))
(check-expect (seq-g-fives 5) (list 3 15 75 375 1875))
;; 23.3.4
;; geometric-sequence number number -> [N -> number]
;; creates a function representing a geometric sequence
;; where a0=start and a(n+1)=a(n)*s
(define (geometric-sequence start s)
  (lambda (n) (* start (expt s n))))
(check-expect ((geometric-sequence 3 5)12) (g-fives-closed 12))
;; 23.3.5
(check-expect (series 3 (geometric-sequence 1 .1)) 
              (/ (sub1 (expt .1 (add1 3))) (sub1 .1)));;1.111 
(check-expect (series 7 (geometric-sequence 1 .1)) 
              (/ (sub1 (expt .1 (add1 7))) (sub1 .1)));;1.1111111
(check-expect (series 88 (geometric-sequence 1 .1)) 
              (/ (sub1 (expt .1 (add1 88))) (sub1 .1)));;1 trailed by 88 1s

;; 23.3.7
;; ln-taylor : number -> [number -> number]
;; outputs the formula which determines the value of the nth term 
;; in the taylor series that estimates the natural log
(define (ln-taylor n)
  (lambda (x) (/ (expt (/(sub1 x) (add1 x)) (add1 (* 2 n))) (add1 (* 2 n)))))
(check-expect ((ln-taylor 12) 8) (/ (expt (/ 7 9) 25) 25))
;; ln : number -> number
;; estimates the natural log of a number
;; num > 0
(define (ln num)
  (* 2 (series 250 (lambda (x) ((ln-taylor x) num)))))
(check-range (ln (expt e 5)) (- 5 .001) (+ 5 .001))

;;23.3.8
;; ! : N -> N
;; Determines the factorial of a number
(define (! n)
  (apply * (build-list n add1)))
(check-expect (! 0) 1)
(check-expect (! 1) 1)
(check-expect (! 4) 24)
;; sin-taylor : number -> [number -> number]
;; outputs the formula which determine the value of the nth term 
;; in the taylor series that estimates sin
(define (sin-taylor n)
  (lambda (x) (/ (expt x (add1 (* 2 n))) (! (add1 (* 2 n))) (expt -1 n))))
(check-expect ((sin-taylor 13) 8) (/ (expt 8 27) (! 27) -1))
(check-expect ((sin-taylor 12) 8) (/ (expt 8 25) (! 25)))
;; my-sin number -> number
;; estimates the sin of a number
(define (my-sin num)
  (series 100 (lambda (x) ((sin-taylor x) num))))
(check-range (my-sin pi) (- 0 .0001) (+ 0 .0001))
(check-range (my-sin (/ pi 2)) (- 1 .0001) (+ 1 .0001))
;; 23.3.9
;; greg : N -> number
;; determines the term in Gregory's sequence
(define (greg n)
  (* (expt -1 n) (expt (add1 (* 2 n)) -1)))
(check-expect (greg 0) 1)
(check-expect (greg 1) -1/3)
(check-expect (greg 2) 1/5)
(check-range (* 4 (series 100 greg)) (- pi .01) (+ pi .01))


;;23.4.2.
;;apologies for the indentation - 80 character limit
(define R 5000) ;;number of rectangles for rectangle integration approximation
;; integrate : [number -> number] number number
;; estimates the area under a function with rectangle approximation
(define (integrate f a b)
  (local [(define width  (/(abs (- b a)) R))
          (define (integrate-accum f a b sum)
            (cond [(>= a b) sum]
                  [else (integrate-accum f (+ a width) b 
                                         (+ sum 
                                            (* width(f (+ a (/ width 2))))))]))]
    (cond [(= a b) 0]
          [(< a b) (integrate-accum f a b 0)]
          [else (* -1 (integrate-accum f b a 0))])))
(check-range (integrate sin 0 pi) (- 2 .0001) (+ 2 .0001))
(check-range (integrate sin pi 0) (- -2 .0001) (+ -2 .0001))
(check-expect (integrate sin pi pi) 0)