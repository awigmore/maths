;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname powersetnew) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (ps s)
  (if (empty? s) (cons empty empty)
      (local ((define r (ps (rest s))))
        (append (map (λ (l) (cons (first s) l)) r) r))))
(time (first (ps '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22))))