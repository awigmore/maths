;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |majority voting|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)
;; A World is a list of Voters
;; A Voter is a (make-voter Nat Nat Boolean)
;; Where the two numbers do not equal each other,
;; nor the voter's place in the list,
;; and neither exceeds the length of the world - 1
(define-struct voter (n1 n2 s))
;; make-world : Pos -> World
;; Makes a completely random world with n many voters
;; N >= 3
(define (make-world n)
  (local ((define max  n)
          (define (make-world n)
            (cond [(zero? n) empty]
                  [else (cons (make-a-voter (- max n) max) (make-world (sub1 n)))])))
    (make-world n)))

;; make-a-voter : Nat x Nat -> Voter
;; Makes a voter where neither number
;; is the first number
;; nor exceeds the second number
;; with random initial state
(define (make-a-voter place max)
  (local ((define random1 (random  max))
          (define random2 (random max)))
    (cond [(or (= random1 random2) (= random1 place) (= random2 place)) (make-a-voter place max)]
          [else (make-voter random1 random2 (zero? (random 2)))])))

;; Majority : Boolean x Boolean x Boolean -> Boolean
;; Returns the majority of the booleans
(define (majority b1 b2 b3)
  (or (and b1 b2 b3)
      (and b1 b2)
      (and b1 b3)
      (and b2 b3)))
(check-expect (majority true true true) true)
(check-expect (majority true true false) true)
(check-expect (majority true false true) true)
(check-expect (majority false true true) true)
(check-expect (majority false false true) false)
(check-expect (majority false true false) false)
(check-expect (majority true false false) false)
(check-expect (majority false false false) false)

;; Next-World : World -> World
;; Updates the states of the voters in the world
(define (next-world world)
  (local ((define old-world world)
          (define (next-world world)
            (cond [(empty? world) empty]
                  [else (cons (update (first world) old-world) (next-world (rest world)))])))
    (next-world world)))
(check-expect (next-world (list (make-voter 4 3 true) (make-voter 4 0 false) (make-voter 3 0 true) (make-voter 1 2 false) (make-voter 0 1 false)))
              (list (make-voter 4 3 false) (make-voter 4 0 false) (make-voter 3 0 true) (make-voter 1 2 false) (make-voter 0 1 false)))
;; Update : Voter World -> Voter
;; Updates the voter into its next state
(define (update voter world)
  (make-voter (voter-n1 voter) (voter-n2 voter) (majority (voter-s (list-ref world (voter-n1 voter)))
                                                          (voter-s (list-ref world (voter-n2 voter)))
                                                          (voter-s voter))))
(check-expect (update (make-voter 4 3 true) (list (make-voter 4 3 true) (make-voter 4 0 false) (make-voter 3 0 true) (make-voter 1 2 false) (make-voter 0 1 false)))
              (make-voter 4 3 false))

;;  : List X -> Nat
;; Determines the location of X in a list
;; Used to find the period of a world
(define (reverse-ref alist x)
  (local ((define (reverse-ref alist n)
            (cond [(empty? alist) (error 'not-here)]
                  [(equal? (first alist) x) n]
                  [else (reverse-ref (rest alist) (add1 n))])))
    (reverse-ref alist 0)))
(check-expect (build-list 10 (lambda (y) (reverse-ref (build-list 10 (lambda (x) x)) y)))
              (build-list 10 (lambda (x) x)))
;; Alternate : Nat -> World
;; Creates a world with period 2
;; Nat must be greater than 3
(define (alternate n)
  (local ((define amount (cond [(even? n) n]
                               [else (- n 1)]))
          (define (alternate n)
            (cond [(zero? n) empty]
                  [else (cons (make-voter (modulo (sub1 (- amount n)) amount) (modulo (add1 (- amount n))  amount) (even? n))
                              (alternate (sub1 n)))])))
    (cond [(even? n) (alternate n)]
          [else (append (alternate (- n 1)) (list (make-voter 0 (- n 2) (even? (random 2)))))])))

;; This section deals with making worlds of size n with period of size n
(define (make-even-period n)
  (local ((define firstfour (list (make-voter (sub1 n) 1 true)
                                  (make-voter (- n 2) 0 false)
                                  (make-voter (sub1 n) 0 true)
                                  (make-voter (- n 2) 1 false)))
          (define (others k)
            (cond [(= k (- n 4)) empty]
                  [else (cons (make-voter (if (even? k) 0 1) (+ 2 k) (even? k)) (others (add1 k)))])))
    (append firstfour (others 0))))
(define (make-odd-period n)
  (local ((define (make-odd-period k)
            (cond [(= k n) empty]
                  [else (cons (make-voter (modulo (add1 k) n) (modulo (+ (- n 2) k) n) (even? k)) (make-odd-period (add1 k)))])))
    (make-odd-period 0)))
(define (make-period n)
  (if (even? n) (make-even-period n) (make-odd-period n)))
;; Period : World -> Nat
;; Given a world, determine its period
(define (period world)
  (local ((define (determine-period oldworlds currentworld)
            (cond [(member? currentworld oldworlds) (add1 (reverse-ref oldworlds currentworld))]
                  [else (determine-period (cons currentworld oldworlds) (next-world currentworld))])))
    (determine-period empty world)))

(check-expect (period (alternate (+ 4 (random 50)))) 2)
(check-expect (period (make-world 3)) 1)

;; bigger-period : Nat -> World
;; Force a world of size n that has a period > 1
;; n > 3
(define (bigger-period n)
  (local ((define world (make-world n)))
    (cond [(> (period world) 1) world]
          [else (bigger-period n)])))

;; Turns-Before-Period : World -> Nat
;; After how many updates
;; Has the period begun?
(define (turns-before-period world)
  (local ((define (determine-turns oldworlds currentworld)
            (cond [(member? currentworld oldworlds)   (reverse-ref (reverse oldworlds) currentworld)]
                  [else (determine-turns (cons currentworld oldworlds) (next-world currentworld))])))
    (determine-turns empty world)))
(check-expect (member? (turns-before-period (make-world 3)) (list 0 1)) true)
;; Combine-Worlds : List of Worlds -> World
;; Combines worlds together
(define (combine-worlds worlds)
  (local ((define lengths (map length worlds))
          (define (sumupto numbers max k)
            (cond [(= max k) 0]
                  [else (+ (first numbers) (sumupto (rest numbers) max (add1 k)))]))
          (define (new-worlds worlds k)
            (cond [(empty? worlds) empty]
                  [else (append (map (lambda (v) (make-voter (+ (sumupto lengths k 0) (voter-n1 v))
                                                             (+ (sumupto lengths k 0) (voter-n2 v))
                                                             (voter-s v))) (first worlds)) (new-worlds (rest worlds) (add1 k)))])))
    (new-worlds worlds 0)))

;; Interactive/visual/fun stuff

;; Visual constants 
(define DRAW-HEIGHT 500)
(define DRAW-WIDTH 500)
(define CIRC-RADIUS 10)

;; A pic has a voter and an x-y coordinate
(define-struct pic (x y voter))
;; World->CirclePics : World -> Pics
;; Turns the world into pics
;; Where they start out in a circle
(define (world->circlepics world)
  (local ((define amount (length world))
          (define (world->pics world)
            (cond [(empty? world) empty]
                  [else (cons (make-pic (+ (* DRAW-WIDTH (cos (* (length world) (/ (* 2 pi) amount))) 7/16) (/ DRAW-WIDTH 2))
                                        (+ (* DRAW-HEIGHT (sin (* (length world) (/ (* 2 pi) amount))) 7/16) (/ DRAW-HEIGHT 2))
                                        (first world))
                              (world->pics (rest world)))]))) 
    (world->pics world)))

;; World->RandomPics : World -> Pics
;; Turns the world into pics
;; Where they start in a random position
(define (world->randompics world)
  (cond [(empty? world) empty]
        [else (cons (make-pic (random DRAW-WIDTH)
                              (random DRAW-HEIGHT)
                              (first world))
                    (world->randompics (rest world)))]))

;; Pic->Image : Pic -> Image
;; Turns a pic into an image
(define (pic->image pic)
  (circle CIRC-RADIUS 'solid (if (voter-s (pic-voter pic)) 'blue 'red)))

;; Draw-World : Pics -> Image
;; Draws the pictures
(define (draw-pics pics)
  (foldr (lambda (pic img) (place-image (pic->image pic) (pic-x pic)  (pic-y pic) img)) (empty-scene DRAW-WIDTH DRAW-HEIGHT)  pics))

;; Draw-Connector : Pic Pic Pic Image-> Image
;; Pic2 and Pic3 are neighbors of Pic1
;; Draws connectors from one voter to its neighbors
(define (draw-connector pic1 pic2 pic3 image)
  (add-line
   (add-line
    (add-line
     (add-line image (pic-x pic1) (pic-y pic1)
               (/ (+ (pic-x pic1) (pic-x pic2)) 2)
               (/ (+ (pic-y pic1) (pic-y pic2)) 2)
               (make-pen 'green 1 'dot 'round 'round))
     (/ (+ (pic-x pic1) (pic-x pic2)) 2)
     (/ (+ (pic-y pic1) (pic-y pic2)) 2)
     (pic-x pic2)
     (pic-y pic2)
     (make-pen 'purple 1 'dot 'round 'round))
    (pic-x pic1) (pic-y pic1)
    (/ (+ (pic-x pic1) (pic-x pic3)) 2)
    (/ (+ (pic-y pic1) (pic-y pic3)) 2)
    (make-pen 'green 1 'dot 'round 'round))
   (/ (+ (pic-x pic1) (pic-x pic3)) 2)
   (/ (+ (pic-y pic1) (pic-y pic3)) 2)
   (pic-x pic3) (pic-y pic3)
   (make-pen 'purple 1 'dot 'round 'round)))
;; Draw-Connectors : Pics -> Image
;; Draws the world and the connectors
(define (draw-connectors pics)
  (foldr (lambda (pic image) (draw-connector pic (list-ref pics (voter-n1 (pic-voter pic))) (list-ref pics (voter-n2 (pic-voter pic))) image)) (draw-pics pics) pics))

;; Updates the world of pictures
;; Does the same as updating voters
;; But keeps the voters 'wrapped'
;; inside pictures
;; next-pics : Pics -> Pics
(define (next-pics w)
  (local ((define oldworld (map (lambda (p) (pic-voter p)) w))
          (define (next-pics w)
            (cond [(empty? w) empty]
                  [else (cons (make-pic (pic-x (first w))
                                        (pic-y (first w))
                                        (update (pic-voter (first w)) oldworld)) (next-pics (rest w)))])))
    (next-pics w)))

;; For making sure dragging doesn't overlap pictures
(define (in-circle? x y h k r)
  (>= r (sqrt (+ (sqr (- x h)) (sqr (- y k))))))
(define (pics-touching? pic1 pic2)
  (<= (sqrt (+ (sqr (- (pic-x pic1) (pic-x pic2))) (sqr (- (pic-y pic1) (pic-y pic2))))) (* 2 CIRC-RADIUS)))
(define (pic-touching-list? pic piclist)
  (ormap (lambda (p) (pics-touching? pic p)) piclist))
(define (none-touching? lop)
  (cond [(empty? lop) true]
        [(empty? (rest lop)) true]
        [(pic-touching-list? (first lop) (rest lop)) false]
        [else (none-touching? (rest lop))]))


;; mouse : World x y me -> World
;; Controls dragging and clicking to add a 
;; new random voter
(define (mouse w x y me)
  (cond [(and (string=? me "drag") 
              (ormap (lambda (p) (in-circle? x y (pic-x p) (pic-y p) CIRC-RADIUS)) (first w)))
         (local ((define none-touch? (none-touching? (first w))))
           (if none-touch?
               (if (none-touching? (map (lambda (p) (if (in-circle? x y (pic-x p) (pic-y p) CIRC-RADIUS) 
                                                        (make-pic x y (pic-voter p))
                                                        p)) (first w)))
                   (list (map (lambda (p) (if (in-circle? x y (pic-x p) (pic-y p) CIRC-RADIUS) 
                                              (make-pic x y (pic-voter p))
                                              p)) (first w)) (second w))
                   w)
               (list (map (lambda (p) (if (in-circle? x y (pic-x p) (pic-y p) CIRC-RADIUS) 
                                          (make-pic x y (pic-voter p))
                                          p)) (first w)) (second w))))]
        [(and (string=? me "button-down") (not (ormap (lambda (p) (in-circle? x y (pic-x p) (pic-y p) CIRC-RADIUS)) (first w))))
         (list (cons (make-pic x y (make-a-voter 0 (length (first w)))) (map (lambda (p) (make-pic (pic-x p) (pic-y p) (make-voter (add1 (voter-n1 (pic-voter p))) (add1 (voter-n2 (pic-voter p))) (voter-s (pic-voter p))))) (first w))) (second w))]
        [else w]))
;; Constant to shift the world
;; up/down/left/right
(define FORCE-MOVE 15)
;; key : World Key -> World
;; Handles updates (space or enter), 
;; shifting (arrow keys),
;; moving them (p)
;; stopping them (s)
;; random placement (r)
;; initial placement (i)
;; move to initial placement (o)
;; rotate in a circle (c)
(define (key w ke) 
  (cond [(or (string=? ke " ")(string=? ke "\r")) (list (next-pics (first w)
                                                                   ) (second w))]
        [(string=? ke "up") (list (map (lambda (pic) (make-pic (pic-x pic)
                                                               (- (pic-y pic) FORCE-MOVE)
                                                               (pic-voter pic))) (first w)) (second w))]
        [(string=? ke "down") (list (map (lambda (pic) (make-pic (pic-x pic)
                                                                 (+ (pic-y pic) FORCE-MOVE)
                                                                 (pic-voter pic))) (first w)) (second w))]
        [(string=? ke "right") (list (map (lambda (pic) (make-pic (+ (pic-x pic) FORCE-MOVE)
                                                                  (pic-y pic) 
                                                                  (pic-voter pic))) (first w)) (second w))]
        [(string=? ke "left") (list (map (lambda (pic) (make-pic (- (pic-x pic) FORCE-MOVE)
                                                                 (pic-y pic) 
                                                                 (pic-voter pic))) (first w)) (second w))]
        [(string=? ke "p") (list (first w) (if (symbol=? 'go (second w)) 'stop 'go))]
        [(string=? ke "o") (list (first w) (if (symbol=? 'reverse (second w)) 'stop 'reverse))]
        [(string=? ke "c") (list (first w) (if (symbol=? 'rotate (second w)) 'stop 'rotate))]
        [(string=? ke "i") (list (world->circlepics (map pic-voter (first w))) 'stop)]
        [(string=? ke "r") (list (world->randompics (map pic-voter (first w))) 'stop)]
        [(string=? ke "s") (list (first w) 'stop)]
        [else w])) 

;; constant for how fast they move
(define MOVE-SPEED 2)
;; Move-To : Real Real Real Real -> (list Real Real)
;; Determines how far something should move in x/y
;; direction when it's moving
(define (move-to x1 y1 x2 y2)
  (cond [(and (= x1 x2) (= y1 y2)) (list 0 0)]
        [else (local ((define angle (atan (- y2 y1) (- x2 x1))))
                (list (* MOVE-SPEED (cos angle)) (* MOVE-SPEED (sin angle))))]))
;; How fast the world should rotate
(define ROTATE-SPEED (/ MOVE-SPEED (* 10 pi)))
;; rotate-pic : Pic -> (list Real Real)
;; Determines the new location of a pic
;; When rotating about the center
(define (rotate-pic pic)
  (local ((define x  (pic-x pic) )
          (define y (pic-y pic) ))
    (cond [(and (= x (/ DRAW-WIDTH 2)) (= y (/ DRAW-HEIGHT 2))) (list x y)]
          [else (local ((define angle (atan  (- y (/ DRAW-HEIGHT 2)) (- x (/ DRAW-WIDTH 2))))
                        (define newangle (+ angle ROTATE-SPEED))
                        (define radius (sqrt (+ (sqr (- x (/ DRAW-WIDTH 2))) (sqr (- y (/ DRAW-HEIGHT 2)))))))
                  (list (+ (/ DRAW-WIDTH 2) (* radius (cos newangle))) (+ (/ DRAW-WIDTH 2) (* radius (sin newangle)))))])))
;; move-pics : Pics -> Pics
;; Moves all the pics towards the midpoint
;; Of their neighbors
(define (move-pics-mean pics)
  (local ((define worldpics pics))
    (foldr (lambda (pic listofpics) (cons (make-pic
                                           (+ (pic-x pic)
                                              (first (move-to (pic-x pic)
                                                              (pic-y pic)
                                                              (/ (+ 
                                                                  (pic-x (list-ref pics (voter-n1 (pic-voter pic))))
                                                                  (pic-x (list-ref pics (voter-n2 (pic-voter pic))))) 2)
                                                              
                                                              (/ (+ 
                                                                  (pic-y (list-ref pics (voter-n1 (pic-voter pic))))
                                                                  (pic-y (list-ref pics (voter-n2 (pic-voter pic))))) 3))))
                                           (+ (pic-y pic)
                                              (second (move-to (pic-x pic)
                                                               (pic-y pic)
                                                               (/ (+ 
                                                                   (pic-x (list-ref pics (voter-n1 (pic-voter pic))))
                                                                   (pic-x (list-ref pics (voter-n2 (pic-voter pic))))) 2)
                                                               
                                                               (/ (+ 
                                                                   (pic-y (list-ref pics (voter-n1 (pic-voter pic))))
                                                                   (pic-y (list-ref pics (voter-n2 (pic-voter pic))))) 2))))
                                           (pic-voter pic)) listofpics)) empty
                                                                         worldpics)))
;; Rotates the pictures
(define (rotate-pics pics)
  (foldr (lambda (pic listofpics) (cons (make-pic (first (rotate-pic pic))
                                                  (second (rotate-pic pic))
                                                  (pic-voter pic)) listofpics)) empty pics))
;; Moves pics to their initial placement
(define (reverse-em pics)
  (local ((define initialpics (world->circlepics (map pic-voter pics)) ))
    (foldr (lambda (n restlist) (cons
                                 (make-pic 
                                  (+ (pic-x (list-ref pics n))
                                     (first (move-to 
                                             (pic-x (list-ref pics n))
                                             (pic-y (list-ref pics n))
                                             (pic-x (list-ref initialpics n))
                                             (pic-y (list-ref initialpics n)))))
                                  (+ (pic-y (list-ref pics n))
                                     (second (move-to 
                                              (pic-x (list-ref pics n))
                                              (pic-y (list-ref pics n))
                                              (pic-x (list-ref initialpics n))
                                              (pic-y (list-ref initialpics n)))))
                                  (pic-voter (list-ref pics n)))
                                 restlist)) empty (build-list (length pics) (lambda (x) x)))))
;; Handles movement
(define (movement alist)
  (cond [(symbol=? 'go (second alist)) (list (move-pics-mean (first alist)) 'go)]
        [(symbol=? 'rotate (second alist)) (list (rotate-pics (first alist)) 'rotate)]
        [(symbol=? 'reverse (second alist)) (list (reverse-em (first alist)) 'reverse)]
        [else alist]))
;; Visual made from the world
(define (draw alist)
  (draw-connectors (first alist)))


;;Initial pics options:
;; (alternate n) or (make-world n) or (bigger-period n) or (make-period n)
;; (world->circlepics) or (world->randompics)
;; to-draw options:
;; draw-pics or draw-connectors

(big-bang (list (world->circlepics (bigger-period 8)) 'stop)
          (on-tick movement)
          (on-mouse mouse)
          (on-key key)
          (to-draw draw)
          (stop-when (lambda (w) (equal? (first w) (next-pics (first w))))))