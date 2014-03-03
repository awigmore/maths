;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname same-shape) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t quasiquote repeating-decimal #f #t none #f ())))
;; Data definition for a graph, G1, and some same-graph? tests:
;; http://www.ccs.neu.edu/course/csu211/Assignments/10h.html
;; ascii art: http://patorjk.com/software/taag/#p=display&f=Bell&t=ascii
;; Written in Intermediate Student with lambda
;; By Matthew Singer

;; Problem: Given two graphs, determine if they have the same shape.
;; Two graphs have the same shape if, after removing all node names,
;; one can rearrange the nodes (without altering which edges connect 
;; to which node) and get back the same picture.
;; One could try every permutation swapping the node names from the first
;; with the second, but that's guaranteed factorial time.
;; The algorithm used here only alters corresponding nodes with the same
;; connection-hash. Currently, it takes the amount of edges coming out from
;; all of its neighbors and hashes that.

;; Document structure:
;; Data Definitions
;; List Functions (helpers)
;; Bin and Choice (helpers - explained in data definitions)
;; Hash (helpers)
;; Examples (of graphs-related data definitions)
;; Graph Functions (helpers)
;; same-shape? (the final result and many tests)

;; To easily find one section, View -> Show Program Contour (or command+u/ctrl+u)
;; or command+f/ctrl+f

; .___           .             .                          
; /   `    ___  _/_     ___ 
; |    |  /   `  |     /   `
; |    | |    |  |    |    |
; /---/  `.__/|  \__/ `.__/|
; .___          ,__              .                          
; /   `    ___  /  ` ` , __   ` _/_   `   __.  , __     ____
; |    | .'   ` |__  | |'  `. |  |    | .'   \ |'  `.  (    
; |    | |----' |    | |    | |  |    | |    | |    |  `--. 
; /---/  `.___, |    / /    | /  \__/ /  `._.' /    | \___.'

;; Data Definitions 

;; An [Equality X] is a [X X -> Boolean]
;; and is an equivalence relation

(define-struct graph (nodes neighbors node=?))
;; A [Graph X] is a (make-graph [List X] [X -> [List X]] [Equality X])
;; Invariant: For all nodes n a graph g,
;; (member n (remove n (graph-nodes g))) = false
;; (i.e. all node names are distinct)

;; A [Connection-Graph X] is a [Graph (list X Natural)]
;; Where the number in the pair of the node is
;; based off of the connections the node has
;; (currently, a hash of the lengths of its neighbors
;; connections)

;; A [Sorted-Connection-Graph X] is a [Connection-Graph x]
;; where the list of nodes is sorted in descending
;; order based on the node's number
;; Note: A sorted size graph represents the same
;; graph as its unsorted version, but the sorting
;; of nodes can be (and is) used for computation

;; A Bin is a [List Positive]
;; A bin represents how many elements in a list
;; belong to a certain "bin"
;; For example, the list '(a a a b d e e)
;; lends itself to the Bin '(3 1 1 2)

;; A Choice is a [List Natural]
;; A choice represents indices of a nested for loop
;; Using java syntax:
;; for (int i = 0; i < 2; i++) 
;;   for (int j = 0; j < 3; j++)
;;     for (int k = 0; k < 2; k++)
;; '(1 2 0) would represent the moment in the for loop
;; where i = 1, j = 2, and k = 0

;; A [Pair X Y] is a (list X Y)

;; A [Maybe X] is one of:
;; - X
;; - false

; .               .                      
; /     `   ____ _/_       
; |     |  (      |         
; |     |  `--.   |       
; /---/ / \___.'  \__/     
; .____                      .                          
; /     ,   . , __     ___  _/_   `   __.  , __     ____
; |__.  |   | |'  `. .'   `  |    | .'   \ |'  `.  (    
; |     |   | |    | |       |    | |    | |    |  `--. 
; /     `._/| /    |  `._.'  \__/ /  `._.' /    | \___.'

;; List Functions

;; list=? : [List X] x [List X] x [Equality X] -> Boolean
;; Are the two lists the same? (order does not matter)
(define (list=? l1 l2 equals?)
  (or (and (empty? l1) (empty? l2))
      (and (cons? l1) (cons? l2)
           (local ((define x (rem (first l1) l2 equals?)))
             (and (not (false? x)) (list=? (rest l1) x equals?))))))
(check-expect (list=? empty empty =) true)
(check-expect (list=? empty '(5) =) false)
(check-expect (list=? '(5) empty =) false)
(check-expect (list=? '(5) '(5) =) true)
(check-expect (list=? '(5 6 7 8 9 10) '(10 9 8 7 6 5) =) true)
(check-expect (list=? '(5 6 6 7 8 9 7 10) '(10 7 6 9 8 7 6 5) =) true)

;; rem : X x [List X] x [Equality X] -> [Maybe [List X]]
;; Removes an element from the list, or returns false
;; if it isn't there
;; Written to increase speed of list=?
(define (rem x lox equals?)
  (if (empty? lox) false 
      (if (equals? x (first lox)) (rest lox)
          (local ((define y (rem x (rest lox) equals?)))
            (if (false? y) y (cons (first lox) y))))))
(check-expect (rem 5 empty =) false)
(check-expect (rem 5 '(5) =) empty)
(check-expect (rem 5 '(6 5 10) =) '(6 10))
(check-expect (rem 5 '(6 7 8) =) false)

;; inverse-ref : [List X] x X x [Equality X] -> Natural
;; The location of the element in the list
(define (inverse-ref lox x equals?)
  (if (empty? lox) (error "not here")
      (if (equals? x (first lox)) 0 (add1 (inverse-ref (rest lox) x equals?)))))
(check-expect (inverse-ref '(0 1 2 3) 2 =) 2)
(check-error (inverse-ref empty 2 =) "not here")

;; pair=? : [Equality X] x [Equality Y] -> [Equality [Pair X Y]]
(define (pair=? x-equals? y-equals?) 
  (λ (p1 p2) 
    (and (x-equals? (first p1) (first p2))
         (y-equals? (second p1) (second p2)))))
(check-expect ((pair=? symbol=? =)'(a 1) '(a 1)) true)
(check-expect ((pair=? symbol=? =)'(a 1) '(a 2)) false)
(check-expect ((pair=? symbol=? =)'(c 1) '(a 1)) false)

;; simple-map : [List X] x [List X] x [Equality X] -> [X -> X]
;; A simple map from corresponding elements in x1 to x2
;; (length x1) = (length x2)
(define (simple-map x1 x2 equals?)
  (λ (x)
    (local ((define (simple-map x1 x2)
              (if (empty? x1) (error "not here")
                  (if (equals? x (first x1)) (first x2) (simple-map (rest x1) (rest x2))))))
      (simple-map x1 x2))))
(check-expect ((simple-map '(0 1 2) '(3 4 5) =) 1) 4)
(check-error ((simple-map '(0 1 2) '(3 4 5) =) 3) "not here")

; ____                                    _        ___  _                            
; /   \  ` , __          ___  , __     ___/      .'   \ /        __.  `   ___    ___ 
; |,_-<  | |'  `.       /   ` |'  `.  /   |      |      |,---. .'   \ | .'   ` .'   `
; |    ` | |    |      |    | |    | ,'   |      |      |'   ` |    | | |      |----'
; `----' / /    |      `.__/| /    | `___,'       `.__, /    |  `._.' /  `._.' `.___,

;; Bin and Choice

;; ! : Natural -> Positive
;; x!
(define (! x)
  (if (zero? x) 1
      (local ((define (loop acc y)
                (if (= y 0) acc
                    (loop (* y acc) (sub1 y)))))
        (loop x (sub1 x)))))
(check-expect (! 0) 1)
(check-expect (! 1) 1)
(check-expect (! 5) 120)


;; kth-perm : Nat x [List X] -> [List X]
;; Create the kth permutation of x
;; Assumes no repeats
;; k < (length x)!
(define (kth-perm x k) 
  (if (empty? x) x 
      (local ((define x-1! (! (sub1 (length x))))
              (define element (list-ref x (quotient k x-1!))))
        (cons element (kth-perm (remove element x) (modulo k x-1!))))))
(check-expect (kth-perm '() 0) empty)
(check-expect (kth-perm '(1) 0) '(1))
(check-expect (kth-perm '(1 2 3) 0) '(1 2 3))
(check-expect (kth-perm '(1 2 3) 1) '(1 3 2))
(check-expect (kth-perm '(1 2 3) 2) '(2 1 3))
(check-expect (kth-perm '(1 2 3) 3) '(2 3 1))
(check-expect (kth-perm '(1 2 3) 4) '(3 1 2))
(check-expect (kth-perm '(1 2 3) 5) '(3 2 1))

;; choice-maker : Nat x [List Positive] -> Choice
;; x is a list of numbers representing how many
;; options can be chosen for each choice
;; n is which number choice you wish to make
;; n is in the range [0, (apply * x))
(define (choice-maker n x)
  (local ((define (f n x prod)
            (if (empty? x) x 
                (local ((define newprod (/ prod (first x))))
                  (cons (quotient n newprod) (f (modulo n newprod) (rest x) newprod))))))
    (f n x (apply * x))))
(check-expect (choice-maker 0 empty) empty)
(check-expect (choice-maker 0 '(1)) '(0))
(check-expect (choice-maker 0 '(1 2)) '(0 0))
(check-expect (choice-maker 1 '(1 2)) '(0 1))
(check-expect (build-list (apply * '(2 1 3)) (λ (n) (choice-maker n '(2 1 3))))
              '((0 0 0) (0 0 1) (0 0 2) (1 0 0) (1 0 1) (1 0 2)))

;; first-n : [List X] x Nat -> [List X]
;; first-n elements of a list
;; n <= (length lox)
(define (first-n lox n)
  (if (and (empty? lox) (not (zero? n))) (error "asked for too much")
      (if (zero? n) empty (cons (first lox) (first-n (rest lox) (sub1 n))))))
(check-error (first-n empty 1) "asked for too much")
(check-expect (first-n empty 0) empty)
(check-expect (first-n '(0 1 2 3) 2) '(0 1))

;; rest-n : [List X] x Nat -> [List X]
;; take the rest n many times
;; n <= (length lox)
(define (rest-n lox n)
  (if (and (empty? lox) (not (zero? n))) (error "asked for too much")
      (if (zero? n) lox (rest-n (rest lox) (sub1 n)))))
(check-error (rest-n empty 1) "asked for too much")
(check-expect (rest-n empty 0) empty)
(check-expect (rest-n '(0 1 2 3) 2) '(2 3))
(check-expect (append (first-n '(0 1 2 3 4 5 6) 2) (rest-n '(0 1 2 3 4 5 6) 2))
              '(0 1 2 3 4 5 6))

;; bin-swap : [List X] x Bin x Natural -> [List X]
;; The nth rearrangement of lox, based on the bin
;; n < (apply * (map ! bin))
(define (bin-swap lox bin n)
  (local ((define choice (choice-maker n (map ! bin)))
          (define (loop choice bin lox)
            (if (empty? choice) empty
                (local ((define perm (kth-perm (first-n lox (first bin)) (first choice))))
                  (append perm (loop (rest choice) 
                                     (rest bin) 
                                     (rest-n lox (first bin))))))))
    (loop choice bin lox)))
(define BIN-SWAP-TEST '((0 batman) (1 flash) (2 flash) (3 supes) 
                                   (4 supes) (5 supes) (6 wonderwoman)))
;; The bin sizes are 1, 2, 3, 1
(check-expect (choice-maker 10 (map ! '(1 2 3 1))) '(0 1 4 0))
;; The 0th, 1st, 4th, and 0th permutations of the bins will be used
(check-expect (kth-perm '((0 batman)) 0) '((0 batman)))
(check-expect (kth-perm '((1 flash) (2 flash)) 1) '((2 flash) (1 flash)))
(check-expect (kth-perm '((3 supes) (4 supes) (5 supes)) 4) 
              '((5 supes) (3 supes) (4 supes)))
(check-expect (kth-perm '((6 wonderwoman)) 0) '((6 wonderwoman)))
;; The actual bin-swap
(check-expect (bin-swap BIN-SWAP-TEST '(1 2 3 1) 10)
              '((0 batman)
                (2 flash) (1 flash)
                (5 supes) (3 supes) (4 supes)
                (6 wonderwoman)))

; __  __               _     
; |   |    ___    ____ /     
; |___|   /   `  (     |,---.
; |   |  |    |  `--.  |'   `
; /   /  `.__/| \___.' /    |

;; Hash

;; all-unique : [List X] x [Equality X] -> Boolean
;; Checks to see all members are unique
(define (all-unique x equals?)
  (or (empty? x) 
      (and (false? (rem (first x) (rest x) equals?))
           (all-unique (rest x) equals?))))
(check-expect (all-unique empty =) true)
(check-expect (all-unique '(1 2 3 4 5 6 7 8 9 10) =) true)
(check-expect (all-unique '(1 2 3 4 5 1 7 8 9 10) =) false)

;; pairing-function : Nat x Nat -> Positive
;; A hash for two nats
(define (pairing-function x y)
  (+ (add1 y) (* .5 (+ x (add1 y)) (+ 1 x (add1 y)))))
(check-expect (all-unique 
               (build-list 9 
                           (λ (n) (local ((define y (choice-maker n '(3 3))))
                                    (pairing-function (first y) (second y))))) =)
              true)

;; pair-off : [List Nat] -> [List Nat]
;; Pairs off a list of naturals by applying the pairing-function
;; to every pair of numbers
(define (pair-off x)
  (if (or (empty? x) (empty? (rest x))) x
      (cons (pairing-function (first x) (second x)) 
            (pair-off (rest (rest x))))))
(check-expect (pair-off empty) empty)
(check-expect (pair-off '(1)) '(1))
(check-expect (pair-off '(0 1)) (list (pairing-function 0 1)))
(check-expect (pair-off '(0 1 2 3)) 
              (list (pairing-function 0 1) 
                    (pairing-function 2 3)))
(check-expect (pair-off '(0 1 2 3 4)) 
              (list (pairing-function 0 1) (pairing-function 2 3) 4))

;; hash-nats : [List Nat] -> Integer
;; hash a list of nats
(define (hash-nats x)
  (local ((define (hash-nats x)
            (cond 
              [(empty? x) 0]
              [(empty? (rest x)) (first x)]
              [(empty? (rest (rest x))) (pairing-function (first x) (second x))]
              [else (hash-nats (pair-off x))])))
    (pairing-function (length x) (hash-nats x))))
(check-expect (hash-nats '(0 1 2 3 4))
              (pairing-function 5 
                                (pairing-function 
                                 (pairing-function 
                                  (pairing-function 0 1) 
                                  (pairing-function 2 3)) 
                                 4)))

;; all-unique-test : Natural -> Boolean
;; Makes sure hash-nats generates unique values
;; for all lists of size [0, n] 
;; with values from [0, n)
(define (all-unique-test n)
  (all-unique
   (apply append
          (build-list (add1 n)
                      (λ (power)
                        (build-list
                         (expt n power)
                         (λ (x)
                           (hash-nats
                            (choice-maker x (make-list power n))))))))
   =))
(check-expect (all-unique-test 4) true)

; .____                                 .                
; /      _  .-   ___  , _ , _   \,___,  |     ___    ____
; |__.    \,'   /   ` |' `|' `. |    \  |   .'   `  (    
; |       /\   |    | |   |   | |    |  |   |----'  `--. 
; /----/ /  \  `.__/| /   '   / |`---' /\__ `.___, \___.'

;; Examples

;; G1 is a [Graph Symbol]
(define G1
  (make-graph '(A B C D E F G)
              (lambda (n)
                (cond [(symbol=? n 'A) '(B E)]
                      [(symbol=? n 'B) '(E F)]
                      [(symbol=? n 'C) '(D)]
                      [(symbol=? n 'D) '()]
                      [(symbol=? n 'E) '(C F A)]
                      [(symbol=? n 'F) '(D G)]
                      [(symbol=? n 'G) '()]))
              symbol=?))
(check-expect ((graph-neighbors G1) 'A) '(B E))
(check-expect ((graph-neighbors G1) 'G) '())
;; SG1 is a [Connection-Graph Symbol] and represents
;; the connection-graph version of G1
(define CG1 (local ((define equals? (pair=? symbol=? =)))
              (make-graph 
               (map list '(A B C D E F G) '(403 403 4 2 23649 18 2))
               (lambda (n)
                 (cond [(equals? n '(A 403)) '((B 403) (E 23649))]
                       [(equals? n '(B 403)) '((E 23649) (F 18))]
                       [(equals? n '(C 4)) '((D 2))]
                       [(equals? n '(D 2)) '()]
                       [(equals? n '(E 23649)) '((C 4) (F 18) (A 403))]
                       [(equals? n '(F 18)) '((D 2) (G 2))]
                       [(equals? n '(G 2)) '()]))
               equals?)))
;; hashes for the indivudal nodes
(check-expect (hash-nats '(3 2)) 403) ; 'A 'B
(check-expect (hash-nats '(0)) 4) ; 'C 'G
(check-expect (hash-nats '()) 2) ; 'D
(check-expect (hash-nats '(2 2 1)) 23649) ; 'E
(check-expect (hash-nats '(0 0)) 18) ; 'F
(check-expect ((graph-neighbors CG1) '(A 403)) '((B 403) (E 23649)))
(check-expect ((graph-neighbors CG1) '(G 2)) '())

;; SSG1 is a [Sorted-Connection-Graph Symbol] and is
;; the sorted-connection-graph version of CG1
(define SCG1 (local ((define equals? (pair=? symbol=? =)))
               (make-graph 
                '((E 23649) (A 403) (B 403) (F 18) (C 4) (D 2)  (G 2))
                (graph-neighbors CG1)
                equals?)))
(check-expect ((graph-neighbors SCG1) '(A 403)) '((B 403) (E 23649)))
(check-expect ((graph-neighbors SCG1) '(G 2)) '())

;   ___                       _       
; .'   \  .___    ___  \,___, /       
; |       /   \  /   ` |    \ |,---. 
; |    _  |   ' |    | |    | |'   `  
;  `.___| /     `.__/| |`---' /    |  
;                      \            
; .____                      .
; /     ,   . , __     ___  _/_   `   __.  , __     ____
; |__.  |   | |'  `. .'   `  |    | .'   \ |'  `.  (    
; |     |   | |    | |       |    | |    | |    |  `--. 
; /     `._/| /    |  `._.'  \__/ /  `._.' /    | \___.'

;; Graph Functions

;; same-graph? : [Equality [Graph X]]
;; Are the two graphs the same?
(define (same-graph? g1 g2)
  (local ((define listx=? (λ (x y) (list=? x y (graph-node=? g1))))
          (define nodes1 (graph-nodes g1)))
    (and (listx=? nodes1 (graph-nodes g2))
         (andmap (λ (node) (listx=? ((graph-neighbors g1) node) 
                                    ((graph-neighbors g2) node))) 
                 nodes1))))
(check-expect (same-graph? (make-graph '() (lambda (x) '()) symbol=?)
                           (make-graph '() (lambda (x) '()) symbol=?)) true)
(check-expect (same-graph? (make-graph '(a) (lambda (x) '()) symbol=?)
                           (make-graph '() (lambda (x) '()) symbol=?)) false)
(check-expect (same-graph? (make-graph '(a) (lambda (x) '()) symbol=?)
                           (make-graph '(a) (lambda (x) '()) symbol=?)) true)
(check-expect (same-graph? (make-graph '(b) (lambda (x) '()) symbol=?)
                           (make-graph '(a) (lambda (x) '()) symbol=?)) false)
(check-expect (same-graph? (make-graph '(a b) (lambda (x) '()) symbol=?)
                           (make-graph '(b a) (lambda (x) '()) symbol=?)) true)
(check-expect (same-graph? (make-graph '(a b) 
                                       (lambda (x)
                                         (cond 
                                           [(symbol=? x 'b) '()]
                                           [(symbol=? x 'a) '(b)]))
                                       symbol=?)
                           (make-graph '(a b) 
                                       (lambda (x)
                                         (cond 
                                           [(symbol=? x 'b) '(a)] 
                                           [(symbol=? x 'a) '()]))
                                       symbol=?)) false)
(check-expect (same-graph? (make-graph '(a b) 
                                       (lambda (x)
                                         (cond 
                                           [(symbol=? x 'a) '(a b)] 
                                           [(symbol=? x 'b) '()]))
                                       symbol=?)
                           (make-graph '(a b) 
                                       (lambda (x)
                                         (cond 
                                           [(symbol=? x 'a) '(b a)] 
                                           [(symbol=? x 'b) '()]))
                                       symbol=?)) true)
(check-expect (same-graph? CG1 SCG1) true)

;; hash-node : [Graph X] x X -> Natural
;; The connection hash for an individual node
(define (hash-node g x)
  (hash-nats (sort (map (λ (node)
                          (length ((graph-neighbors g) node))) 
                        ((graph-neighbors g) x)) 
                   >)))
(check-expect (hash-node G1 'A) (hash-nats (list 3 2)))

;; hash-list : [Graph X] -> [List Natural]
;; A list of the hashes of the nodes
(define (hash-list g)
  (map (λ (x) (hash-node g x)) (graph-nodes g)))
(check-expect (hash-list (make-graph '(a) (λ (n) empty) symbol=?)) '(2))
(check-expect (hash-list G1) '(403 403 4 2 23649 18 2))

;; connection-graph : [Graph X] -> [Connection-Graph X]
;; Every node becomes its named paired with its connection hash
(define (connection-graph g)
  (local ((define newnodes (map list (graph-nodes g) (hash-list g)))
          (define (lookup oldnode newnodes)
            (if (empty? newnodes) (error "oldnode not found in newnodes")
                (if ((graph-node=? g) oldnode (first (first newnodes))) (first newnodes)
                    (lookup oldnode (rest newnodes))))))
    (make-graph newnodes
                (λ (newnode) 
                  (map (λ (oldnode) (lookup oldnode newnodes)) 
                       ((graph-neighbors g) (first newnode))))
                (pair=? (graph-node=? g) =))))
(check-expect (same-graph? (connection-graph G1) CG1) true)

;; sort-connection-graph : [Connection-Graph X] -> [Sorted-Connection-Graph X]
;; Sorts a connection graph in descending order by the node's number
(define (sort-connection-graph g)
  (make-graph (sort (graph-nodes g)
                    (λ (p1 p2) (> (second p1) (second p2))) )
              (graph-neighbors g) (graph-node=? g)))
(check-expect (same-graph? (sort-connection-graph CG1)
                           SCG1) true)
(check-expect (same-graph? CG1 SCG1) true)
(check-expect (apply >= (map second (graph-nodes (sort-connection-graph CG1)))) true)
(check-expect (graph-nodes (sort-connection-graph CG1)) (graph-nodes SCG1))

;; change-names : [Graph X] [List X] -> [Graph X]
;; Change the names of vertices within a graph
;; (length (graph-nodes g)) = (length x)
(define (change-names g x)
  (local ((define s-map (simple-map (graph-nodes g) x (graph-node=? g))))
    (make-graph x
                (λ (node) (map s-map 
                               ((graph-neighbors g) 
                                (list-ref (graph-nodes g)
                                          (inverse-ref x node (graph-node=? g))))))
                (graph-node=? g))))
(define g1 (change-names G1 '(a b c d e f g)))
(check-expect (graph-nodes g1)
              '(a b c d e f g))
(check-expect ((graph-neighbors g1) 'a)
              '(b e))

;; swap-names : [Graph X] x [Graph X] -> [Graph X]
;; Swap all names in the second graph with the names in the first graph
;; in order (note: graphs must be of the same size)
(define (swap-names ssg1 ssg2)
  (change-names ssg2 (graph-nodes ssg1)))
(define SCG2 (make-graph '((a 2) (b 0) (c 0)) 
                         (λ (node) (if ((pair=? symbol=? =) node '(a 2)) 
                                       '((a 2) (b 0)) '())) 
                         (pair=? symbol=? =)))
(define SCG3 (make-graph '((d 2) (e 0) (f 0)) 
                         (λ (node) (if ((pair=? symbol=? =) node '(d 2)) 
                                       '((d 2) (f 0)) '())) 
                         (pair=? symbol=? =)))
(check-expect ((graph-neighbors (swap-names SCG2 SCG3)) '(a 2)) '((a 2) (c 0)))
(check-expect (same-graph?  (swap-names SCG2 SCG3) SCG2) false)

;; bin-pair : [List (list X Natural)] -> Bin
;; Output how many times the second number occurs in a sorted list of pairs
(define (bin-pair x)
  (local ((define (bin-helper x)
            (cond [(empty? x) empty]
                  [(cons? x)
                   (local ((define r (bin-helper (rest x))))
                     (cond [(empty? r)  (list (list (second (first x)) 1))]
                           [(cons? r)
                            (cond [(= (first (first r)) (second (first x))) 
                                   (cons (list (first (first r))
                                               (add1 (second (first r))))
                                         (rest r))]
                                  [else (cons (list (second (first x)) 1) r)])]))])))
    (map second (bin-helper x))))
;; 2 appears 4 times, 0 appears once
(check-expect (bin-pair '((a 2) (b 2) (c 2) (c 2) (d 0))) '(4 1))
;; bin-scg : [Sorted-Connection-Graph X] -> Bin
;; bins an scg
(define (bin-scg scg)
  (bin-pair (graph-nodes scg)))
(check-expect (bin-scg SCG1) '(1 2 1 1 2))
(check-expect (bin-scg SCG2) '(1 2))
(check-expect (bin-scg SCG2) (bin-scg SCG3))

;; change-scg : [Sorted-Connection-Graph X] x Natural -> [Sorted-Connection-Graph X]
;; The nth refactoring of the scg
(define (change-scg scg n)
  (change-names scg (bin-swap (graph-nodes scg) (bin-scg scg) n)))
(check-expect (graph-nodes (change-scg SCG2 1)) (list (list 'a 2) (list 'c 0) (list 'b 0)))

;; perm-count : [Sorted-Connection-Graph X] -> Natural
;; How many permutations will need to be made?
(define (perm-count scg)
  (foldl (λ (bin-num product) (* (! bin-num) product)) 1 (bin-scg scg)))
(check-expect (perm-count SCG2) 2)
(check-expect (perm-count SCG1) 4)

;; natural-graph : [Graph X] -> [Graph Natural]
;; Convert the graph to a natural graph
;; where every node is now named by its place
;; in the original graph's list
(define (natural-graph g)
  (local ((define x->nat (λ (x) (inverse-ref (graph-nodes g) x (graph-node=? g)))))
    (make-graph (build-list (length (graph-nodes g)) identity) 
                (λ (n) (map x->nat ((graph-neighbors g) (list-ref (graph-nodes g) n))))
                =))) 
(check-expect (same-graph? (natural-graph G1)
                           (make-graph '(0 1 2 3 4 5 6)
                                       (lambda (n)
                                         (cond [(= n 0) '(1 4)]
                                               [(= n 1) '(4 5)]
                                               [(= n 2) '(3)]
                                               [(= n 3) '()]
                                               [(= n 4) '(2 5 0)]
                                               [(= n 5) '(3 6)]
                                               [(= n 6) '()]))
                                       =)) true)

;; nat-connection-graph : [Graph X] ->  [Sorted-Connection-Graph Natural]
;; Converts a graph into a sorted natural connection graph
(define (nat-connection-graph g)
  (sort-connection-graph (connection-graph (natural-graph g))))
(check-expect (same-graph? (nat-connection-graph G1)
                           (local ((define equals? (pair=? = =)))
                             (sort-connection-graph 
                              (make-graph 
                               (map list '(0 1 2 3 4 5 6) '(403 403 4 2 23649 18 2))
                               (lambda (n)
                                 (cond [(equals? n '(0 403)) '((1 403) (4 23649))]
                                       [(equals? n '(1 403)) '((4 23649) (5 18))]
                                       [(equals? n '(2 4)) '((3 2))]
                                       [(equals? n '(3 2)) '()]
                                       [(equals? n '(4 23649)) '((2 4) (5 18) (0 403))]
                                       [(equals? n '(5 18)) '((3 2) (6 2))]
                                       [(equals? n '(6 2)) '()]))
                               equals?)))) true)
;                                             _                            __  
;   ____   ___  , _ , _     ___          ____ /        ___  \,___,   ___  /  `.
;  (      /   ` |' `|' `. .'   ` .---'  (     |,---.  /   ` |    \ .'   ` `   '
;  `--.  |    | |   |   | |----'        `--.  |'   ` |    | |    | |----'    / 
; \___.' `.__/| /   '   / `.___,       \___.' /    | `.__/| |`---' `.___,   ,  
;                                                           \               ' 

;; same-shape?

;; same-shape-helper? : [Equality [Sorted-Connection-Graph Natural]]
;; Are the graphs of the same shape
(define (same-shape-helper g1 g2)
  (local ((define (loop n)
            (and (not (= n -1))
                 (or (same-graph? g1 (change-scg g2 n))
                     (loop (sub1 n))))))
    (loop (sub1 (perm-count g1)))))
;; same-shape? : [Equality Graph]
;; Do two graphs have the same shape?
(define (same-shape? g1 g2)
  (and (= (length (graph-nodes g1)) (length (graph-nodes g2)))
       (list=? (hash-list g1) (hash-list g2) =)
       (same-shape-helper (nat-connection-graph g1)
                          (swap-names (nat-connection-graph g1) 
                                      (nat-connection-graph g2)))))
(check-expect (same-shape? (make-graph '(a) (λ (n) empty) symbol=?) 
                           (make-graph '(a) (λ (n) empty) symbol=?)) true)
(check-expect (same-shape? (make-graph '(a) (λ (n) '(a)) symbol=?) 
                           (make-graph '(a) (λ (n) empty) symbol=?)) false)
(check-expect (same-shape? (make-graph '(a b c)
                                       (λ (node)
                                         (cond [(symbol=? node 'a) '(b c)]
                                               [(symbol=? node 'b) '(b c)]
                                               [(symbol=? node 'c) '(b)]))
                                       symbol=?)
                           (make-graph '(1 2 3)
                                       (λ (node)
                                         (cond 
                                           [(= node 3) '(1 2)]
                                           [(= node 1) '(1 2)]
                                           [(= node 2) '(1)])) =)) true)
(check-expect (same-shape? (make-graph '(a b c)
                                       (λ (node)
                                         (cond [(symbol=? node 'a) '(b c)]
                                               [(symbol=? node 'b) '(b c)]
                                               [(symbol=? node 'c) '(b)]))
                                       symbol=?)
                           (make-graph '(1 2 3)
                                       (λ (node)
                                         (cond 
                                           [(= node 3) '(1 2)]
                                           [(= node 1) '(1 3)]
                                           [(= node 2) '(1)])) =)) false)
(check-expect (same-shape? (make-graph '(a b c d e f g)
                                       (λ (node)
                                         (local ((define (f sym)
                                                   (symbol=? sym node)))
                                           (cond [(f 'a) '(f e g)]
                                                 [(f 'b) '()]
                                                 [(f 'c) '()]
                                                 [(f 'd) '(a c b)]
                                                 [(f 'e) '(e)]
                                                 [(f 'f) '(f)]
                                                 [(f 'g) '(c)]))) symbol=?)
                           (make-graph 
                            (kth-perm (list "a" "b" "c" "d" "e" "f" "g") (random (! 7)))
                            (λ (node)
                              (local ((define (f str)
                                        (string=? str node)))
                                (cond [(f "a") '()]
                                      [(f "b") '()]
                                      [(f "c") '("d" "e" "f")]
                                      [(f "d") '("d")]
                                      [(f "e") '("e")]
                                      [(f "f") '("a")]
                                      [(f "g") '("c" "a" "b")]))) string=?)) true)
(check-expect (same-shape? (make-graph '(a b c d e f g)
                                       (λ (node)
                                         (local ((define (f sym)
                                                   (symbol=? sym node)))
                                           (cond [(f 'a) '(f e g)]
                                                 [(f 'b) '()]
                                                 [(f 'c) '()]
                                                 [(f 'd) '(a c b)]
                                                 [(f 'e) '(e)]
                                                 [(f 'f) '(f)]
                                                 [(f 'g) '(c)]))) symbol=?)
                           (make-graph 
                            (kth-perm (list "a" "b" "c" "d" "e" "f" "g") (random (! 7)))
                            (λ (node)
                              (local ((define (f str)
                                        (string=? str node)))
                                (cond [(f "a") '()]
                                      [(f "b") '()]
                                      [(f "c") '("d" "e" "f")]
                                      [(f "d") '("d")]
                                      [(f "e") '("e")]
                                      [(f "f") '("a")]
                                      [(f "g") '("d" "a" "b")]))) string=?)) false)
;; Checks for equivalence relation

;; equivalence-check : [Equality X] -> [[List X] -> Boolean]
;; Generates a check for an equivalence 
(define (equivalence-check eq)
  (λ (listx)
    (local ((define (loop listx)
              (or (empty? listx)
                  (and (eq (first listx) (first listx))
                       (andmap (λ (x) (and (eq x (first listx))
                                           (eq (first listx) x))) (rest listx))
                       (loop (rest listx))))))
      (loop listx))))
(check-expect ((equivalence-check same-shape?) '()) true)
(check-expect ((equivalence-check =) '(5 5 5)) true)
(check-expect ((equivalence-check =) '(5 6 5)) false)
(check-expect ((equivalence-check same-shape?) `(,SCG2 ,SCG3)) true) 
(check-expect ((equivalence-check same-shape?) `(,g1 ,G1 ,CG1 ,SCG1)) true)
(check-expect ((equivalence-check same-shape?) `(,SCG1 ,SCG2)) false)