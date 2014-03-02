;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname same-shape) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t quasiquote repeating-decimal #f #t none #f ())))
;; Data definition for a graph and saome graph examples and same-graph? tests
;; come from http://www.ccs.neu.edu/course/csu211/Assignments/10h.html
;; Written in Intermediate Student with lambda
;; By Matthew Singer

;; Problem: Given two graphs, determine if they have the same shape.
;; Two graphs have the same shape if, after removing all node names,
;; one can rearrange the nodes (without altering which edges connect 
;; to which node) and get back the same picture.

;; Document structure:
;; Data Definitions
;; List Functions (helpers)
;; Bin, Choice, and Factorial (helpers - explained in data definitions)
;; Examples (of graphs-related data definitions)
;; Graph Functions (helpers)
;; same-shape? (the final result and many tests)

;; To easily find one section, View -> Show Program Contour (or command+U/ctrl+U)

; .___           .                .___          ,__              .                          
; /   `    ___  _/_     ___       /   `    ___  /  ` ` , __   ` _/_   `   __.  , __     ____
; |    |  /   `  |     /   `      |    | .'   ` |__  | |'  `. |  |    | .'   \ |'  `.  (    
; |    | |    |  |    |    |      |    | |----' |    | |    | |  |    | |    | |    |  `--. 
; /---/  `.__/|  \__/ `.__/|      /---/  `.___, |    / /    | /  \__/ /  `._.' /    | \___.'

(define-struct graph (nodes neighbors node=?))
;; A [Graph X] is a (make-graph [List X] [X -> [List X]] [X X -> Boolean])
;; Invariant: For all nodes n a graph g,
;; (member n (remove n (graph-nodes g))) = false
;; (i.e. all node names are distinct)

;; A [Size-Graph X] is a [Graph (list X Natural)]
;; Where the number in the pair of the node is
;; how many neighbors the node has

;; A [Sorted-Size-Graph X] is a [Size-Graph x]
;; where the list of nodes is sorted in descending
;; order based on how many neighbors it has
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
;; where i = 1, j = 2, and k = 3

;; A [Pair X Y] is a (list X Y)

;; A [Maybe X] is one of:
;; - X
;; - false

; .               .         .____                      .                          
; /     `   ____ _/_        /     ,   . , __     ___  _/_   `   __.  , __     ____
; |     |  (      |         |__.  |   | |'  `. .'   `  |    | .'   \ |'  `.  (    
; |     |  `--.   |         |     |   | |    | |       |    | |    | |    |  `--. 
; /---/ / \___.'  \__/      /     `._/| /    |  `._.'  \__/ /  `._.' /    | \___.'

;; list=? : [List X] x [List X] x [X X -> Boolean] -> Boolean
;; Are the two lists the same? (order does not matter)
(define (list=? l1 l2 c)
  (or (and (empty? l1) (empty? l2))
      (and (cons? l1) (cons? l2)
           (local ((define x (rem (first l1) l2 c)))
             (and (not (false? x)) (list=? (rest l1) x c))))))
(check-expect (list=? empty empty =) true)
(check-expect (list=? empty '(5) =) false)
(check-expect (list=? '(5) empty =) false)
(check-expect (list=? '(5) '(5) =) true)
(check-expect (list=? '(5 6 7 8 9 10) '(10 9 8 7 6 5) =) true)
(check-expect (list=? '(5 6 6 7 8 9 7 10) '(10 7 6 9 8 7 6 5) =) true)

;; rem : X x [List X] x [X X -> Boolean] -> [Maybe [List X]]
;; Removes an element from the list, or returns false
;; if it isn't there
;; Written to increase speed of list=?
(define (rem x lox c)
  (if (empty? lox) false 
      (if (c x (first lox)) (rest lox)
          (local ((define y (rem x (rest lox) c)))
            (if (false? y) y (cons (first lox) y))))))
(check-expect (rem 5 empty =) false)
(check-expect (rem 5 '(5) =) empty)
(check-expect (rem 5 '(6 5 10) =) '(6 10))
(check-expect (rem 5 '(6 7 8) =) false)

;; reverse-ref : [List X] x X x [X X -> Boolean] -> Natural
;; The location of the element in the list
(define (reverse-ref lox x equals?)
  (if (empty? lox) (error "not here")
      (if (equals? x (first lox)) 0 (add1 (reverse-ref (rest lox) x equals?)))))
(check-expect (reverse-ref '(0 1 2 3) 2 =) 2)
(check-error (reverse-ref empty 2 =) "not here")

;; pair=? : [X X -> Boolean] x [Y Y -> Boolean] -> [[Pair X Y] x [Pair X Y]  -> Boolean]
(define (pair=? x-equals? y-equals?) 
  (λ (p1 p2) 
    (and (x-equals? (first p1) (first p2))
         (y-equals? (second p1) (second p2)))))
(check-expect ((pair=? symbol=? =)'(a 1) '(a 1)) true)
(check-expect ((pair=? symbol=? =)'(a 1) '(a 2)) false)
(check-expect ((pair=? symbol=? =)'(c 1) '(a 1)) false)

;; simple-map : [List X] x [List X] x [X X -> Boolean] -> [X -> X]
;; A simple map from corresponding elemnts in x1 to x2
;; (length x1) = (length x2)
(define (simple-map x1 x2 equals?)
  (λ (x)
    (local ((define (dumb-map x1 x2)
              (if (empty? x1) (error "not here")
                  (if (equals? x (first x1)) (first x2) (dumb-map (rest x1) (rest x2))))))
      (dumb-map x1 x2))))
(check-expect ((simple-map '(0 1 2) '(3 4 5) =) 1) 4)
(check-error ((simple-map '(0 1 2) '(3 4 5) =) 3) "not here")

; ____                     ___  _                                               
; /   \  ` , __          .'   \ /        __.  `   ___    ___                    
; |,_-<  | |'  `.        |      |,---. .'   \ | .'   ` .'   `                   
; |    ` | |    |        |      |'   ` |    | | |      |----'                   
; `----' / /    | ,       `.__, /    |  `._.' /  `._.' `.___, ,                 
;                 /                                           /                 
;                    _      .____                .                           .  
;   ___  , __     ___/      /       ___    ___  _/_     __.  .___  `   ___   |  
;  /   ` |'  `.  /   |      |__.   /   ` .'   `  |    .'   \ /   \ |  /   `  |  
; |    | |    | ,'   |      |     |    | |       |    |    | |   ' | |    |  |  
; `.__/| /    | `___,'      /     `.__/|  `._.'  \__/  `._.' /     / `.__/| /\__

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
              (define i (quotient k x-1!))
              (define element (list-ref x i)))
        (cons element (kth-perm (remove element x) (modulo k x-1!))))))
(check-expect (kth-perm '() 0) empty)
(check-expect (kth-perm '(1) 0) '(1))
(check-expect (kth-perm '(1 2 3) 0) '(1 2 3))
(check-expect (kth-perm '(1 2 3) 1) '(1 3 2))
(check-expect (kth-perm '(1 2 3) 2) '(2 1 3))
(check-expect (kth-perm '(1 2 3) 3) '(2 3 1))
(check-expect (kth-perm '(1 2 3) 4) '(3 1 2))
(check-expect (kth-perm '(1 2 3) 5) '(3 2 1))

;; choice-maker : Nat x [List Positive] -> [List Nat]
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
(check-expect (equal? (build-list (apply * '(2 1 3)) (λ (n) (choice-maker n '(2 1 3))))
                      '((0 0 0)
                        (0 0 1)
                        (0 0 2)
                        (1 0 0)
                        (1 0 1)
                        (1 0 2))) true)

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
                  (append perm (loop (rest choice) (rest bin) (rest-n lox (first bin))))))))
    (loop choice bin lox)))
(define BIN-SWAP-TEST '((0 batman) (1 flash) (2 flash) (3 supes) 
                                   (4 supes) (5 supes) (6 wonderwoman)))
;; The bin sizes are 1, 2, 3, 1
(check-expect (choice-maker 10 (map ! '(1 2 3 1))) '(0 1 4 0))
;; The 0th, 1st, 4th, and 0th permutations of the bins will be used
(check-expect (kth-perm '((0 batman)) 0) '((0 batman)))
(check-expect (kth-perm '((1 flash) (2 flash)) 1) '((2 flash) (1 flash)))
(check-expect (kth-perm '((3 supes) (4 supes) (5 supes)) 4) '((5 supes) (3 supes) (4 supes)))
(check-expect (kth-perm '((6 wonderwoman)) 0) '((6 wonderwoman)))
;; The actual bin-swap
(check-expect (bin-swap BIN-SWAP-TEST '(1 2 3 1) 10)
              '((0 batman)
                (2 flash) (1 flash)
                (5 supes) (3 supes) (4 supes)
                (6 wonderwoman)))



; .____                                 .                
; /      _  .-   ___  , _ , _   \,___,  |     ___    ____
; |__.    \,'   /   ` |' `|' `. |    \  |   .'   `  (    
; |       /\   |    | |   |   | |    |  |   |----'  `--. 
; /----/ /  \  `.__/| /   '   / |`---' /\__ `.___, \___.'

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
;; SG1 is a [Size-Graph Symbol] and represents
;; the size-graph version of G1
(define SG1 (local ((define equals? (pair=? symbol=? =)))
              (make-graph 
               (map list '(A B C D E F G) '(2 2 1 0 3 2 0))
               (lambda (n)
                 (cond [(equals? n '(A 2)) '((B 2) (E 3))]
                       [(equals? n '(B 2)) '((E 3) (F 2))]
                       [(equals? n '(C 1)) '((D 0))]
                       [(equals? n '(D 0)) '()]
                       [(equals? n '(E 3)) '((C 1) (F 2) (A 2))]
                       [(equals? n '(F 2)) '((D 0) (G 0))]
                       [(equals? n '(G 0)) '()]))
               equals?)))
(check-expect ((graph-neighbors SG1) '(A 2)) '((B 2) (E 3)))
(check-expect ((graph-neighbors SG1) '(G 0)) '())



;; SSG1 is a [Sorted-Size-Graph Symbol] and is
;; the sorted-size-graph version of SG1
(define SSG1 (local ((define equals? (pair=? symbol=? =)))
               (make-graph 
                '((E 3) (A 2) (B 2) (F 2) (C 1) (D 0) (G 0))
                (lambda (n)
                  (cond [(equals? n '(A 2)) '((B 2) (E 3))]
                        [(equals? n '(B 2)) '((E 3) (F 2))]
                        [(equals? n '(C 1)) '((D 0))]
                        [(equals? n '(D 0)) '()]
                        [(equals? n '(E 3)) '((C 1) (F 2) (A 2))]
                        [(equals? n '(F 2)) '((D 0) (G 0))]
                        [(equals? n '(G 0)) '()]))
                equals?)))
(check-expect ((graph-neighbors SSG1) '(A 2)) '((B 2) (E 3)))
(check-expect ((graph-neighbors SSG1) '(G 0)) '())

;   ___                       _           .____                      .
; .'   \  .___    ___  \,___, /           /     ,   . , __     ___  _/_   `   __.  , __     ____
; |       /   \  /   ` |    \ |,---.      |__.  |   | |'  `. .'   `  |    | .'   \ |'  `.  (    
; |    _  |   ' |    | |    | |'   `      |     |   | |    | |       |    | |    | |    |  `--. 
;  `.___| /     `.__/| |`---' /    |      /     `._/| /    |  `._.'  \__/ /  `._.' /    | \___.'
;                      \                                                                        
;; same-graph? : [Graph X] x [Graph X] -> Boolean
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
(check-expect (same-graph? SG1 SSG1) true)

;; neighbor-size : [Graph X] x X -> Natural
;; How many neighbors a single node has
(define (neighbor-size g x)
  (length ((graph-neighbors g) x)))
(check-expect (neighbor-size G1 'A) 2)

;; neighbor-size-list : [Graph X] -> [List Natural]
;; A list of how many neighbors each node has
(define (neighbor-size-list g)
  (map (λ (x) (neighbor-size g x)) (graph-nodes g)))
(check-expect (neighbor-size-list (make-graph '(a) (λ (n) empty) symbol=?)) '(0))
(check-expect (neighbor-size-list G1) '(2 2 1 0 3 2 0))

;; size-graph : [Graph X] -> [Size-Graph X]
;; Every node becomes its named paired with its neighbor size
(define (size-graph g)
  (local ((define newnodes (map list (graph-nodes g) (neighbor-size-list g)))
          (define (lookup oldnode newnodes)
            (if (empty? newnodes) (error "oldnode not found in newnodes")
                (if ((graph-node=? g) oldnode (first (first newnodes))) (first newnodes)
                    (lookup oldnode (rest newnodes))))))
    (make-graph newnodes
                (λ (newnode) 
                  (map (λ (oldnode) (lookup oldnode newnodes)) 
                       ((graph-neighbors g) (first newnode))))
                (pair=? (graph-node=? g) =))))
(check-expect (same-graph? (size-graph G1) SG1) true)

;; sort-size-graph : [Size-Graph X] -> [Sorted-Size-Graph X]
;; Sorts a size graph in descending order by size of nodes
(define (sort-size-graph g)
  (make-graph (sort (graph-nodes g)
                    (λ (p1 p2) (> (second p1) (second p2))) )
              (graph-neighbors g) (graph-node=? g)))
(check-expect (same-graph? (sort-size-graph SG1)
                           SSG1) true)
(check-expect (apply >= (map second (graph-nodes (sort-size-graph SG1)))) true)
(check-expect (graph-nodes (sort-size-graph SG1)) (graph-nodes SSG1))

;; swap-names : [Sorted-Size-Graph X] x [Sorted-Size-Graph X] -> [Sorted-Size-Graph X]
;; Swap all names in the second graph with the names in the first graph
;; in order (note: graphs must be of the same size and each corresponding node
;; must have the same number of connections)
(define (swap-names ssg1 ssg2)
  (local ((define p=? (graph-node=? ssg1)))
    (make-graph (graph-nodes ssg1)
                (λ (newnodename) 
                  (local ((define oldlocation (reverse-ref (graph-nodes ssg1) newnodename p=?))
                          (define oldnode (list-ref (graph-nodes ssg2) oldlocation))
                          (define oldneighbors ((graph-neighbors ssg2) oldnode))
                          (define (oldnode->newnode oldnode)
                            (list-ref (graph-nodes ssg1) 
                                      (reverse-ref (graph-nodes ssg2) oldnode p=?))))
                    (map oldnode->newnode oldneighbors)))
                p=?)))
(define SSG2 (make-graph '((a 2) (b 0) (c 0)) 
                         (λ (node) (if ((pair=? symbol=? =) node '(a 2)) '((a 2) (b 0)) '())) 
                         (pair=? symbol=? =)))
(define SSG3 (make-graph '((d 2) (e 0) (f 0)) 
                         (λ (node) (if ((pair=? symbol=? =) node '(d 2)) '((d 2) (f 0)) '())) 
                         (pair=? symbol=? =)))
(check-expect ((graph-neighbors (swap-names SSG2 SSG3)) '(a 2)) '((a 2) (c 0)))
(check-expect (same-graph?  (swap-names SSG2 SSG3) SSG2) false)

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
;; bin-ssg : [Sorted-Size-Graph X] -> Bin
;; bins an ssg
(define (bin-ssg ssg)
  (bin-pair (graph-nodes ssg)))
(check-expect (bin-ssg SSG1) '(1 3 1 2))
(check-expect (bin-ssg SSG2) '(1 2))
(check-expect (bin-ssg SSG2) (bin-ssg SSG3))

;; change-names : [Graph X] [List X] -> [Graph X]
;; Change the names of vertices within a graph
;; (length (graph-nodes g)) = (length x)
(define (change-names g x)
  (local ((define s-map (simple-map (graph-nodes g) x (graph-node=? g))))
    (make-graph (map s-map (graph-nodes g))
                (λ (node) (map s-map 
                               ((graph-neighbors g) 
                                (list-ref (graph-nodes g)
                                          (reverse-ref x node (graph-node=? g))))))
                (graph-node=? g))))
(define SYMBOLAPPEND1 (λ (sym) (string->symbol (string-append "1" (symbol->string sym)))))
(check-expect (SYMBOLAPPEND1 'A) '1A)
(check-expect (graph-nodes (change-names G1 (map SYMBOLAPPEND1 (graph-nodes G1))))
              '(1A 1B 1C 1D 1E 1F 1G))
(check-expect ((graph-neighbors (change-names G1 (map SYMBOLAPPEND1 (graph-nodes G1)))) '1A)
              '(1B 1E))

;; change-ssg : [Sorted-Size-Graph X] x Natural -> [Sorted-Size-Graph X]
;; The nth refactoring of the ssg
(define (change-ssg ssg n)
  (change-names ssg (bin-swap (graph-nodes ssg) (bin-ssg ssg) n)))
(check-expect (graph-nodes (change-ssg SSG2 1)) (list (list 'a 2) (list 'c 0) (list 'b 0)))

;; perm-count : [Sorted-Size-Graph X] -> Natural
;; How many permutations will need to be made?
(define (perm-count ssg)
  (foldl (λ (bin-num product) (* (! bin-num) product)) 1 (bin-ssg ssg)))
(check-expect (perm-count SSG2) 2)
(check-expect (perm-count SSG1) 12)

;; natural-graph : [Graph X] -> [Graph Natural]
;; Convert the graph to a natural graph
;; where every node is now named by its place
;; in the original graph's list
(define (natural-graph g)
  (local ((define x->nat (λ (x) (reverse-ref (graph-nodes g) x (graph-node=? g)))))
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

;; nat-size-graph : [Graph X] ->  [Sorted-Size-Graph Natural]
;; Converts a graph into a sorted natural size graph
(define (nat-size-graph g)
  (sort-size-graph (size-graph (natural-graph g))))
(check-expect (same-graph? (nat-size-graph G1)
                           (local ((define equals? (pair=? = =)))
                             (sort-size-graph (make-graph 
                                               (map list '(0 1 2 3 4 5 6) '(2 2 1 0 3 2 0))
                                               (lambda (n)
                                                 (cond [(equals? n '(0 2)) '((1 2) (4 3))]
                                                       [(equals? n '(1 2)) '((4 3) (5 2))]
                                                       [(equals? n '(2 1)) '((3 0))]
                                                       [(equals? n '(3 0)) '()]
                                                       [(equals? n '(4 3)) '((2 1) (5 2) (0 2))]
                                                       [(equals? n '(5 2)) '((3 0) (6 0))]
                                                       [(equals? n '(6 0)) '()]))
                                               equals?)))) true)
;                                             _                            __  
;   ____   ___  , _ , _     ___          ____ /        ___  \,___,   ___  /  `.
;  (      /   ` |' `|' `. .'   ` .---'  (     |,---.  /   ` |    \ .'   ` `   '
;  `--.  |    | |   |   | |----'        `--.  |'   ` |    | |    | |----'    / 
; \___.' `.__/| /   '   / `.___,       \___.' /    | `.__/| |`---' `.___,   ,  
;                                                           \               '  

;; same-shape-helper? :  [Sorted-Size-Graph Natural] x  [Sorted-Size-Graph Natural] -> Boolean
;; Are the graphs of the same shape
(define (same-shape-helper g1 g2)
  (local ((define (loop n)
            (and (not (= n -1))
                 (or (same-graph? g1 (change-ssg g2 n))
                     (loop (sub1 n))))))
    (loop (sub1 (perm-count g1)))))
;; same-shape? : [Graph X] x [Graph Y] -> Boolean
;; Do two graphs have the same shape?
(define (same-shape? g1 g2)
  (and (= (length (graph-nodes g1)) (length (graph-nodes g2)))
       (list=? (neighbor-size-list g1) (neighbor-size-list g2) =)
       (same-shape-helper (nat-size-graph g1)
                          (swap-names (nat-size-graph g1) (nat-size-graph g2)))))
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
(check-expect (same-shape? SSG2 SSG3) true)