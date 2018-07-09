#lang racket
; ttr.rkt
; AndrewJ 2018-04-25

; Play with TTR in Racket.
;
; Graphs in Racket don't support multiple edges, so we need to attach a list of properties to
; each edge, that show in particular the different colours. 

(require graph
         data/queue
         threading
         2htdp/batch-io)

;------------------------
; Route data
(struct R
  (colour length is-tunnel locos)
  #:transparent)

; Constructor for R
(define (make-R a b c d)
  ; Helper function
  (define (string->boolean s)
    (if (string=? (string-downcase s) "true") #t #f))

  (R (string->symbol a)
     (string->number b)
     (string->boolean c)
     (string->number d)))

; TTR scoring function for route length
(define (score len)
  (cond ((= len 3) 4)
        ((= len 4) 7)
        ((= len 6) 10)
        (else len)))

;------------------------
; Define the graph and edge properties

(define g0 (weighted-graph/undirected '()))
(define-edge-property *g* Route)

;========================
; Import data

; Turn CSV rows into a list
; convert-map :: [[String]] -> [(Vertex Vertex R)]

(define (convert-map rows)  
  (for/list ([row rows])
    (list (string->symbol (first row))
          (string->symbol (second row))
          (make-R (third row)
                  (fourth row)
                  (fifth row)
                  (sixth row)))))

;------------------------
; Load routes into the graph
; load-map :: Graph -> [String] -> ()

(define (load-map G routes)
  (for ([row routes])
    (let* ([source (first row)]
           [dest (second row)]
           [data (third row)])
      
      (add-vertex! G source)
      (add-vertex! G dest)

      ; Add multiple edges with different colours
      (if (has-edge? G source dest)
          (let* ([curr (Route source dest)]
                 [new (append curr (list data))])
            (Route-set! source dest new)
            #;(Route-set! dest source new))
          ;else
          (let ()
            (add-edge! G source dest (R-length data)) ; weighted edge
            (Route-set! source dest (list data))
            #;(Route-set! dest source (list data))) ; why??
          ))))

;========================
; Analysis

; Return a list of the unique vertices up to max hops from src
; get-nearest :: Graph -> Vertex (-> Integer) -> [Vertex]

(define (get-nearest G source
                     #:max-dist (max 2))
  (define acc (mutable-set source))

  ; Recursive search
  (define (add-nearest G src acc count)
    (cond ((> count 0)
           (for ([vertex (get-neighbors g0 src)])
             (set-add! acc vertex)
             (add-nearest G vertex acc (sub1 count)))))
    acc)

  (set->list (add-nearest G source acc max)))

;------------------------
; Create a subgraph from a subset of vertices in a graph
; subgraph :: Graph -> [Vertex] -> Graph

(define (subgraph G vertices)
  (define subg (graph-copy G))
  (define removals (remove* vertices (get-vertices subg)))
  (map (curry remove-vertex! subg) removals)
  subg)

;------------------------
; Define all paths between two vertices
; BEWARE, this can blow all your memory on a large graph

(define (all-paths G src dest)
  
  (define visited (mutable-set))
  (define path (make-queue))
  (define results (make-queue))

  ; Recursive function
  (define (all-paths-fn G start end visited path results)
    (set-add! visited start)
    (enqueue-front! path start)
    (if (eq? start end)
        #;(displayln (reverse (queue->list path)))
        (enqueue! results (queue->list path))
        ;else
        (for ([v (in-neighbors G start)]
              #:unless (set-member? visited v))
          (all-paths-fn G v end visited path results)))
    (dequeue! path)
    (set-remove! visited start))

  (all-paths-fn G src dest visited path results)
  (map reverse (queue->list results)))

;------------------------
; Get total over a path

(define (overlapping-pairs lst)
  (for/list ([i (sub1 (length lst))])
    (list (list-ref lst i)
          (list-ref lst (add1 i)))))

; Fold f/g over the pairs
(define (fold-path total-fn pair-fn G path)
  (apply total-fn
         (map (λ (p) (pair-fn G (first p) (second p)))
              (overlapping-pairs path)) ))

(define path-weight
  (curry fold-path + edge-weight))

(define path-score
  (curry fold-path + (compose score edge-weight)))

;------------------------
; Apply a function over all paths from start to end in G

(define (all-path-fn f G start end)
  (for/hash ([p (all-paths G start end)])
    (values p (f G p))))

(define all-path-weights (curry all-path-fn path-weight))
(define all-path-scores (curry all-path-fn path-score))

;========================
; Run

(define map-data
  (list-tail
   (read-csv-file "ttr-europe.csv") 1))

(load-map g0 (convert-map map-data))

(define subset (get-nearest g0 'Marseille #:max-dist 1))
(define g1 (subgraph g0 subset))

;========================
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define-test-suite ttr-tests
    (test-case "Simple stuff"
               (check-equal? (score 4) 7))
    
    (test-case "Graph load"
               (check-equal? (length map-data) 101)
               (check-equal? (length (get-edges g0)) 180)
               (check-equal? (edge-weight g1 'Marseille 'Paris) 4)
               (check-equal? (length (get-neighbors g0 'Paris)) 7)
               (check-equal? (length (get-nearest g0 'Marseille #:max-dist 2)) 15))

    (test-case "Subset"
               (check-equal? (length (get-vertices g1)) 6)
               (check-equal? (length (get-edges g1)) 16))
    
    (test-case "Finding and measuring paths"
               (define p (all-paths g1 'Marseille 'Paris))
               (check-equal? (length p) 4)
               (check-equal? (map (curry path-weight g0) p) '(8 10 4 5))
               (check-equal? (map (curry path-score g0) p) '(14 16 7 6)))
    )

  (run-tests ttr-tests))

; The End