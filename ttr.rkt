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
            (add-edge! G source dest length) ; weighted edge
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
; @@ WIP

(define (all-paths G src dest)
  
  (define visited (mutable-set))
  (define path (make-queue))
  (define results (make-queue))

  ; Recursive function
  (define (all-paths-fn G u d visited path results)
    (set-add! visited u)
    (enqueue-front! path u)
    (if (eq? u d)
        #;(displayln (reverse (queue->list path)))
        (enqueue! results (queue->list path))
        ;else
        (for ([v (in-neighbors G u)]
              #:unless (set-member? visited v))
          (all-paths-fn G v d visited path results)))
    (dequeue! path)
    (set-remove! visited u))

  (all-paths-fn G src dest visited path results)
  (map reverse (queue->list results)))

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
    (check-equal? (length map-data) 101)
    (check-equal? (length (get-edges g0)) 180)
    (check-equal? (length (get-neighbors g0 'Paris)) 7)
    (check-equal? (length (get-nearest g0 'Marseille #:max-dist 2)) 15)

    (check-equal? (length (get-vertices g1)) 6)
    (check-equal? (length (get-edges g1)) 16)

    (check-equal? (length (all-paths g1 'Marseille 'Paris)) 4)
    )

  
  (run-tests ttr-tests))

; The End