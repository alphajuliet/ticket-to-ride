#lang racket
; ttr.rkt
; AndrewJ 2018-04-25

; Play with TTR in Racket.
;
; Graphs in Racket don't support multiple edges, so we need to attach a list of properties to
; each edge, that show in particular the different colours. 

(require graph
         threading
         2htdp/batch-io)

; Route data
(struct R
  (colour length is-tunnel locos)
  #:transparent)

; Define the graph and edge properties
(define *g* (weighted-graph/undirected '()))
(define-edge-property *g* Route)

;------------------------
; Import data


; Turn CSV rows into a list
(define (convert-map rows)

  ; Helper function
  (define (string->boolean s)
    (if (string=? (string-downcase s) "true") #t #f))
  
  (for/list ([row rows])
    (list (string->symbol (first row))
          (string->symbol (second row))
          (string->symbol (third row))
          (string->number (fourth row))
          (string->boolean (fifth row))
          (string->number (sixth row)))))

; Load routes into the graph
(define (load-map G routes)
  (for ([row routes])
    (let* ([source (first row)]
           [dest (second row)]
           [colour (third row)]
           [length (fourth row)]
           [data (R colour
                    length
                    (fifth row)
                    (sixth row))])
      
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

;------------------------
; Analysis

; Return a list of the unique vertices up to max hops from src
(define (get-nearest G source
                     #:max-dist (max 2))
  (define acc (mutable-set)) ;empty set

  ; Recursive breadth-first search
  (define (add-nearest G src acc count)
    (cond ((> count 0)
           (for ([vertex (get-neighbors *g* src)])
             (set-add! acc vertex)
             (add-nearest G vertex acc (sub1 count)))))
    acc)

  (set->list (add-nearest G source acc max)))

; Create a subgraph from a subset of vertices in a graph
(define (subgraph G vertices)
  (define subg (graph-copy G))
  (define removals (remove* vertices (get-vertices subg)))
  (map (curry remove-vertex! subg) removals)
  subg)

; Define all paths between two vertices
(define (all-paths G src dest)
  #f)

;------------------------
; Run

(define map-data
  (list-tail
   (read-csv-file "ttr-europe.csv") 1))

(load-map *g* (convert-map map-data))

;------------------------
(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define subset (append (get-nearest *g* 'Marseille #:max-dist 1)
                         (list 'Marseille)))
  (define g1 (subgraph *g* subset))

  (define-test-suite ttr-tests
    (check-equal? (length map-data) 101)
    (check-equal? (length (get-edges *g*)) 180)
    (check-equal? (length (get-neighbors *g* 'Paris)) 7)
    (check-equal? (length (get-nearest *g* 'Marseille #:max-dist 2)) 15)
    (check-equal? (length (get-edges g1)) 16))
  
  (run-tests ttr-tests))

; The End