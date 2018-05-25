#lang racket
; ttr.rkt
; AndrewJ 2018-04-25

; Play with TTR in Racket.
;
; Graphs in Racket don't support multiple edges, so we need to attach a list of properties to
; each edge, that show in particular the different colours. 

(require graph
         2htdp/batch-io)

(struct RouteData
  (colour length is-tunnel locos)
  #:transparent)

; Define the graph and edge properties
(define *g* (weighted-graph/undirected '()))
(define-edge-property *g* Data)

(define (string->boolean s)
  (if (string=? (string-downcase s) "true") #t #f))

; Load routes into the graph
(define (load-map routes)
  (for ((row routes))
    (let* ([source (string->symbol(first row))]
           [dest (string->symbol(second row))]
           [length (string->number (fourth row))]
           [data (RouteData (string->symbol(third row))
                            length
                            (string->boolean(fifth row))
                            (string->number(sixth row)))])
      
      (add-vertex! *g* source)
      (add-vertex! *g* dest)

      ; Add multiple edges with different colours
      (if (has-edge? *g* source dest)
          (let ([curr (Data source dest)])
            (Data-set! source dest (append curr (list data))))
          ;else
          (let ()
            (add-edge! *g* source dest length) ; weighted edge
            (Data-set! source dest (list data))
            (Data-set! dest source (list data))) ; why??
          )
      )
    ))

(define (list-edges graph)
  (let ([edges (Data->hash)])
    (foldl append '() (hash-values edges))))

; Import data
(define map-data
  (list-tail
   (read-csv-file "ttr-europe.csv") 1))

(load-map map-data)
(length (get-edges *g*)) ; Should be 200, but is reporting 180 because of multiple edges


; The End