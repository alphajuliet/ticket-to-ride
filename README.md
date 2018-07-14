# Ticket to Ride explorations

Play with TTR Europe data. 

I first manually encoded the Europe map as RDF (Turtle) and uploaded it to an RDF triple store. I used R to query it using SPARQL and analyse with `igraph` and `ggraph`. I also exported it as CSV to keep it local.

I've also explored the graph approach with other languages: Racket and SageMath. Both have graph libraries. SageMath's is more sophisticated mathematically, as you'd expect, but Racket gives more programmatic insertion points in the graph functions. I also prefer the Lisp family to Python. 

From a data analytical and visualisation point of view, R wins, but SageMath has so far provided the easiest approach to developing route scoring between cities, with a combination of shallow breadth-first search, and easy subgraph generation and manipulation.

I've re-implemented a lot of the graph algorithms from the SageMath version in the Racket source, and developed a
`best-path` function that gives reasonable results between two cities.  
