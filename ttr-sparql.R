# ttr-sparql.R
# aj 2018-03-04

# Query TTR Europe data from a SPARQL endpoint

library(tidyverse)
library(SPARQL)

#----------------------
# Set up namespace prefixes and SPARQL endpoint

prefixes = c(
  'rdf',  'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  'rdfs', 'http://www.w3.org/2000/01/rdf-schema#',
  'xsd',  'http://www.w3.org/2001/XMLSchema#',
  'skos', 'http://www.w3.org/2004/02/skos/core#',
  'ttr',  'http://alphajuliet.com/ns/ttr#')

endpoint = 'https://dydra.com/alphajuliet/ttr-europe/sparql'

q_sparql <- partial(SPARQL, url = endpoint, ns = prefixes, format = "xml")
qs <- function(q) {
  q_sparql(query = q)$results %>% as.tibble()
}

# SPARQL Queries

#----------------------
# First 20 triples in the repo

q_first20 = "
select * where {
?s ?p ?o
} limit 20"

#----------------------
# Return the distributions by colour

q_sbc = "
prefix : <http://alphajuliet.com/ns/ttr#>
select ?colour (count(?r) as ?routes) (sum(?len) as ?length)
where {
?r a :Route ;
:colour ?c ;
:length ?len .
?c rdfs:label ?colour .
} 
group by ?colour
order by desc(?length)"

#----------------------
# Return the distributions by length

q_dlen = "
prefix : <http://alphajuliet.com/ns/ttr#>
select ?len (count(?r) as ?number)
where {
?r a :Route ;
:length ?len .
} 
group by ?len
order by ?len"

#----------------------
# Return all route data

q_all = "
prefix : <http://alphajuliet.com/ns/ttr#>
select distinct ?id ?name1 ?name2 ?colour ?length ?isTunnel ?locos
where {
?id a :Route ;
:colour ?clr ;
:length ?length ;
:isTunnel ?isTunnel ;
:locomotives ?locos ;
:end ?city1 , ?city2 .
?city1 rdfs:label ?name1 .
?city2 rdfs:label ?name2 .
?clr rdfs:label ?colour .
filter (?city1 < ?city2)
}"

# The End