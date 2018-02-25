#!/usr/bin/env ruby
# rdf_store.rb
# aj 2017-06-03

$:.unshift File.dirname(__FILE__)

require 'rdf'
require 'rdf/turtle'
require 'sparql/client'
require 'my_prefixes'

class DydraStore

  # Constructor
  def initialize
    @endpoint = "http://dydra.com/alphajuliet/ttr-europe/sparql"
    @sparql = SPARQL::Client.new(@endpoint)
  end

  # Run a SPARQL query against an endpoint
  def query(q)
    @sparql.query(q)
  end

  # Print out the raw responses
  def inspect_resp(q)
    query(q).each_solution do |solution|
      puts solution.inspect
    end
  end

  # Clear all triples from the repo
  def clear_all
    resp = @sparql.query("clear all")
    resp.inspect
  end

  # Load new triples into the repo
  def insert_triples
    @triples = RDF::Graph.load("ttr-europe.ttl", format: :ttl)
  end

  #--------------------------------
  def first20
    inspect_resp("select * where { ?s ?p ?o . } limit 10")
  end

end

st = DydraStore.new
st.first20

# The End
