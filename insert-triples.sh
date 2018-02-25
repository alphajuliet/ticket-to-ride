#!/bin/sh

curl -X PUT -H "Content-Type: text/plain" --data-binary @ttr-europe.ttl \
  https://dydra.com/alphajuliet/ttr-europe/statements

