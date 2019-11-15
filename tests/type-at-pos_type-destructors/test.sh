#!/bin/bash

queries_in_file "type-at-pos" "exact.js"
queries_in_file "type-at-pos" "react-component.js"
queries_in_file "type-at-pos" "spread.js"
queries_in_file "type-at-pos" "type-destructor.js"
queries_in_file "type-at-pos" "type-destructor.js" "--evaluate-type-destructors"
