#!/bin/bash

lib="$(dirname $(dirname $0))/r/jackanalyzer"
script="$(dirname $0)/tokenizer.R"
infile="$1"
outfile="${infile%.jack}T.my.xml"

echo "Tokenizing $infile"

Rscript "$script" "$lib" "$infile" "$outfile"
