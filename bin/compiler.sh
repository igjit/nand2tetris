#!/bin/bash

lib="$(dirname $(dirname $0))/r/jackanalyzer"
script="$(dirname $0)/compiler.R"
infile="$1"
outfile="${infile%.jack}.my.vm"

echo "Compiling $infile"

set -e
Rscript "$script" "$lib" "$infile" > "$outfile"
