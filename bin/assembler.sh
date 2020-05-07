#!/bin/bash

lib="$(dirname $(dirname $0))/r/assembler"
infile="$1"
outfile="${infile%.asm}.my.hack"

echo "Assembling $outfile"

Rscript -e "devtools::load_all('$lib', quiet = TRUE); writeLines(to_hack(parse('$infile')))" > "$outfile"
