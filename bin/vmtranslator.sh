#!/bin/bash

lib="$(dirname $(dirname $0))/r/vmtranslator"
infile="$1"
outfile="${infile%.vm}.asm"

echo "Translating $infile"

Rscript -e "devtools::load_all('$lib', quiet = TRUE); writeLines(to_asm(parse('$infile')))" > "$outfile"
