#!/bin/bash

lib="$(dirname $(dirname $0))/r/vmtranslator"
infile="$1"

if [[ "$infile" =~ ".vm" ]]; then
    outfile="${infile%.vm}.asm"
else
    base="$(basename $infile)"
    outfile="$infile/$base.asm"
fi

echo "Translating $infile"

Rscript -e "devtools::load_all('$lib', quiet = TRUE); writeLines(to_asm(parse('$infile')))" > "$outfile"
