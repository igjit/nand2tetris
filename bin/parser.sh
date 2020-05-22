#!/bin/bash

lib="$(dirname $(dirname $0))/r/jackanalyzer"
script="$(dirname $0)/parser.R"
infile="$1"
outfile="${infile%.jack}.my.xml"

echo "Parsing $infile"

set -e
Rscript "$script" "$lib" "$infile" "$outfile"
sed -i -r -e 's/(\s+)<(.+)\/>/\1<\2>\n\1<\/\2>/' "$outfile"
