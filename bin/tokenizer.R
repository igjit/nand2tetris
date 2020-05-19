args <- commandArgs(trailingOnly = TRUE)
lib <- args[1]
infile <- args[2]
outfile <- args[3]

devtools::load_all(lib, quiet = TRUE)

read_jack(infile) %>%
    tokenize %>%
    as_xml %>%
    write_xml(outfile, options = c("format", "no_declaration"))
