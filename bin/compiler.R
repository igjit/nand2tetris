args <- commandArgs(trailingOnly = TRUE)
lib <- args[1]
infile <- args[2]

devtools::load_all(lib, quiet = TRUE)

read_jack(infile) %>%
  tokenize %>%
  parse %>%
  compile %>%
  writeLines
