parse <- function(filename) {
  readLines(filename) %>%
    extract_commands %>%
    map(parse_command)
}

#' @import purrr
#' @import stringr
extract_commands <- function(lines) {
  lines %>%
    str_remove("//.+") %>%
    str_trim() %>%
    keep(~ str_length(.) > 0)
}

parse_command <- function(string) {
  token <- str_split(string, "\\s+")[[1]]
  arg1 <- if (length(token) > 1) token[2]
  arg2 <- if (length(token) > 2) as.integer(token[3])
  command(token[1], arg1, arg2)
}
