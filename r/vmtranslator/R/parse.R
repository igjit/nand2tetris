parse <- function(filename) {
  if (str_ends(filename, "\\.vm")) {
    parse_file(filename)
  } else {
    list.files(filename, "*.vm", full.names = TRUE) %>%
      map(parse_file) %>%
      flatten
  }
}

parse_file <- function(filename) {
  scope <- basename(filename) %>%
    str_remove(".vm")

  readLines(filename) %>%
    extract_commands %>%
    map(parse_command, scope)
}

#' @import purrr
#' @import stringr
extract_commands <- function(lines) {
  lines %>%
    str_remove("//.+") %>%
    str_trim() %>%
    keep(~ str_length(.) > 0)
}

parse_command <- function(string, scope = NULL) {
  token <- str_split(string, "\\s+")[[1]]
  arg1 <- if (length(token) > 1) token[2]
  arg2 <- if (length(token) > 2) as.integer(token[3])
  command(token[1], arg1, arg2, scope)
}
