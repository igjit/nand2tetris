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
  if (str_starts(string, "@")) parse_a_command(string)
  else if (str_detect(string, "[=;]")) parse_c_command(string)
  else stop("Unknown command: ", string)
}

parse_a_command <- function(string) {
  sym <- str_match(string, "@(.+)")[, 2]
  int <- as.integer(sym)
  a_command(int = int)
}

parse_c_command <- function(string) {
  dest_comp_jump <- str_split(string, ";")[[1]]
  jump <- dest_comp_jump[2]
  if (is.na(jump)) jump <- NULL

  dest_comp <- str_split(dest_comp_jump[1], "=")[[1]]
  if (length(dest_comp) == 1) {
    dest <- NULL
    comp <- dest_comp[1]
  } else {
    dest <- dest_comp[1]
    comp <- dest_comp[2]
  }

  c_command(dest, comp, jump)
}

a_command <- function(int) {
  structure(list(int = int), class = "a_command")
}

c_command <- function(dest = NULL, comp = NULL, jump = NULL) {
  structure(list(dest = dest, comp = comp, jump = jump), class = "c_command")
}
