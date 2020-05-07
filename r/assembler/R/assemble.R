assemble <- function(x) UseMethod("assemble")

assemble.a_command <- function(x) {
  str_c("0", as_bin(x$int, 15), collapse = "")
}

as_bin <- function(int, digit) {
  intToBits(int) %>%
    head(digit) %>%
    rev %>%
    as.integer %>%
    str_c(collapse = "")
}

a_command <- function(int) {
  structure(list(int = int), class = "a_command")
}

c_command <- function(comp, dest = NULL, jump = NULL) {
  structure(list(comp = comp, dest = dest, jump = jump), class = "c_command")
}
