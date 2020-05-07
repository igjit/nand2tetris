assemble <- function(x) UseMethod("assemble")

assemble.a_command <- function(x) {
  val <- intToBits(x$int) %>%
    head(15) %>%
    rev %>%
    as.integer %>%
    str_c(collapse = "")
  str_c("0", val, collapse = "")
}

a_command <- function(int) {
  structure(list(int = int), class = "a_command")
}

c_command <- function(dest = NULL, comp = NULL, jump = NULL) {
  structure(list(dest = dest, comp = comp, jump = jump), class = "c_command")
}
