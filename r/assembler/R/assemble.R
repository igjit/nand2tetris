a_command <- function(int) {
  structure(list(int = int), class = "a_command")
}

c_command <- function(dest = NULL, comp = NULL, jump = NULL) {
  structure(list(dest = dest, comp = comp, jump = jump), class = "c_command")
}
