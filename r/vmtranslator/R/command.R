command <- function(name, arg1 = NULL, arg2 = NULL) {
  structure(list(name = name, arg1 = arg1, arg2 = arg2), class = "command")
}
