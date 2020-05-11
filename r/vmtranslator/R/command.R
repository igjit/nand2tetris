translate_command <- function(command) {
  fn <- dispatch_table[[command$name]]
  if (is.null(fn)) stop("Not implemented: ", command$name)
  fn(command$arg1, command$arg2)
}

dispatch_table <- list(
  push = function(arg1, arg2) {
    switch(arg1,
           "constant" = c(str_c("@", arg2),
                          "D=A",
                          "@SP",
                          "A=M",
                          "M=D",
                          "@SP",
                          "M=M+1"),
           stop("Not implemented: ", arg1))
  })

command <- function(name, arg1 = NULL, arg2 = NULL) {
  structure(list(name = name, arg1 = arg1, arg2 = arg2), class = "command")
}
