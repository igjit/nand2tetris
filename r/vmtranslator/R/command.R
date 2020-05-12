to_asm <- function(commands) {
  gen_label <- label_generator()
  commands %>%
    map(translate_command, gen_label) %>%
    flatten_chr
}

translate_command <- function(command, gen_label) {
  fn <- dispatch_table[[command$name]]
  if (is.null(fn)) stop("Not implemented: ", command$name)
  fn(command$arg1, command$arg2, gen_label = gen_label)
}

dispatch_table <- list(
  push = function(arg1, arg2, ...) {
    switch(arg1,
           "constant" = c(str_c("@", arg2),
                          "D=A",
                          "@SP",
                          "A=M",
                          "M=D",
                          "@SP",
                          "M=M+1"),
           stop("Not implemented: ", arg1))
  },
  add = function(...) c("@SP",
                        "A=M-1",
                        "D=M",
                        "A=A-1",
                        "M=D+M",
                        "@SP",
                        "M=M-1"),
  sub = function(...) c("@SP",
                        "A=M-1",
                        "D=M",
                        "A=A-1",
                        "M=M-D",
                        "@SP",
                        "M=M-1"),
  eq = function(..., gen_label) compare_command("JEQ", gen_label()),
  gt = function(..., gen_label) compare_command("JGT", gen_label()),
  lt = function(..., gen_label) compare_command("JLT", gen_label()))

compare_command <- function(jump, label) {
  c("@SP",
    "A=M-1",
    "D=M",
    "A=A-1",
    "D=M-D",
    "M=-1",
    at(label),
    str_c("D;", jump),
    "@SP",
    "A=M-1",
    "A=A-1",
    "M=0",
    sym(label),
    "@SP",
    "M=M-1")
}

label_generator <- function() {
  i <- 0
  prefix <- "LABEL"
  function() {
    label <- str_c(prefix, i)
    i <<- i + 1
    label
  }
}

at <- function(value) str_c("@", value)

sym <- function(label) str_c("(", label, ")")

command <- function(name, arg1 = NULL, arg2 = NULL) {
  structure(list(name = name, arg1 = arg1, arg2 = arg2), class = "command")
}
