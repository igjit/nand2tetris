R_THIS <- 3
R_TEMP0 <- 5

NAMESPACE_GLOBAL <- "GLOBAL"

to_asm <- function(commands) {
  gen_label <- label_generator()

  sys_init <- detect(commands, ~ .$name == "function" && .$arg1 == "Sys.init")
  bootstrap <- if (!is.null(sys_init)) bootstrap_code(gen_label)

  commands %>%
    map(translate_command, gen_label) %>%
    flatten_chr %>%
    c(bootstrap, .)
}

bootstrap_code <- function(gen_label) {
  c("@256",
    "D=A",
    "@SP",
    "M=D",
    call_command("Sys.init", 0, gen_label))
}

translate_command <- function(command, gen_label) {
  fn <- dispatch_table[[command$name]]
  if (is.null(fn)) stop("Not implemented: ", command$name)
  fn(command$arg1, command$arg2, gen_label = gen_label, scope = command$scope)
}

dispatch_table <- list(
  push = function(arg1, arg2, ..., scope) {
    switch(arg1,
           constant = c(at(arg2),
                        "D=A",
                        "@SP",
                        "A=M",
                        "M=D",
                        "@SP",
                        "M=M+1"),
           argument = push_command("ARG", arg2),
           local = push_command("LCL", arg2),
           this = push_command("THIS", arg2),
           that = push_command("THAT", arg2),
           temp = push_address_command(R_TEMP0 + arg2),
           pointer = push_address_command(R_THIS + arg2),
           static = push_address_command(static_symbol(scope, arg2)),
           stop("Not implemented: ", arg1))
  },
  pop = function(arg1, arg2, ..., scope) {
    switch(arg1,
           argument = pop_command("ARG", arg2),
           local = pop_command("LCL", arg2),
           this = pop_command("THIS", arg2),
           that = pop_command("THAT", arg2),
           temp = pop_address_command(R_TEMP0 + arg2),
           pointer = pop_address_command(R_THIS + arg2),
           static = pop_address_command(static_symbol(scope, arg2)),
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
  neg = function(...) c("@SP",
                        "A=M-1",
                        "M=-M"),
  eq = function(..., gen_label) compare_command("JEQ", gen_label()),
  gt = function(..., gen_label) compare_command("JGT", gen_label()),
  lt = function(..., gen_label) compare_command("JLT", gen_label()),
  and = function(...) logical_command("&"),
  or = function(...) logical_command("|"),
  not = function(...) c("@SP",
                        "A=M-1",
                        "M=!M"),
  label = function(arg1, ...) sym(arg1),
  goto = function(arg1, ...) c(at(arg1),
                               "0;JMP"),
  `if-goto` = function(arg1, ...) if_goto_command(arg1),
  `function` = function(arg1, arg2, ...) function_command(arg1, arg2),
  `return` = function(...) return_command(),
  call = function(arg1, arg2, gen_label, ...) call_command(arg1, arg2, gen_label))

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

logical_command <- function(op) {
  c("@SP",
    "A=M-1",
    "D=M",
    "A=A-1",
    str_c("M=D", op, "M"),
    "@SP",
    "M=M-1")
}

push_command <- function(base, index) {
  c(at(index),
    "D=A",
    at(base),
    "A=D+M",
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1")
}

push_label_command <- function(label) {
  c(at(label),
    "D=A",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1")
}

push_address_command <- function(address) {
  c(at(address),
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1")
}

pop_command <- function(base, index) {
  c(at(index),
    "D=A",
    at(base),
    "D=D+M",
    "@R13",
    "M=D",
    "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    "@R13",
    "A=M",
    "M=D")
}

pop_address_command <- function(address) {
  c("@SP",
    "M=M-1",
    "A=M",
    "D=M",
    at(address),
    "M=D")
}

if_goto_command <- function(label) {
  c("@SP",
    "M=M-1",
    "A=M",
    "D=M",
    at(label),
    "D;JNE")
}

function_command <- function(label, nlocal) {
  # TODO: initialize locals
  sym(label)
}

return_command <- function() {
  c("@LCL",
    "D=M",
    "@R14", # FRAME
    "M=D",
    load_to_d("R14", -5),
    "@R15", # RET
    "M=D",  # RET = *(FRAME-5)
    pop_command("ARG", 0),
    "@ARG",
    "D=M+1",
    "@SP",
    "M=D", # SP = ARG+1
    load_to_d("R14", -1),
    "@THAT",
    "M=D", # THAT = *(FRAME-1)
    load_to_d("R14", -2),
    "@THIS",
    "M=D", # THIS = *(FRAME-2)
    load_to_d("R14", -3),
    "@ARG",
    "M=D", # ARG = *(FRAME-3)
    load_to_d("R14", -4),
    "@LCL",
    "M=D", # LCL = *(FRAME-4)
    "@R15",
    "A=M",
    "0;JMP")
}

call_command <- function(f, n, gen_label) {
  return_address <- gen_label()
  c(push_label_command(return_address),
    push_address_command("LCL"),
    push_address_command("ARG"),
    push_address_command("THIS"),
    push_address_command("THAT"),
    "@SP",
    "D=M",
    at(n + 5),
    "D=D-A",
    "@ARG",
    "M=D", # ARG = SP-n-5
    "@SP",
    "D=M",
    "@LCL",
    "M=D", # LCL = SP
    at(f),
    "0;JMP", # goto f
    sym(return_address))
}

load_to_d <- function(base, index) {
  if (index > 0) stop()
  c(at(base),
    "D=M",
    at(abs(index)),
    "D=D-A",
    "A=D",
    "D=M")
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

static_symbol <- function(namespace, index) {
  if (is.null(namespace)) namespace <- NAMESPACE_GLOBAL
  str_c(namespace, ".", index)
}

sym <- function(label) str_c("(", label, ")")

command <- function(name, arg1 = NULL, arg2 = NULL, scope = NULL) {
  structure(list(name = name, arg1 = arg1, arg2 = arg2, scope = scope), class = "command")
}
