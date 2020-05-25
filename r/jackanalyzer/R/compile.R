compile <- function(node) {
  if (!is(node, "class_node")) stop()
  class_name <- node$elements[[2]]$identifier
  subroutines <- node$elements %>% keep(~ is(., "subroutine_dec_node"))
  subroutines %>%
    map(compile_function, class_name) %>%
    flatten_chr
}

compile_function <- function(node, class_name) {
  fname <- node$elements[[3]]$identifier
  statements <- find(node, "statements")
  # TODO
  n_locals <- 0
  c(paste("function", str_c(class_name, ".", fname), n_locals),
    map(statements$elements, compile_statement) %>% flatten_chr)
}

compile_statement <- function(node) {
  fname <- str_c("compile_", pluck(node, "elements", 1, "keyword"), "_statement")
  do.call(fname, list(node))
}

compile_do_statement <- function(node) {
  if (!is_token_of(node$elements[[3]], ".")) stop("TODO")
  fname <- node$elements[2:4] %>%
    map_chr(1) %>%
    str_c(collapse = "")
  expressions <- find(node, "expressionList") %>%
    pluck("elements") %>%
    discard(~ is_token_of(., ","))

  c(map(expressions, compile_expression) %>% flatten_chr,
    paste("call", fname, length(expressions)),
    "pop temp 0")
}

compile_return_statement <- function(node) {
  # TODO
  c("push constant 0",
    "return")
}

compile_expression <- function(node) {
  if (length(node$elements) == 1) {
    compile_term(node$elements[[1]])
  } else {
    c(compile_term(node$elements[[1]]),
      compile_term(node$elements[[3]]),
      compile_op(node$elements[[2]]))
  }
}

compile_term <- function(node) {
  if (is(node$elements[[1]], "int_const_token")) {
    val <- node$elements[[1]]$int_val
    paste("push constant", val)
  } else {
    stop("TODO")
  }
}

compile_op <- function(token) {
  switch(token$symbol,
         "+" = "add",
         "-" = "sub",
         "*" = "call Math.multiply 2",
         "/" = "call Math.divide 2")
}

find <- function(node, name) {
  found <- node$elements %>% detect(~ .[[1]] == name)
  if (!is.null(found)) {
    found
  } else {
    child <- NULL
    node$elements %>% detect(~ !is.null(child <<- find(., name)))
    child
  }
}
