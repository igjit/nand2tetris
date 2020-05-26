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
  ftable <- function_symbol_table(node)
  lookup <- new_lookup(ftable)
  n_locals <- ftable %>% filter(kind == "var") %>% nrow
  c(paste("function", str_c(class_name, ".", fname), n_locals),
    map(statements$elements, compile_statement, lookup) %>% flatten_chr)
}

compile_statement <- function(node, lookup) {
  fname <- str_c("compile_", pluck(node, "elements", 1, "keyword"), "_statement")
  do.call(fname, list(node, lookup))
}

compile_let_statement <- function(node, lookup) {
  name <- node$elements[[2]]$identifier
  expression <- node$elements[[length(node$elements) - 1]]
  index <- lookup(name)$index
  c(compile_expression(expression, lookup),
    paste("pop local", index))
}

compile_do_statement <- function(node, lookup) {
  # drop 'do'
  term_node <- list(elements = node$elements[-1])

  c(compile_subroutine_call(term_node, lookup),
    "pop temp 0")
}

compile_return_statement <- function(node, lookup) {
  # TODO
  c("push constant 0",
    "return")
}

compile_expression <- function(node, lookup) {
  if (length(node$elements) == 1) {
    compile_term(node$elements[[1]], lookup)
  } else {
    c(compile_term(node$elements[[1]], lookup),
      compile_term(node$elements[[3]], lookup),
      compile_op(node$elements[[2]]))
  }
}

compile_term <- function(node, lookup) {
  if (is(node$elements[[1]], "int_const_token")) {
    val <- node$elements[[1]]$int_val
    paste("push constant", val)
  } else if (is(node$elements[[1]], "identifier_token")) {
    compile_ver_name_term(node, lookup)
  } else if (is_token_of(node$elements[[1]], "(")) {
    compile_expression(node$elements[[2]], lookup)
  } else if (is_token_of(node$elements[[1]], "-")) {
    c(compile_term(node$elements[[2]], lookup),
      "neg")
  } else {
    stop("TODO")
  }
}

compile_ver_name_term <- function(node, lookup) {
  name <- node$elements[[1]]$identifier
  index <- lookup(name)$index
  paste("push local", index)
}

compile_subroutine_call <- function(node, lookup) {
  if (!is_token_of(node$elements[[2]], ".")) stop("TODO")
  fname <- node$elements[1:3] %>%
    map_chr(1) %>%
    str_c(collapse = "")
  expressions <- find(node, "expressionList") %>%
    pluck("elements") %>%
    discard(~ is_token_of(., ","))

  c(map(expressions, compile_expression, lookup) %>% flatten_chr,
    paste("call", fname, length(expressions)))
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

new_lookup <- function(table) {
  function(name) filter(table, name == !!name)
}
