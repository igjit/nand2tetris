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
  counter <- new_counter()
  n_locals <- ftable %>% filter(kind == "var") %>% nrow
  c(paste("function", str_c(class_name, ".", fname), n_locals),
    compile_statements(statements, lookup, counter))
}

compile_statements <- function(node, lookup, counter) {
  map(node$elements, compile_statement, lookup, counter) %>%
    flatten_chr
}

compile_statement <- function(node, lookup, counter) {
  keyword <- pluck(node, "elements", 1, "keyword")
  fname <- str_c("compile_", keyword, "_statement")
  if (keyword %in% c("if", "while")) {
    do.call(fname, list(node, lookup, counter))
  } else {
    do.call(fname, list(node, lookup))
  }
}

compile_let_statement <- function(node, lookup) {
  name <- node$elements[[2]]$identifier
  expression <- node$elements[[length(node$elements) - 1]]
  index <- lookup(name)$index
  segment <- segment_of(lookup(name)$kind)
  if (is_token_of(node$elements[[3]], "[")) {
    c(compile_expression(node$elements[[4]], lookup),
      paste("push", segment, index),
      "add",
      compile_expression(expression, lookup),
      "pop temp 0",
      "pop pointer 1",
      "push temp 0",
      "pop that 0")
  } else {
    c(compile_expression(expression, lookup),
      paste("pop", segment, index))
  }
}

compile_if_statement <- function(node, lookup, counter) {
  index <- counter("if")
  has_else <- length(node$elements) > 10 && is_token_of(node$elements[[8]], "else")
  c(compile_expression(node$elements[[3]], lookup),
    str_c("if-goto IF_TRUE", index),
    str_c("goto IF_FALSE", index),
    str_c("label IF_TRUE", index),
    compile_statements(node$elements[[6]], lookup, counter),
    if (has_else) str_c("goto IF_END", index),
    str_c("label IF_FALSE", index),
    if (has_else) compile_statements(node$elements[[10]], lookup, counter),
    if (has_else) str_c("label IF_END", index))
}

compile_while_statement <- function(node, lookup, counter) {
  index <- counter("while")
  c(str_c("label WHILE_EXP", index),
    compile_expression(node$elements[[3]], lookup),
    "not",
    str_c("if-goto WHILE_END", index),
    compile_statements(node$elements[[6]], lookup, counter),
    str_c("goto WHILE_EXP", index),
    str_c("label WHILE_END", index))
}

compile_do_statement <- function(node, lookup) {
  # drop 'do'
  term_node <- list(elements = node$elements[-1])

  c(compile_subroutine_call(term_node, lookup),
    "pop temp 0")
}

compile_return_statement <- function(node, lookup) {
  if (is(node$elements[[2]], "expression_node")) {
    c(compile_expression(node$elements[[2]], lookup),
      "return")
  } else {
    c("push constant 0",
      "return")
  }
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
  } else if (is(node$elements[[1]], "string_const_token")) {
    compile_string_constant(node$elements[[1]])
  } else if (is_keyword_constant(node$elements[[1]])) {
    compile_keyword_constant(node$elements[[1]])
  } else if (is(node$elements[[1]], "identifier_token")) {
    if (length(node$elements) == 1) {
      compile_ver_name_term(node, lookup)
    } else if (is_token_of(node$elements[[2]], "[")) {
      compile_array_access_term(node, lookup)
    } else {
      compile_subroutine_call(node, lookup)
    }
  } else if (is_token_of(node$elements[[1]], "(")) {
    compile_expression(node$elements[[2]], lookup)
  } else if (is_unary_op(node$elements[[1]])) {
    c(compile_term(node$elements[[2]], lookup),
      compile_unary_op(node$elements[[1]]))
  } else {
    stop("TODO")
  }
}

compile_string_constant <- function(node) {
  val <- node$string_val
  c(paste("push constant", str_length(val)),
    "call String.new 1",
    utf8ToInt(val) %>%
    map(~ c(paste("push constant", .), "call String.appendChar 2")) %>%
    flatten_chr)
}

compile_ver_name_term <- function(node, lookup) {
  name <- node$elements[[1]]$identifier
  index <- lookup(name)$index
  segment <- segment_of(lookup(name)$kind)
  paste("push", segment, index)
}

compile_array_access_term <- function(node, lookup) {
  name <- node$elements[[1]]$identifier
  index <- lookup(name)$index
  segment <- segment_of(lookup(name)$kind)
  c(compile_expression(node$elements[[3]], lookup),
    paste("push", segment, index),
    "add",
    "pop pointer 1",
    "push that 0")
}

compile_subroutine_call <- function(node, lookup) {
  if (!is_token_of(node$elements[[2]], ".")) stop("TODO")
  name <- node$elements[[1]]$identifier
  class_name <- lookup(name)$type
  is_method_call <- length(class_name) > 0
  receiver <- if (is_method_call) class_name else name
  fname <- str_c(receiver, ".", node$elements[[3]]$identifier)
  expressions <- find(node, "expressionList") %>%
    pluck("elements") %>%
    discard(~ is_token_of(., ","))
  n_args <- length(expressions) + if (is_method_call) 1 else 0

  c(if (is_method_call) compile_term(list(elements = node$elements[1]), lookup),
    map(expressions, compile_expression, lookup) %>% flatten_chr,
    paste("call", fname, n_args))
}

compile_keyword_constant <- function(token) {
  switch(token$keyword,
         "true" = c("push constant 0",
                    "not"),
         "false" = "push constant 0",
         "null" = "push constant 0")
}

compile_op <- function(token) {
  switch(token$symbol,
         "+" = "add",
         "-" = "sub",
         "*" = "call Math.multiply 2",
         "/" = "call Math.divide 2",
         "&" = "and",
         "|" = "or",
         "<" = "lt",
         ">" = "gt",
         "=" = "eq")
}

compile_unary_op <- function(token) {
  switch(token$symbol,
         "-" = "neg",
         "~" = "not")
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

new_counter <- function() {
  index <- list()
  function(name) {
    if (is.null(index[[name]])) {
      (index[[name]] <<- 0)
    } else {
      (index[[name]] <<- index[[name]] + 1)
    }
  }
}

segment_of <- function(kind) {
  if (kind == "argument") "argument" else "local"
}
