parse <- function(tokens) {
  parse_class(tokens, new_state())
}

parse_class <- function(tokens, state) {
  if (!is_token_of(tokens[[state$i]], "class")) stop()
  if (!is(tokens[[state$i + 1]], "identifier_token")) stop()
  if (!is_token_of(tokens[[state$i + 2]], "{")) stop()
  elements <- tokens[(state$i):(state$i + 2)]
  inc(state, 3)

  # classVarDec*
  while (is_keyword_in(tokens[[state$i]], c("static", "field"))) {
    elements <- c(elements, list(parse_class_var_dec(tokens, state)))
  }

  if (!is_token_of(tokens[[state$i]], "}")) stop()
  elements <- c(elements, list(tokens[[state$i]]))
  inc(state)

  class_node(elements)
}

new_state <- function() as.environment(list(i = 1))

inc <- function(state, n = 1) state$i <- state$i + n

pop_token_of <- function(tokens, state, val) {
  if (!is_token_of(tokens[[state$i]], val)) stop()
  token <- tokens[[state$i]]
  inc(state)
  token
}

parse_class_var_dec <- function(tokens, state) {
  from <- state$i
  if (!is_type(tokens[[state$i + 1]])) stop()
  inc(state, 2) # ('static' | 'field') type

  while(!is_token_of(tokens[[state$i]], ";")) {
    inc(state) # varName
    if (is_token_of(tokens[[state$i]], ",")) {
      inc(state) # ','
    }
  }
  to <- state$i
  inc(state)
  class_var_dec_node(tokens[from:to])
}

parse_let_statement <- function(tokens, state) {
  if (!is_token_of(tokens[[state$i]], "let")) stop()
  # let varName
  elements <- tokens[state$i + 0:1]
  inc(state, 2)

  if (is_token_of(tokens[[state$i]], "[")) {
    elements <- c(elements,
                  list(pop_token_of(tokens, state, "["),
                       parse_expression(tokens, state),
                       pop_token_of(tokens, state, "]")))
  }
  elements <- c(elements,
                list(pop_token_of(tokens, state, "="),
                     parse_expression(tokens, state),
                     pop_token_of(tokens, state, ";")))

  structure(list(name = "letStatement", elements = elements), class = c("let_statement_node", "node"))
}

parse_return_statement <- function(tokens, state) {
  elements <- list(pop_token_of(tokens, state, "return"))
  if (!is_token_of(tokens[[state$i]], ";")) {
    elements <- c(elements, list(parse_expression(tokens, state)))
  }
  elements <- c(elements, list(pop_token_of(tokens, state, ";")))

  structure(list(name = "returnStatement", elements = elements), class = c("return_statement_node", "node"))
}

parse_expression <- function(tokens, state) {
  # TODO
  if (!is(token <- tokens[[state$i]], "identifier_token")) stop("TODO")
  inc(state)
  expression_node(list(term_node(list(token))))
}

is_type <- function(token) {
  is_keyword_in(token, c("int", "char", "boolean")) || is(token, "identifier_token")
}

is_keyword_in <- function(token, keywords) {
  is(token, "keyword_token") && token$keyword %in% keywords
}

is_token_of <- function(token, val) token[[1]] == val

class_node <- function(elements) {
  structure(list(name = "class", elements = elements), class = c("class_node", "node"))
}

class_var_dec_node <- function(elements) {
  structure(list(name = "classVarDec", elements = elements), class = c("class_var_dec_node", "node"))
}

expression_node <- function(elements) {
  structure(list(name = "expression", elements = elements), class = c("expression_node", "node"))
}

term_node <- function(elements) {
  structure(list(name = "term", elements = elements), class = c("term_node", "node"))
}
