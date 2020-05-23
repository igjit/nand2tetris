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

  # subroutineDec*
  while(is_beginning_of_subroutine_dec(tokens, state)) {
    elements <- c(elements, list(parse_subroutine_dec(tokens, state)))
  }

  if (!is_token_of(tokens[[state$i]], "}")) stop()
  elements <- c(elements, list(tokens[[state$i]]))
  inc(state)

  structure(list(name = "class", elements = elements), class = c("class_node", "node"))
}

new_state <- function(i = 1) as.environment(list(i = i))

inc <- function(state, n = 1) state$i <- state$i + n

pop <- function(tokens, state) {
  token <- tokens[[state$i]]
  inc(state)
  token
}

pop_token_of <- function(tokens, state, val) {
  if (!is_token_of(tokens[[state$i]], val)) stop()
  pop(tokens, state)
}

pop_identifier_token <- function(tokens, state) {
  if (!is(tokens[[state$i]], "identifier_token")) stop()
  pop(tokens, state)
}

pop_type_token <- function(tokens, state) {
  if (!is_type(tokens[[state$i]])) stop()
  pop(tokens, state)
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

  structure(list(name = "classVarDec", elements = tokens[from:to]), class = c("class_var_dec_node", "node"))
}

parse_subroutine_dec <- function(tokens, state) {
  if (!is_beginning_of_subroutine_dec(tokens, state)) stop()
  if(!is_token_of(tokens[[state$i + 1]], "void") && !is_type(tokens[[state$i + 1]])) stop()
  elements <- list(pop(tokens, state),
                   pop(tokens, state),
                   pop_identifier_token(tokens, state),
                   pop_token_of(tokens, state, "("),
                   parse_parameter_list(tokens, state),
                   pop_token_of(tokens, state, ")"),
                   parse_subroutine_body(tokens, state))

  structure(list(name = "subroutineDec", elements = elements), class = c("subroutine_dec_node", "node"))
}

is_beginning_of_subroutine_dec <- function(tokens, state) {
  is_keyword_in(tokens[[state$i]], c("constructor", "function", "method"))
}

parse_subroutine_body <- function(tokens, state) {
  elements <- list(pop_token_of(tokens, state, "{"))
  while(is_token_of(tokens[[state$i]], "var")) {
    elements <- c(elements,
                  list(parse_var_dec(tokens, state)))
  }
  elements <- c(elements,
                list(parse_statements(tokens, state),
                     pop_token_of(tokens, state, "}")))

  structure(list(name = "subroutineBody", elements = elements), class = c("subroutine_body_node", "node"))
}

parse_var_dec <- function(tokens, state) {
  elements <- list(pop_token_of(tokens, state, "var"),
                   pop_type_token(tokens, state),
                   pop_identifier_token(tokens, state))
  while(is_token_of(tokens[[state$i]], ",")) {
    elements <- c(elements,
                  list(pop_token_of(tokens, state, ","),
                       pop_identifier_token(tokens, state)))
  }
  elements <- c(elements, list(pop_token_of(tokens, state, ";")))

  structure(list(name = "varDec", elements = elements), class = c("var_dec_node", "node"))
}

parse_parameter_list <- function(tokens, state) {
  elements <- list()
  while(!is_token_of(tokens[[state$i]], ")")) {
    elements <- c(elements,
                  list(pop_type_token(tokens, state),
                       pop_identifier_token(tokens, state)))
    if (is_token_of(tokens[[state$i]], ",")) {
      elements <- c(elements, list(pop_token_of(tokens, state, ",")))
    }
  }

  structure(list(name = "parameterList", elements = elements), class = c("parameter_list_node", "node"))
}

parse_statements <- function(tokens, state) {
  elements <- list()
  while(is_beginning_of_statement(tokens, state)) {
    elements <- c(elements, list(parse_statement(tokens, state)))
  }

  structure(list(name = "statements", elements = elements), class = c("statements_node", "node"))
}

is_beginning_of_statement <- function(tokens, state) {
  state$i <= length(tokens) &&
    is_keyword_in(tokens[[state$i]], c("let", "if", "while", "do", "return"))
}

parse_statement <- function(tokens, state) {
  fname <- str_c("parse_", tokens[[state$i]]$keyword, "_statement")
  do.call(fname, list(tokens, state))
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

parse_if_statement <- function(tokens, state) {
  elements <- list(pop_token_of(tokens, state, "if"),
                   pop_token_of(tokens, state, "("),
                   parse_expression(tokens, state),
                   pop_token_of(tokens, state, ")"),
                   pop_token_of(tokens, state, "{"),
                   parse_statements(tokens, state),
                   pop_token_of(tokens, state, "}"))
  if (state$i <= length(tokens) && is_token_of(tokens[[state$i]], "else")) {
    elements <- c(elements,
                  list(pop_token_of(tokens, state, "else"),
                       pop_token_of(tokens, state, "{"),
                       parse_statements(tokens, state),
                       pop_token_of(tokens, state, "}")))
    }

    structure(list(name = "ifStatement", elements = elements), class = c("if_statement_node", "node"))
}

parse_while_statement <- function(tokens, state) {
  elements <- list(pop_token_of(tokens, state, "while"),
                   pop_token_of(tokens, state, "("),
                   parse_expression(tokens, state),
                   pop_token_of(tokens, state, ")"),
                   pop_token_of(tokens, state, "{"),
                   parse_statements(tokens, state),
                   pop_token_of(tokens, state, "}"))

    structure(list(name = "whileStatement", elements = elements), class = c("while_statement_node", "node"))
}

parse_do_statement <- function(tokens, state) {
  elements <- c(list(pop_token_of(tokens, state, "do")),
                parse_subroutine_call(tokens, state),
                list(pop_token_of(tokens, state, ";")))

    structure(list(name = "doStatement", elements = elements), class = c("do_statement_node", "node"))
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
  elements <- list(parse_term(tokens, state))
  while(state$i <= length(tokens) && is_op(tokens[[state$i]])) {
    elements <- c(elements,
                  list(pop(tokens, state),
                       parse_term(tokens, state)))
  }

  structure(list(name = "expression", elements = elements), class = c("expression_node", "node"))
}

parse_term <- function(tokens, state) {
  elements <- if (is(tokens[[state$i]], "int_const_token") ||
                  is(tokens[[state$i]], "string_const_token") ||
                  is_keyword_constant(tokens[[state$i]])) {
                list(pop(tokens, state))
              } else if (is(tokens[[state$i]], "identifier_token")) {
                if (state$i < length(tokens) &&
                    is_token_of(tokens[[state$i + 1]], "[")) {
                  # varName '[' expression ']'
                  list(pop(tokens, state),
                       pop_token_of(tokens, state, "["),
                       parse_expression(tokens, state),
                       pop_token_of(tokens, state, "]"))
                } else if (state$i < length(tokens) &&
                           (is_token_of(tokens[[state$i + 1]], "(") ||
                            is_token_of(tokens[[state$i + 1]], "."))) {
                  # subroutineCall
                  parse_subroutine_call(tokens, state)
                } else {
                  # varName
                  list(pop(tokens, state))
                }
              } else if (is_token_of(tokens[[state$i]], "(")) {
                list(pop_token_of(tokens, state, "("),
                     parse_expression(tokens, state),
                     pop_token_of(tokens, state, ")"))
              } else if (is_unary_op(tokens[[state$i]])) {
                list(pop(tokens, state),
                     parse_term(tokens, state))
              } else {
                stop()
              }

  structure(list(name = "term", elements = elements), class = c("term_node", "node"))
}

parse_subroutine_call <- function(tokens, state) {
  elements <- list(pop_identifier_token(tokens, state))
  if(is_token_of(tokens[[state$i]], ".")) {
    elements <- c(elements,
                  list(pop_token_of(tokens, state, "."),
                       pop_identifier_token(tokens, state)))
  }
  c(elements,
    list(pop_token_of(tokens, state, "("),
         parse_expression_list(tokens, state),
         pop_token_of(tokens, state, ")")))
}

parse_expression_list <- function(tokens, state) {
  elements <- list()
  while(!is_token_of(tokens[[state$i]], ")")) {
    elements <- c(elements, list(parse_expression(tokens, state)))
    if (is_token_of(tokens[[state$i]], ",")) {
      elements <- c(elements, list(pop_token_of(tokens, state, ",")))
    }
  }

  structure(list(name = "expressionList", elements = elements), class = c("expression_list_node", "node"))
}

is_type <- function(token) {
  is_keyword_in(token, c("int", "char", "boolean")) || is(token, "identifier_token")
}

is_op <- function(token) {
  is(token, "symbol_token") &&
    token$symbol %in% c("+", "-", "*", "/", "&", "|", "<", ">", "=")
}

is_unary_op <- function(token) {
  is(token, "symbol_token") && token$symbol %in% c("-", "~")
}

is_keyword_constant <- function(token) {
  is_keyword_in(token, c("true", "false", "null", "this"))
}

is_keyword_in <- function(token, keywords) {
  is(token, "keyword_token") && token$keyword %in% keywords
}

is_token_of <- function(token, val) token[[1]] == val
