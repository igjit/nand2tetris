parse <- function(tokens) {
  parse_class(tokens, new_state())
}

parse_class <- function(tokens, state) {
  if (!identical(tokens[[state$i]], keyword_token("class"))) stop()
  if (!is(tokens[[state$i + 1]], "identifier_token")) stop()
  if (!identical(tokens[[state$i + 2]], symbol_token("{"))) stop()
  elements <- tokens[(state$i):(state$i + 2)]
  inc(state, 3)

  if (!identical(tokens[[state$i]], symbol_token("}"))) stop()
  elements <- c(elements, list(tokens[[state$i]]))
  inc(state)

  class_node(elements)
}

new_state <- function() as.environment(list(i = 1))

inc <- function(state, n = 1) state$i <- state$i + n

class_node <- function(elements) {
  structure(list(name = "class", elements = elements), class = c("class_node", "node"))
}
