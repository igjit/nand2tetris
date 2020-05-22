ELEMENT_NAMES <- c(keyword_token = "keyword",
                   symbol_token = "symbol",
                   int_const_token = "integerConstant",
                   string_const_token = "stringConstant",
                   identifier_token = "identifier")

#' @importFrom methods is
as_xml <- function(x) {
  if (is(x[[1]], "token")) {
    tokens_to_xml(x)
  } else if (is(x, "node")) {
    node_to_xml(x)
  } else {
    stop()
  }
}

#' @import xml2
tokens_to_xml <- function(tokens) {
  elm_names <- map_chr(tokens, element_name)
  values <- map(tokens, ~ list(str_c(" ", .[[1]], " ")))
  names(values) <- elm_names
  list(tokens = values) %>%
    as_xml_document
}

node_to_xml <- function(node) {
  node_to_list(node) %>%
    list %>%
    set_names(node$name) %>%
    as_xml_document
}

node_to_list <- function(node) {
  elm_names <- node$elements %>%
    map_chr(~ if (is(., "node")) .$name else element_name(.))
  node$elements %>%
    map(~ if (is(., "node")) node_to_list(.)
          else list(str_c(" ", .[[1]][[1]], " "))) %>%
    set_names(elm_names)
}

element_name <- function(token) unname(ELEMENT_NAMES[class(token)[1]])
