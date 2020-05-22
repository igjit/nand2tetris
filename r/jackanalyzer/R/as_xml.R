ELEMENT_NAMES <- c(keyword_token = "keyword",
                   symbol_token = "symbol",
                   int_const_token = "integerConstant",
                   string_const_token = "stringConstant",
                   identifier_token = "identifier")

#' @import xml2
as_xml <- function(tokens) {
  elm_names <- map_chr(tokens, element_name)
  values <- map(tokens, ~ list(str_c(" ", .[[1]], " ")))
  names(values) <- elm_names
  list(tokens = values) %>%
    as_xml_document
}

element_name <- function(token) unname(ELEMENT_NAMES[class(token)[1]])
