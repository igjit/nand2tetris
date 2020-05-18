KEYWORDS <- c("class", "constructor", "function", "method", "field", "static",
              "var", "int", "char", "boolean", "void", "true", "false",
              "null", "this", "let", "do", "if", "else", "while", "return")

keyword_regexp <- str_c("(", str_c(KEYWORDS, collapse = "|"), ")")

#' @import purrr
#' @import stringr
tokenize <- function(s) {
  tokens <- list()
  i <- 1
  while(str_length(s) >= i) {
    chr <- str_sub(s, i, i)
    subst <- str_sub(s, i)
    if (str_detect(chr, "^\\s")) {
      # skip
      i <- i + 1
    } else if (!is.na(val <- matched(subst, str_c("^", keyword_regexp, "(\\W|$)")))) {
      tokens <- c(tokens, list(keyword_token(val)))
      i <- i + str_length(val)
    } else if (str_detect(chr, "[{}()\\[\\].,;+\\-*/&|<>=~]")) {
      tokens <- c(tokens, list(symbol_token(chr)))
      i <- i + 1
    } else if (!is.na(val <- matched(subst, "^(\\d+)(\\W|$)"))) {
      tokens <- c(tokens, list(int_const_token(as.integer(val))))
      i <- i + str_length(val)
    } else if (!is.na(val <- matched(subst, '^"(.+?)"'))) {
      tokens <- c(tokens, list(string_const_token(val)))
      i <- i + str_length(val) + 2
    } else if (!is.na(val <- matched(subst, "^([\\w_][\\w\\d_]*)(\\W|$)"))) {
      tokens <- c(tokens, list(identifier_token(val)))
      i <- i + str_length(val)
    } else {
      stop()
    }
  }
  tokens
}

char_at <- function(s, i) str_sub(s, i, i)

matched <- function(s, pattern) str_match(s, pattern)[, 2]

keyword_token <- function(keyword) {
  structure(list(keyword = keyword), class = c("keyword_token", "token"))
}

symbol_token <- function(symbol) {
  structure(list(symbol = symbol), class = c("symbol_token", "token"))
}

int_const_token <- function(int_val) {
  structure(list(int_val = int_val), class = c("int_const_token", "token"))
}

string_const_token <- function(string_val) {
  structure(list(string_val = string_val), class = c("string_const_token", "token"))
}

identifier_token <- function(identifier) {
  structure(list(identifier = identifier), class = c("identifier_token", "token"))
}
