#' @import dplyr
#' @import purrr
class_symbol_table <- function(class_node) {
  var_df <- class_node$elements %>%
    keep(~ is(., "class_var_dec_node")) %>%
    map_dfr(var_dec_to_df)
  if (nrow(var_df) == 0) {
    var_df
  } else {
    var_df %>%
      group_by(kind) %>%
      mutate(index = row_number() - 1) %>%
      ungroup()
  }
}

method_symbol_table <- function(method_node, class_name) {
  class_table <- tibble(name = "this", type = class_name, kind = "argument")
  rbind(class_table, param_var_table(method_node)) %>%
    group_by(kind) %>%
    mutate(index = row_number() - 1) %>%
    ungroup()
}

function_symbol_table <- function(function_node) {
  param_var_table(function_node) %>%
    group_by(kind) %>%
    mutate(index = row_number() - 1) %>%
    ungroup()
}

param_var_table <- function(node) {
  param_table <- node$elements %>%
    detect(~ is(., "parameter_list_node")) %>%
    parameter_list_to_df()

  var_table <- node$elements %>%
    detect(~ is(., "subroutine_body_node")) %>%
    pluck("elements") %>%
    keep(~ is(., "var_dec_node")) %>%
    map_dfr(var_dec_to_df)

  rbind(param_table, var_table)
}

var_dec_to_df <- function(node) {
  kind <- node$elements[[1]]$keyword
  type <- node$elements[[2]][[1]]
  sym_names <- node$elements %>%
    tail(-2) %>%
    keep(~ is(., "identifier_token")) %>%
    map_chr("identifier")
  sym_names %>%
    map_dfr(~ list(name = ., type = type, kind = kind))
}

parameter_list_to_df <- function(node) {
  node$elements %>%
    discard(~ is_token_of(., ",")) %>%
    map_chr(~ .[[1]]) %>%
    matrix(ncol = 2, byrow = TRUE, dimnames = list(NULL, c("type", "name"))) %>%
    as_tibble() %>%
    select(name, type) %>%
    mutate(kind = "argument")
}
