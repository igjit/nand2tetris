#' @import dplyr
#' @import purrr
class_symbol_table <- function(class_node) {
  class_node$elements %>%
    keep(~ is(., "class_var_dec_node")) %>%
    map_dfr(var_dec_to_df) %>%
    group_by(kind) %>%
    mutate(index = row_number() - 1) %>%
    ungroup()
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
