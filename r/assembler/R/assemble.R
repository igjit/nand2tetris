to_hack <- function(commands) {
  symbol_table <- generate_symbol_table(commands)
  resolve_symbols(commands, symbol_table) %>%
    map_chr(assemble)
}

#' @importFrom methods is
generate_symbol_table <- function(commands) {
  symbol_table <- c()
  pc <- 0
  commands %>% walk(function(cmd) {
    if (is(cmd, "l_command")) {
      symbol_table[cmd$symbol] <<- pc
    } else {
      pc <<- pc + 1
    }
  })
  symbol_table
}

resolve_symbols <- function(commands, symbol_table) {
  gen_address <- address_generator()
  commands %>%
    discard(~ is(., "l_command")) %>%
    map(~ resolve_symbol(., symbol_table, gen_address))
}

resolve_symbol <- function(command, symbol_table, gen_address) {
  if (!is(command, "a_command") || !is.na(command$int)) {
    command
  } else {
    sym <- command$symbol
    adr <- if (!is.na(DEFINED_SYMBOL[sym])) {
      DEFINED_SYMBOL[sym]
    } else if (!is.na(symbol_table[sym])) {
      symbol_table[sym]
    } else {
      gen_address(sym)
    }
    command$int <- adr
    command
  }
}

assemble <- function(x) UseMethod("assemble")

assemble.a_command <- function(x) {
  str_c("0", as_bin(x$int, 15), collapse = "")
}

assemble.c_command <- function(x) {
  str_c("111",
        assemble_c_comp(x$comp),
        assemble_c_dest(x$dest),
        assemble_c_jump(x$jump),
        collapse = "")
}

assemble_c_comp <- function(comp) {
  switch(comp,
         "0" = "0101010",
         "1" = "0111111",
         "-1" = "0111010",
         "D" = "0001100",
         "A" = "0110000",
         "!D" = "0001101",
         "!A" = "0110001",
         "-D" = "0001111",
         "-A" = "0110011",
         "D+1" = "0011111",
         "A+1" = "0110111",
         "D-1" = "0001110",
         "A-1" = "0110010",
         "D+A" = "0000010",
         "D-A" = "0010011",
         "A-D" = "0000111",
         "D&A" = "0000000",
         "D|A" = "0010101",
         "M" = "1110000",
         "!M" = "1110001",
         "-M" = "1110011",
         "M+1" = "1110111",
         "M-1" = "1110010",
         "D+M" = "1000010",
         "D-M" = "1010011",
         "M-D" = "1000111",
         "D&M" = "1000000",
         "D|M" = "1010101",
         stop("Not implemented: ", comp))
}

assemble_c_dest <- function(dest) {
  if (is.null(dest)) dest <- ""
  contains <- c("A", "D", "M") %>%
    map_chr(~ str_match(dest, .)[1, 1])
  ifelse(is.na(contains), "0", "1") %>%
    str_c(collapse = "")
}

assemble_c_jump <- function(jump) {
  if (is.null(jump)) return("000")
  which(c("JGT", "JEQ", "JGE", "JLT", "JNE", "JLE", "JMP") == jump) %>%
    as_bin(3)
}

as_bin <- function(int, digit) {
  intToBits(int) %>%
    head(digit) %>%
    rev %>%
    as.integer %>%
    str_c(collapse = "")
}

a_command <- function(int = NA, symbol = NULL) {
  structure(list(int = as.numeric(int), symbol = symbol), class = "a_command")
}

c_command <- function(comp, dest = NULL, jump = NULL) {
  structure(list(comp = comp, dest = dest, jump = jump), class = "c_command")
}

l_command <- function(symbol) {
  structure(list(symbol = symbol), class = "l_command")
}
