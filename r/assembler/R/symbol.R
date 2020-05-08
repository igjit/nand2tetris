DEFINED_SYMBOL <- c(
  SP = 0,
  LCL = 1,
  ARG = 2,
  THIS = 3,
  THAT = 4,
  set_names(0:15, paste0("R", 0:15)),
  SCREEN = 16384,
  KBD = 24576)

address_generator <- function() {
  adr <- 16
  sym_table <- list()
  function(symbol) {
    if (is.null(sym_table[[symbol]])) {
      sym_table[[symbol]] <<- adr
      adr <<- adr + 1
    }
    sym_table[[symbol]]
  }
}
