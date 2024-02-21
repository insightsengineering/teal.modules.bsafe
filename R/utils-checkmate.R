#' @keywords internal
paste_ctxt_factory <- function(context) {
  function(var_name) paste0("(", context, "): ", checkmate::vname(var_name))
}
