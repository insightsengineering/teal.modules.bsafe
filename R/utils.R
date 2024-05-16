`%||%` <- function(a, b) {
  if (!is.null(a) && !is.na(a) && length(a) > 0) {
    return(a)
  } else {
    return(b)
  }
}
