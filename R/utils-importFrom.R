#' .data object from dplyr
#'
#' @name .data
#' @rdname dplyr_.data
#' @keywords internal
#' @importFrom dplyr .data
NULL

#' .. function from shinymeta
#'
#' @name ..
#' @rdname dot_dot_shinymeta
#' @keywords internal
#' @importFrom shinymeta ..
NULL

# This prevents a warning in check.
# It is included because otherwise the test-bsafe fails when running inside check
# It is a known shiny issue that the following error may appear when including markdowns
# Error in packageVersion: there is no package called ‘markdown’
invisible(markdown::mark())
