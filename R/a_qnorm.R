#' Returns the typical value from a unit-normal distribution
#' @description
#' Returns the typical value from a unit-normal distribution for the
#' \emph{i}th ordered observation in an \emph{n}-sized sample.
#'
#' This is a helper function for FUNOP, which uses the output of this
#' function as the denominator for its slope calculation.
#' @param
#' i Non-zero index of an array
#' @param
#' n Non-zero length of the array
#' @return
#' Quantile of \code{i} from a unit-normal distribution
#' @seealso [vacuum::funop()]
#' @references
#' The formula was described by John Tukey in his paper "The
#' Future of Data Analytics".
#' See section 16, "Modified normal plotting" (p 22),
#' available at \url{https://www.jstor.org/stable/2237638}.
#' @examples
#' a_qnorm(i = 25, n = 42)
#' a_qnorm(21.5, 42)
#' @export

a_qnorm <- function(i, n) {
  if (is.numeric(i) & is.numeric(n)) {
    qnorm((3 * i - 1) / (3 * n + 1))
  } else {
    warning('Both parameters must be numeric.')
  }

}
