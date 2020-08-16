#' Identifies outliers in a numeric vector
#' @description
#' FUNOP stands for FUll NOrmal Plot.
#'
#' The procedure identifies outliers by calculating their slope (\code{z}),
#' relative to the vector's median.
#'
#' The procedure ignores values in the middle third of the \emph{ordered}
#' vector. The remaining values are all candidates for consideration. The
#' slopes of all candidates are calculated, and the median of their slopes
#' is used as the primary basis for identifying outliers.
#'
#' Values whose slope is \code{B} times larger than the median slope are
#' identified as outliers. The procedure also identifies as outliers any
#' values whose magnitude is larger than the identified outliers.
#'
#' However, the procedure will \emph{not} disregard as outliers any values
#' within \code{A} standard deviations of the vector's median (i.e., not
#' the median of candidate slopes).
#' @param x
#' Numeric vector to inspect for outliers (does not need to be ordered)
#' @param A
#' Number of standard deviations beyond the median of \code{x}
#' @param B
#' Multiples beyond the median slope of candidate values
#' @return
#' The indices of outliers in \code{x}
#' @seealso [vacuum::a_qnorm()]
#' @references
#' Tukey, John W. "The Future of Data Analysis."
#' \emph{The Annals of Mathematical Statistics},
#' \emph{33}(1), 1962, pp 1-67. \emph{JSTOR},
#' \url{http://www.jstor.org/stable/2237638}.
#' @examples
#' # funop returns the index of outliers
#' funop(c(1, 2, 3, 11))
#' funop(table_1)
#'
#' # to return the value of outliers, use the function's
#' # output as the index to the original vector
#' table_1[funop(table_1)]
#' @export

funop <- function(x, A = 0, B = 1.5) {

  if (!is.vector(x) | !is.numeric(x)) {
    warning('argument "x" must be a numeric vector')
  } else if (length(A) != 1 | !is.numeric(A)) {
    warning('argument "A" must be a single numeric value')
  } else if (length(B) != 1 | !is.numeric(B)) {
    warning('argument "B" must be a single numeric value')
  } else if (anyNA(c(A, B))) {
    warning('arguments "A" and "B" must be single numeric values')
  } else {
    # (b1)
    # Let a_i|n be a typical value for the ith ordered observation in
    # a sample of n from a unit normal distribution.
    order_map <- order(as.vector(x))
    ordered <- sort(x)
    n <- length(x)

    # (b2)
    # Let y_1 ≤ y_2 ≤ … ≤ y_n be the ordered values to be examined.
    # Let y_split be their median (or let y_trimmed be the mean of the y_i
    # with  (1/3)n < i ≤ (2/3)n).
    middle_third <- (floor(n / 3) + 1):ceiling(2 * n / 3)
    outer_thirds <- (1:n)[-middle_third]
    y_split <- median(ordered)
    y_trimmed <- mean(ordered[middle_third])

    # (b3)
    # For i ≤ (1/3)n or > (2/3)n only, let z_i = (y_i – y_split) / a_i|n
    # (or let z_i = (y_i – y_trimmed) / a_i|n).
    z <- (ordered[outer_thirds] - y_split) / a_qnorm(outer_thirds, n)

    # (b4)
    # Let z_split be the median of the z’s thus obtained.
    z_split <- median(z)

    # (b5)
    # Give special attention to z’s for which both
    # |y_i – y_split| ≥ A · z_split and z_i ≥ B · z_split where A and B are
    # prechosen.
    # This is the first half (z_i ≥ B · z_split)
    extreme_B <- B * z_split
    extreme_index <- outer_thirds[which(z >= extreme_B)]
    extreme_values <- ordered[extreme_index]

    # (b5*)
    # Particularly for small n, z_j’s with j more extreme than an i
    # for which (b5) selects z_i also deserve special attention.
    extreme_index_high <-
      extreme_index[extreme_index %in% outer_thirds[outer_thirds > max(middle_third)]]

    extreme_index_low <-
      extreme_index[extreme_index %in% outer_thirds[outer_thirds < min(middle_third)]]

    if (length(extreme_index_high) > 0) {
      extreme_index_high <- min(extreme_index_high)
      B_index <- extreme_index_high:n
    } else {
      B_index <- NULL
    }

    if (length(extreme_index_low) > 0) {
      extreme_index_low <- max(extreme_index_low)
      B_index <- c(1:extreme_index_low, B_index)
    }

    # second half of 5a: values outside (y_split ± (A * z_split))
    extreme_A <- A * z_split
    A_index <- which(abs(ordered - y_split) >= extreme_A)

    combined_index <- B_index[which(B_index %in% A_index)]

    order_map[combined_index]

  }

}
