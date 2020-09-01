#' Identifies outliers in a numeric vector
#' @description
#' FUNOP stands for FUll NOrmal Plot.
#'
#' The procedure identifies outliers by calculating their slope (\code{z}),
#' relative to the vector's median.
#'
#' The procedure ignores values in the middle third of the \emph{ordered}
#' vector. The remaining values are all candidates for consideration. The
#' slopes of candidate values are calculated, and the median of their slopes
#' is used as the primary basis for identifying outliers.
#'
#' Any value whose slope is \code{B} times larger than the median slope is
#' identified as an outlier. Additionally, any value whose \emph{magnitude}
#' is larger than that of the slope-based outliers is also identified as
#' an outlier.
#'
#' However, the procedure will \emph{not} identify as outliers any values
#' within \code{A} standard deviations of the vector's median (i.e., not
#' the median of candidate slopes).
#' @param x
#' Numeric vector to inspect for outliers (does not need to be ordered)
#' @param A
#' Number of standard deviations beyond the median of \code{x}
#' @param B
#' Multiples beyond the median slope of candidate values
#' @return
#' A data frame containing one row for every member of \code{x} (in the same
#' order as \code{x}) and the
#' following columns:
#' * \code{y}: Original values of vector \code{x}
#' * \code{i}: Ordinal position of value \code{y} in the sorted vector \code{x}
#' * \code{middle}: Boolean indicating whether ordinal position \code{i} is in the middle third of the vector
#' * \code{a}: Result of \code{a_qnorm(i, length(x))}
#' * \code{z}: Slope of \code{y} relative to \code{median(y)}
#' * \code{special}: Boolean indicating whether \code{y} is an outlier
#' Additionally, the data frame will have the following attributes,
#' which FUNOP calculates as part of its procedure:
#' * \code{y_split}: Median of the vector
#' * \code{y_trimmed}: Mean of the top and bottom thirds of the vector
#' * \code{z_split}: Median slope of candidate values
#' @seealso [vacuum::a_qnorm()]
#' @references
#' Tukey, John W. "The Future of Data Analysis."
#' \emph{The Annals of Mathematical Statistics},
#' \emph{33}(1), 1962, pp 1-67. \emph{JSTOR},
#' \url{https://www.jstor.org/stable/2237638}.
#' @examples
#' funop(c(1, 2, 3, 11))
#' funop(table_1)
#'
#' attr(funop(table_1), 'z_split')
#' @export

funop <- function(x, A = 0, B = 1.5) {
  special <- orig_order <- y <- i <- middle <- a <- z <- NULL

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
    # Let a_{i|n} be a typical value for the ith ordered observation in
    # a sample of n from a unit normal distribution.
    n <- length(x)

    # initialze dataframe to hold results
    result <- data.frame(
      y = x,
      orig_order = 1:n,
      a = NA,
      z = NA,
      middle = FALSE,
      special = FALSE
    )

    # put array in order
    result <- result %>%
      dplyr::arrange(x) %>%
      dplyr::mutate(i = dplyr::row_number())

    # calculate a_{i|n}
    result$a <- a_qnorm(result$i, n)

    # (b2)
    # Let y_1 ≤ y_2 ≤ … ≤ y_n be the ordered values to be examined.
    # Let y_split be their median (or let y_trimmed be the mean of the y_i
    # with  (1/3)n < i ≤ (2/3)n).
    middle_third <- (floor(n / 3) + 1):ceiling(2 * n / 3)
    outer_thirds <- (1:n)[-middle_third]

    result$middle[middle_third] <- TRUE
    y_split <- stats::median(result$y)
    y_trimmed <- mean(result$y[middle_third])

    # (b3)
    # For i ≤ (1/3)n or > (2/3)n only,
    # let z_i = (y_i – y_split) / a_{i|n}
    # (or let z_i = (y_i – y_trimmed) / a_{i|n}).
    result$z[outer_thirds] <-
      (result$y[outer_thirds] - y_split) / result$a[outer_thirds]

    # (b4)
    # Let z_split be the median of the z’s thus obtained.
    z_split <- stats::median(result$z[outer_thirds])

    # (b5)
    # Give special attention to z’s for which both
    # |y_i – y_split| ≥ A · z_split and z_i ≥ B · z_split
    # where A and B are prechosen.
    result$special <-
      ifelse(result$z >= (B * z_split) &
               abs(result$y - y_split) >= (A * z_split), TRUE, FALSE)

    # (b5*)
    # Particularly for small n, z_j’s with j more extreme than an i
    # for which (b5) selects z_i also deserve special attention.

    # in the top third, look for values larger than ones already found
    top_third <- outer_thirds[outer_thirds > max(middle_third)]

    # take advantage of the fact that we've already indexed our result set
    # and simply look for values of i larger than the smallest i in the
    # top third (further to the right of our x-axis)
    if (any(result$special[top_third])) {
      min_i <- result[top_third, ] %>%
        dplyr::filter(special == TRUE)
      min_i <- min(min_i$i)

      result$special[which(result$i > min_i)] <- TRUE
    }

    # in the top third, look for values smaller than ones already found
    bottom_third <- outer_thirds[outer_thirds < min(middle_third)]

    # look for values of i smaller than the largest i in the bottom third
    # (further to the left of our x-axis)
    if (any(result$special[bottom_third])) {
      max_i <- result[bottom_third, ] %>%
        dplyr::filter(special == TRUE)
      max_i <- max(max_i$i)


      result$special[which(result$i < max_i)] <- TRUE
    }

    result <- result %>%
      dplyr::arrange(orig_order) %>%
      dplyr::select(y, i, middle, a, z, special)

    attr(result, 'y_split') <- y_split
    attr(result, 'y_trimmed') <- y_trimmed
    attr(result, 'z_split') <- z_split

    result
  }
}
