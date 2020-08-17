#' Identifies and treats outliers in a two-way table
#' @description
#' FUNOR-FUNOM stands for FUll NOrmal Rejection-FUll NOrmal Modification.
#'
#' The procedure treats a two-way (contingency) table for outliers by
#' isolating residuals from the table's likely systemic effects, which
#' are calculated from the table's grand, row, and column means.
#'
#' The residuals are passed to separate \emph{rejection} (FUNOR) and
#' \emph{modification} (FUNOM) procedures, which both depend upon FUNOP
#' to identify outliers. As such, this procedure requires two sets of
#' \code{A} and \code{B} parameters.
#'
#' The procedure treats outliers by reducing their residuals, resulting
#' in values that are much closer to their expected values (i.e.,
#' combined grand, row, and column effects).
#' @param x
#' Two-way table to treat for outliers
#' @param A_r
#' A for the FUNOR phase (see FUNOP for details)
#' @param B_r
#' B for the FUNOR phase
#' slope
#' @param A_m
#' A for the FUNOM phase (\code{A_m} is usually 0)
#' @param B_m
#' B for the FUNOM phase
#' @return
#' A two-way table of the same size as \code{x}, treated for outliers.
#' @seealso [vacuum::funop()]
#' @references
#' Tukey, John W. "The Future of Data Analysis."
#' \emph{The Annals of Mathematical Statistics},
#' \emph{33}(1), 1962, pp 1-67. \emph{JSTOR},
#' \url{http://www.jstor.org/stable/2237638}.
#' @examples
#' funor_funom(table_2)
#' which(funor_funom(table_2) != table_2)
#' @export
funor_funom <- function(x, A_r = 10, B_r = 1.5, A_m = 0, B_m = 1.5) {

  x <- as.matrix(x)

  if (!is.matrix(x) | !is.numeric(x)) {
    warning('argument "x" must be convertable to a numeric matrix')
  } else if (nrow(x) < 2 | ncol(x) < 2) {
    # change_factor will divide by zero if r = 1 or c = 1
    warning('argument "x" must have at least 2 rows and columns')
  } else if (!is.numeric(c(A_r, B_r, A_m, B_m))) {
    warning('arguments "A" and "B" must be single numeric values')
  } else if (length(c(A_r, B_r, A_m, B_m)) != 4) {
    warning('arguments "A" and "B" must be single numeric values')
  } else if (anyNA(c(A_r, B_r, A_m, B_m))) {
    warning('arguments "A" and "B" must be single numeric values')
  } else {
    # Initialize
    r <- nrow(x)
    c <- ncol(x)
    n <- r * c

    # this will be used in step a3, but only need to calc once
    change_factor <- r * c / ((r - 1) * (c - 1))

    # this data frame makes it easy to track all values
    # j and k are the rows and columns of the table
    dat <- data.frame(
      x = as.vector(x),
      j = ifelse(1:n %% r == 0, r, 1:n %% r),
      k = ceiling(1:n / r),
      change_type = 0
    )

    ###########
    ## FUNOR ##
    ###########
    #repeat {
    for (ctr in 1:n) { # don't repeat for more members than are in dataset
      dat <- dat %>%
        dplyr::select(x, j, k, change_type) # this removes the calculated values from last loop

      # (a1)
      # Fit row and column means to the original observations
      # and form the residuals
      # calculate the row means
      dat <- dat %>%
        dplyr::group_by(j) %>%
        dplyr::summarise(j_mean = mean(x)) %>%
        dplyr::ungroup() %>%
        dplyr::select(j, j_mean) %>%
        dplyr::inner_join(dat, by = 'j')

      # calculate the column means
      dat <- dat %>%
        dplyr::group_by(k) %>%
        dplyr::summarise(k_mean = mean(x)) %>%
        dplyr::ungroup() %>%
        dplyr::select(k, k_mean) %>%
        dplyr::inner_join(dat, by = 'k')

      grand_mean <- mean(dat$x)

      # calculate the residuals
      dat$y <- dat$x - dat$j_mean - dat$k_mean + grand_mean

      # put dat in order based upon y (which will match i in FUNOP)
      dat <- dat %>%
        dplyr::arrange(y) %>%
        dplyr::mutate(i = dplyr::row_number())

      # (a2)
      # apply FUNOP to the residuals
      funop_residuals <- funop(dat$y, A_r, B_r)

      # (a4)
      # repeat until no y_{jk} deserves special attention
      if (!any(funop_residuals$special)) {
        break
      }

      # (a3) modify x_{jk} for largest y_{jk} that deserves special attention
      big_y <- funop_residuals %>%
        dplyr::filter(special == TRUE) %>%
        dplyr::top_n(1, (abs(y)))

      # change x by an amount that's proportional to its
      # position in the distribution (a)
      # here's why it's useful to have z be on same scale as the raw value
      delta_x <- big_y$z * big_y$a * change_factor
      dat$x[which(dat$i == big_y$i)] <- big_y$y - delta_x
      dat$change_type[which(dat$i == big_y$i)] <- 1

      #dat[which(dat$i == big_y$i), c('j', 'k')]
      #attr(funop_residuals, 'y_split')
      #attr(funop_residuals, 'z_split')
      #delta_x

    }

    # Done with FUNOR. To apply subsequent modifications we need
    # the following from the most recent FUNOP
    dat <- funop_residuals %>%
      dplyr::select(i, middle, a, z) %>%
      dplyr::inner_join(dat, by = 'i')

    z_split <- attr(funop_residuals, 'z_split')
    y_split <- attr(funop_residuals, 'y_split')


    ###########
    ## FUNOM ##
    ###########
    # (a5)
    # identify new interesting values based upon new A & B
    # start with threshold for extreme B
    extreme_B <- B_m * z_split
    dat <- dat %>%
      dplyr::mutate(interesting_values = ((middle == FALSE) &
                                            (z >= extreme_B)))

    # logical AND with threshold for extreme A
    extreme_A <- A_m * z_split
    dat$interesting_values <-
      ifelse(dat$interesting_values &
               (abs(dat$y - y_split) >= extreme_A), TRUE, FALSE)

    # (a6)
    # adjust just the interesting values
    delta_x <- dat %>%
      dplyr::filter(interesting_values == TRUE) %>%
      dplyr::mutate(change_type = 2) %>%
      dplyr::mutate(delta_x = (z - extreme_B) * a) %>%
      dplyr::mutate(new_x = x - delta_x) %>%
      dplyr::select(-x, -delta_x) %>%
      dplyr::rename(x = new_x)

    # select undistinguied values from dat and recombine with
    # adjusted versions of the interesting values
    dat <- dat %>%
      dplyr::filter(interesting_values == FALSE) %>%
      dplyr::bind_rows(delta_x)

    # return data to original shape
    dat <- dat %>%
      dplyr::select(j, k, x, change_type) %>%
      dplyr::arrange(j, k)

    # reshape result into a table of the original shape
    matrix(dat$x, nrow = r, byrow = TRUE)
  }
}

