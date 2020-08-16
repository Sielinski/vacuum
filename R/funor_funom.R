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
      A <- A_r
      B <- B_r

      r <- nrow(x)
      c <- ncol(x)
      n <- r * c

      # this will be used in step a6, but only need to calc once
      change_factor <- r * c / ((r - 1) * (c - 1))

      # this will be used to calc y_split and z_split
      middle_third <- (floor(n / 3) + 1):ceiling(2 * n / 3)

      # this data frame makes it easy to track all values
      # j and k are the rows and columns of the table
      dat <- data.frame(
        x = as.vector(as.matrix(x)),
        j = ifelse(1:n %% r == 0, r, 1:n %% r),
        k = ceiling(1:n / r),
        change_type = 0
      )

      ################
      ## start loop ##
      ################

      repeat {
        dat <- dat %>%
          dplyr::select(x, j, k, change_type) # this removes the calculated values from last loop

        # (a1)
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

        # (a2)
        # reimplement FUNOP, simply because I'll need z and a for step a3
        # and other values for FUNOM
        # FUNOP (b2)
        # sort the data
        dat <- dat %>%
          dplyr::arrange(y) %>%
          dplyr::mutate(i = dplyr::row_number())

        dat$middle_third <- ifelse(dat$i %in% middle_third, T, F)

        y_split <- dat %>%
          # use middle_third to calculate y_trimmed only
          #filter(middle_third == TRUE) %>%
          dplyr::summarize(median(y)) %>%
          as.numeric()

        # (b3)
        dat$a <- a_qnorm(dat$i, n)
        dat$z <- (dat$y - y_split) / dat$a

        # (b4)
        z_split <- dat %>%
          dplyr::filter(middle_third == FALSE) %>%
          dplyr::summarize(z_split = median(z)) %>%
          as.numeric()

        # (b5)
        # extreme B
        extreme_B <- B * z_split
        dat <- dat %>%
          dplyr::mutate(interesting_values = ((middle_third == FALSE) &
                                                (z > extreme_B)))

        # (b5*)
        # find actuals as extreme--or greater--than identified extreme_values
        if (sum(dat$interesting_values > 0)) {
          max_low_x <- dat %>%
            dplyr::filter(interesting_values) %>%
            dplyr::filter(x < y_split) %>%
            dplyr::summarise(max_low_x = max(x)) %>%
            as.numeric()

          dat$interesting_values <-
            ifelse(dat$x <= max_low_x, TRUE, dat$interesting_values)

          min_high_x <- dat %>%
            dplyr::filter(interesting_values) %>%
            dplyr::filter(x > y_split) %>%
            dplyr::summarise(min_high_x = min(x)) %>%
            as.numeric()

          dat$interesting_values <-
            ifelse(dat$x >= min_high_x, TRUE, dat$interesting_values)
        }

        # extreme A
        extreme_A <- A * z_split
        dat$interesting_values <-
          ifelse(dat$interesting_values &
                   (abs(dat$x - y_split) >= extreme_A), TRUE, FALSE)

        if (sum(dat$interesting_values) == 0) {
          break
        }

        # (a3)
        # FUNOR
        big_y <- dat %>%
          dplyr::filter(interesting_values == TRUE) %>%
          dplyr::top_n(1, (abs(y)))

        # change x by an amount that's proportional to its
        # position in the distribution (a)
        # here's why it's important that z be on same scale as the raw value
        delta_x <- big_y$z * big_y$a * change_factor
        dat$x[which(dat$i == big_y$i)] <- big_y$x - delta_x
        dat$change_type[which(dat$i == big_y$i)] <- 1

      }


      # FUNOM
      # (a5)
      # extreme B
      extreme_B <- B_m * z_split
      dat <- dat %>%
        dplyr::mutate(interesting_values = ((middle_third == FALSE) &
                                              (z >= extreme_B)))

      # extreme A
      extreme_A <- A_m * z_split
      dat$interesting_values <-
        ifelse(dat$interesting_values &
                 (abs(dat$x - y_split) >= extreme_A), TRUE, FALSE)

      # (a6)
      # adjust interesting values
      delta_x <- dat %>%
        dplyr::filter(interesting_values == TRUE) %>%
        dplyr::mutate(change_type = 2) %>%
        dplyr::mutate(delta_x = (z - extreme_B) * a) %>%
        dplyr::mutate(new_x = x - delta_x) %>%
        dplyr::select(-x,-delta_x) %>%
        dplyr::rename(x = new_x)

      # select undistinguied values and recombine with adjusted values
      dat <- dat %>%
        dplyr::filter(interesting_values == FALSE) %>%
        dplyr::bind_rows(delta_x)

      #dat

      # reshape result into a table of original size
      dat <- dat %>%
        dplyr::select(j, k, x) %>%
        dplyr::arrange(j, k)

      matrix(dat$x, nrow = r, byrow = TRUE)

    }

  }
