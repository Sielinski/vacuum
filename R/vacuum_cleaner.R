l2_norm <- function(v) {
  v ^ 2 %>% sum() %>% sqrt
}

#' Returns the residuals of a two-way table after removing systemic effects
#' @description
#' To remove systemic effects from values in a contingency table,
#' vacuum cleaner uses regression to identify the table's main effect
#' (dual regression), row effect (deviations of row
#' regression from dual regression), and column effect (deviations of
#' column regression from dual regression).
#'
#' Regression is performed twice: First on the table's original values,
#' then on the resulting residuals. The output is a table of residuals
#' "vacuum cleaned" of likely systemic effects.
#' @param x
#' Two-way table to analyze (must be 3x3 or greater).
#' @return
#' Residuals of \code{x}
#' @seealso
#' [vacuum::funop()], [vacuum::funor_funom()]
#' @references
#' Tukey, John W. "The Future of Data Analysis."
#' \emph{The Annals of Mathematical Statistics},
#' \emph{33}(1), 1962, pp 1-67. \emph{JSTOR},
#' \url{http://www.jstor.org/stable/2237638}.
#' @examples
#' vacuum_cleaner(table_8)
#' @export

vacuum_cleaner <- function(x) {
  x <- as.matrix(x)

  if (!is.matrix(x) | !is.numeric(x)) {
    warning('argument "x" must be convertable to a numeric matrix')
  } else if (nrow(x) < 3 | ncol(x) < 3) {
    warning('argument "x" must have at least 3 rows and columns') # (p 53)
  } else {

    input_table <- x

    r <- nrow(input_table)
    c <- ncol(input_table)

    ######################
    ## Initial carriers ##
    ######################

    # sqrt(1/n)

    carrier_r <- rep(sqrt(1 / r), r)
    carrier_c <- rep(sqrt(1 / c), c)


    ###################
    ## Start of loop ##
    ###################

    # Tukey passes through the loop twice.
    # Suggests further passes possible in the "attachments" section (p 53)
    for (pass in 1:2) {
      ##################
      ## Coefficients ##
      ##################

      # Calculate the column coefficients
      coef_c <- rep(NA, c)
      for (i in 1:c) {
        # loop through every column, summing every row
        # denominator is based upon the number of rows in the column
        coef_c[i] <-
          sum(carrier_r * input_table[, i]) / sum(carrier_r ^ 2)
      }

      # Calculate the row coefficients
      coef_r <- rep(NA, r)
      for (i in 1:r) {
        # loop through every row, summing every column
        # denominator is based upon the number of columns in the row
        coef_r[i] <-
          sum(carrier_c * input_table[i, ]) / sum(carrier_c ^ 2)
      }


      ##########
      ## y_ab ##
      ##########

      # either one of these
      y_ab <- sum(coef_c * carrier_c) / sum(carrier_c ^ 2)
      y_ab <- sum(carrier_r * coef_r) / sum(carrier_r ^ 2)


      ########################
      ## Apply subprocedure ##
      ########################

      # create a destination for the output
      output_table <- input_table

      for (i in 1:r) {
        for (j in 1:c) {
          output_table[i, j] <- input_table[i, j] -
            carrier_r[i] * (coef_c[j] - carrier_c[j] * y_ab) -
            carrier_c[j] * (coef_r[i] - carrier_r[i] * y_ab) -
            y_ab * carrier_c[j] * carrier_r[i]
        }
      }

      # These are the coefficients that will get carried forward
      coef_c <- coef_c - carrier_c * y_ab
      coef_r <- coef_r - carrier_r * y_ab

      #########
      ## aov ##
      #########

      #sum(coef_c ^ 2)
      #var(coef_c)
      #
      #sum(coef_r ^ 2)
      #var(coef_r)
      #
      #dat <- output_table %>%
      #  dplyr::mutate('row' = dplyr::row_number()) %>%
      #  tidyr::pivot_longer(cols = starts_with('X'),
      #               names_to = 'col',
      #               values_to = 'value')
      #
      #dat$row <- as.factor(dat$row)
      #
      #aov_results <- aov(value ~ row + col, dat)
      #summary(aov_results)


      ########################
      ## Prep for next pass ##
      ########################

      # normalize coefficients because we want sqrt(sum(a^2)) == 1
      carrier_r <- coef_r / l2_norm(coef_r)
      carrier_c <- coef_c / l2_norm(coef_c)

      input_table <- output_table

      #################
      ## End of loop ##
      #################

    }
    output_table
  }
}
