l2_norm <- function(v) {
  v ^ 2 %>% sum() %>% sqrt
}

#' Returns the residuals of a two-way table after removing systemic effects
#' @description
#' To remove systemic effects from values in a contingency table, the
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
#' The techique was orginally developed by John Tukey and published in
#' "The Future of Data Analytics".
#' Described in sections 38-42 of Tukey's
#' paper, starting with "The vacuum cleaner" (pp 49-60), available at
#' \url{https://www.jstor.org/stable/2237638}.
#' @examples
#' vacuum(table_8)
#' @export

vacuum <- function(x) {

  input_table <- x

  r <- nrow(input_table)
  c <- ncol(input_table)

  # number of rows and columns must be at least 3 (p 53)
  if (r < 3 | c < 3)
    print('Insuffienct size')


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
        sum(carrier_c * input_table[i,]) / sum(carrier_c ^ 2)
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
    #  mutate('row' = row_number()) %>%
    #  pivot_longer(cols = starts_with('X'),
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
