test_that('vacuum_cleaner validates input arguments', {

  expect_warning(vacuum_cleaner(matrix(c('a', 1:3), nrow = 2)), 'argument "x" must be convertable to a numeric matrix')
  expect_warning(vacuum_cleaner(matrix(1:6, nrow = 2)), 'argument "x" must have at least 3 rows and columns')
})
