test_that('funop validates input arguments', {
  expect_warning(funop(c('a', 1)), 'argument "x" must be a numeric vector')
  expect_warning(funop(funop(matrix(c(1, 2, 3, 4), nrow = 2))), 'argument "x" must be a numeric vector')
  expect_warning(funop(table_1, A = 'a'), 'argument "A" must be a single numeric value')
  expect_warning(funop(table_1, A = c(1, 2)), 'argument "A" must be a single numeric value')
  expect_warning(funop(table_1, A = NULL), 'argument "A" must be a single numeric value')
  expect_warning(funop(table_1, A = NA), 'argument "A" must be a single numeric value')
  expect_warning(funop(table_1, B = 'a'), 'argument "B" must be a single numeric value')
  expect_warning(funop(table_1, B = c(1, 2)), 'argument "B" must be a single numeric value')
  expect_warning(funop(table_1, B = NULL), 'argument "B" must be a single numeric value')
  expect_warning(funop(table_1, B = NA), 'argument "B" must be a single numeric value')
})

#test_that('a_qnorm generates expected results', {
#  expect_equal(funop(table_1), c(7, 11))
#})
