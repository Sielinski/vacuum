test_that('a_qnorm validates input arguments', {
  expect_warning(a_qnorm('a', 0), 'arguments "i" and "n" must be numeric')
  expect_warning(a_qnorm(0, 'a'), 'arguments "i" and "n" must be numeric')
})

test_that('a_qnorm generates expected results', {
  expect_equal(a_qnorm(3, 5), 0)
})
