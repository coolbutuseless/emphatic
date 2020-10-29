


test_that("multiplication works", {

  res <- loc_expr_to_ids(mtcars, quote(mpg == 21), axis = 'row')
  expect_identical(res, c(1L, 2L))

  res <- loc_expr_to_ids(mtcars, quote(mpg), axis = 'column')
  expect_identical(res, which(colnames(mtcars) == 'mpg'))

  res <- loc_expr_to_ids(mtcars, quote(c(1, 2, 3)), axis = 'column')
  expect_identical(res, c(1, 2, 3))

  res <- loc_expr_to_ids(mtcars, quote(ends_with('t')), axis = 'column')
  expect_identical(res, c(5L, 6L))
})
