test_that("colVars calculates vars", {
  ts <- tsrecipes::prices$ts

  col_vars_fn <- ts %>%
    dct_transform() %>%
    colVars()

  col_vars_apply <- ts %>%
    dct_transform() %>%
    apply(2, var)

  expect_equal(col_vars_fn, col_vars_apply)
})
