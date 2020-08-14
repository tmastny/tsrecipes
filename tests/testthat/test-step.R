test_that("steps work", {
  xfs <- step_iterator(tsrecipes::prices, steps, ts)

  for (xf in xfs) {
    expect_equal(ncol(xf$data), !!xf$ncol + 2)
    expect_equal(sum(str_starts(names(xf$data), !!xf$name)), !!xf$ncol)
  }
})

test_that("transform steps work with multiple time series", {
  prices <- tsrecipes::prices %>%
    mutate(ts2 = ts)

  xfs <- step_iterator(prices, steps[1:2], starts_with("ts"))

  for (xf in xfs) {
    expect_equal(ncol(xf$data), 2 * !!xf$ncol + 2)
    expect_equal(sum(str_starts(names(xf$data), !!xf$name)), 2 * !!xf$ncol)
  }
})

test_that("steps preserve works", {
  xfs <- step_iterator(tsrecipes::prices, steps, ts, preserve = TRUE)

  for (xf in xfs) {
    expect_true("ts" %in% !!names(xf$data))
  }
})
