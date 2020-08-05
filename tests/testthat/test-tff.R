library(recipes)

test_that("step_tff creates new columns", {
  prices <- tsrecipes::prices

  prices_xf <- recipe(prices) %>%
    step_fft(ts) %>%
    prep() %>%
    bake(prices)

  expect_equal(dim(prices_xf), c(965, 10))
})


test_that("step_tff handles multiple time series", {
  prices <- tsrecipes::prices %>%
    mutate(ts2 = ts)

  prices_xf <- recipe(prices) %>%
    step_fft(starts_with("ts")) %>%
    prep() %>%
    bake(prices)

  expect_equal(dim(prices_xf), c(965, 18))
})
