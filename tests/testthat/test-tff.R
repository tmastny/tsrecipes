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


test_that("step_tff agrees with tff", {
  prices <- tsrecipes::prices

  prices_xf <- recipe(prices) %>%
    step_fft(ts, fns = list(identity = identity)) %>%
    prep() %>%
    bake(prices)

  ts1_step_fft <- prices_xf %>%
    select(starts_with("ts")) %>%
    .[1, ] %>%
    unlist(use.names = FALSE)

  ts1_fft <- fft(prices$ts[[1]])[1:4]

  expect_equal(ts1_step_fft, ts1_fft)
})


test_that("step_tff preserves", {
  prices <- tsrecipes::prices

  prices_xf <- recipe(prices) %>%
    step_fft(ts, preserve = TRUE) %>%
    prep() %>%
    bake(prices)

  expect_true("ts" %in% names(prices_xf))
})

