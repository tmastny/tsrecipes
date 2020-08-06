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



test_that("step_tff preserves", {
  rowVars <- function(x) {
    n <- nrow(x)
    n / (n - 1) * (rowMeans(x * x) - rowMeans(x)^2)
  }

  l <- tsrecipes::prices$ts[1:2]
  l %>%
    simplify2array() %>%
    mvfft() %>%
    rowMeans()

  l %>%
    simplify2array() %>%
    mvfft() %>%
    rowVars()
})

test_that("step_dct works", {
  prices <- tsrecipes::prices

  prices_xf <- recipe(prices) %>%
    step_dct(ts) %>%
    prep() %>%
    bake(prices)

  expect_equal(dim(prices_xf), c(965, 6))
})

test_that("step_dct agrees with dct", {
  prices <- tsrecipes::prices

  prepped <- recipe(prices) %>%
    step_dct(ts) %>%
    prep()

  prices_xf <- prepped %>%
    bake(prices)

  ts1_step_dct <- prices_xf %>%
    select(starts_with("ts")) %>%
    .[1, ] %>%
    unlist(use.names = FALSE)

  step_index = prepped$steps[[1]]$coefs$ts

  ts1_dct <- dtt::dct(prices$ts[[1]])[step_index]

  expect_equal(ts1_step_dct, ts1_dct)
})

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

