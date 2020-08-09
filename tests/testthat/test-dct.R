library(recipes)

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
