library(recipes)
library(dplyr)
library(purrr)
library(stringr)
library(tidymodels)

test_that("step_dct agrees with dct", {
  prices <- tsrecipes::prices

  prepped <- recipe(prices) %>%
    step_dct(ts) %>%
    prep()

  prices_xf <- prepped %>%
    bake(prices)

  ts1_step_dct <- prices_xf %>%
    select(starts_with("dct")) %>%
    .[1, ] %>%
    unlist(use.names = FALSE)

  indices <- prepped$steps[[1]]$coefs$ts$.indices

  ts1_dct <- dtt::dct(prices$ts[[1]])[indices]

  expect_equal(ts1_step_dct, ts1_dct)
})
