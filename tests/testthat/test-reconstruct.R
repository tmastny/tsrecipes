test_that("reconstruct has same length as original time series", {
  prices <- tsrecipes::prices

  trained_rec <- recipe(prices) %>%
    step_dct(ts) %>%
    prep()

  prices_xf <- bake(trained_rec, prices)

  check_length <- prices_xf %>%
    reconstruct("ts", trained_rec$steps[[1]], starts_with("dct_")) %>%
    rowwise() %>%
    mutate(recon_same_length = length(ts_recon) == 60)

  expect_true(all(check_length$recon_same_length))
})
