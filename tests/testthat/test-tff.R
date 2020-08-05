library(recipes)

test_that("step_tff creates new columns", {

})

prices <- tsrecipes::prices
prices

recipe(prices) %>%
  step_fft(ts) %>%
  prep() %>%
  bake(prices)
