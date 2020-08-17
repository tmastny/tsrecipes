test_that("steps work", {
  xfs <- step_iterator(tsrecipes::prices, steps, ts)

  for (xf in xfs) {
    expect_equal(ncol(xf$data), !!xf$ncol + 3)
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

test_that("steps keeps original time series", {
  xfs <- step_iterator(tsrecipes::prices, steps, ts)

  for (xf in xfs) {
    expect_true("ts" %in% !!names(xf$data))
  }
})

step_workflow <- function(rec, val, grid) {
  workflow() %>%
    add_model(logistic_reg() %>% set_engine("glm")) %>%
    add_recipe(rec) %>%
    tune_grid(
      resamples = val,
      grid = grid
    )
}

test_that("steps are tunable", {
  distmat <- readRDS(here::here("tests", "testthat", "prices_distmat.RDS"))
  dtw_options <- list(control = dtwclust::partitional_control(distmat = distmat))

  k_vals <- c(4, 8, 16, 32)
  prices_val <- validation_split(prices)

  recipes <- list(

    dct = recipe(prices, vars = names(prices), roles = c("id", "outcome", "input")) %>%
      step_dct(ts, k = tune()),

    dtw = recipe(prices, vars = names(prices), roles = c("id", "outcome", "input")) %>%
      step_dtw(ts, k = tune(), options = dtw_options) %>%
      step_mutate_at(all_predictors(), fn = factor)
  )

  tune_results <- recipes %>%
    map(step_workflow, prices_val, expand_grid(k = k_vals)) %>%
    map(collect_metrics)

  for (results in tune_results) {
    expect_equal(!!unique(results$k), k_vals)
  }
})
