step_test_prices <- tsrecipes::prices[1:12, ]
step_test_ethanol <- tsrecipes::ethanol[1:20, ]

test_that("steps work", {
  xfs <- step_iterator(step_test_prices, steps, ts)

  for (xf in xfs) {
    expect_equal(ncol(xf$data), !!xf$ncol + 3)
    expect_equal(sum(stringr::str_starts(names(xf$data), !!xf$name)), !!xf$ncol)
  }
})

test_that("transform steps work with multiple time series", {
  prices <- step_test_prices %>%
    dplyr::mutate(ts2 = ts)

  xfs <- step_iterator(prices, steps[1:2], starts_with("ts"))

  for (xf in xfs) {
    expect_equal(ncol(xf$data), 2 * !!xf$ncol + 4)
    expect_equal(sum(stringr::str_starts(names(xf$data), !!xf$name)), 2 * !!xf$ncol)
  }
})

test_that("steps keeps original time series", {
  xfs <- step_iterator(step_test_prices, steps, ts)

  for (xf in xfs) {
    expect_true("ts" %in% !!names(xf$data))
  }
})

test_that("recipes::prep dispatches to installed tsrecipes step methods", {
  cases <- list(
    list(step = step_dct, expected_cols = 4),
    list(step = step_fft, expected_cols = 8),
    list(step = step_dtw, expected_cols = 1)
  )

  for (case in cases) {
    rec <- recipes::recipe(step_test_ethanol)
    rec <- case$step(rec, ts, k = 4)
    rec <- recipes::prep(rec)
    baked <- recipes::bake(rec, step_test_ethanol)

    expect_s3_class(rec, "recipe")
    expect_equal(ncol(baked), ncol(step_test_ethanol) + case$expected_cols)
  }
})

step_workflow <- function(rec, val, grid) {
  workflows::workflow() %>%
    workflows::add_model(parsnip::logistic_reg() %>% parsnip::set_engine("glm")) %>%
    workflows::add_recipe(rec) %>%
    tune::tune_grid(
      resamples = val,
      grid = grid
    )
}

test_that("steps are tunable", {
  small_prices <- prices[1:12, ]
  distmat <- proxy::dist(small_prices$ts, method = "dtw_basic")
  dtw_options <- list(control = dtwclust::partitional_control(distmat = distmat))

  k_vals <- c(2, 4)
  prices_val <- rsample::apparent(small_prices)

  recipes <- list(

    dct = recipes::recipe(small_prices, vars = names(small_prices), roles = c("id", "outcome", "input")) %>%
      step_dct(ts, k = tune::tune()),

    dtw = recipes::recipe(small_prices, vars = names(small_prices), roles = c("id", "outcome", "input")) %>%
      step_dtw(ts, k = tune::tune(), options = dtw_options) %>%
      recipes::step_mutate_at(recipes::all_predictors(), fn = factor)
  )

  tune_results <- recipes %>%
    purrr::map(step_workflow, prices_val, tidyr::expand_grid(k = k_vals)) %>%
    purrr::map(tune::collect_metrics)

  for (results in tune_results) {
    expect_equal(!!unique(results$k), k_vals)
  }
})
