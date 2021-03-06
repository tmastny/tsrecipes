test_that("step_dtw makes default clusters", {
  xfs <- step_iterator(tsrecipes::prices, steps[3], ts)

  clusters <- xfs$dtw$data$dtwclust_ts

  expect_equal(sort(unique(clusters)), c(1, 2, 3, 4))
})

test_that("step_dtw makes k clusters", {
  xfs <- c(2, 5, 8) %>%
    map(~list(
      step = step_iterator(tsrecipes::prices, steps[3], ts, k = .),
      k = .
    ))

  for (xf in xfs) {
    clusters <- xf$step$dtw$data$dtwclust_ts
    expect_equal(sort(unique(clusters)), 1:xf$k)
  }
})

test_that("step_dtw accepts options", {

  prepped <- recipe(tsrecipes::prices) %>%
    step_dtw(ts, options = list(type = "hierarchical")) %>%
    prep()

  clust_obj <- prepped$steps[[1]]$dtwclust$ts

  expect_equal(clust_obj@type, "hierarchical")
})
