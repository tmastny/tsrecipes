steps <- list(
  dct = list(name = "dct", step = step_dct, ncol = 4),
  fft = list(name = "fft", step = step_fft, ncol = 8),
  dtw = list(name = "dtw", step = step_dtw, ncol = 1)
)

step_iterator <- function(x, steps, ...) {
  xfs <- steps

  for (step in steps) {
    xf <- recipe(x) %>%
      step$step(...) %>%
      prep() %>%
      bake(prices)

    xfs[[step$name]]$data <- xf
  }

  xfs
}
