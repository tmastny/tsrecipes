#' Extract discrete cosine transform coefficients
#'
#' @description Extracts \code{k} discrete cosine transform
#' coefficients with the most variance across the set of time series.
#'
#' See \link{reconstruct} to reconstruct the time series using the \code{k}
#' coefficients.
#'
#' Adds \code{k} column to new data (per input column).
#'
#' @param k The number of discrete cosine transform coefficients, tunable.
#'
#' @param coefs A list of the length and coefficient indices for each
#' time series passed to the step, created once the step has been trained.
#'
#' @inheritParams recipes::step_bs
#' @export
step_dct <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  k = 4,
  coefs = NULL,
  skip = FALSE,
  id = recipes::rand_id("dct")
) {

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_dct_new(
      terms = terms,
      trained = trained,
      role = role,
      k = k,
      coefs = coefs,
      skip = skip,
      id = id
    )
  )
}

# TODO:
#   Add the following examples:
#   1. where you want to return a different `k` coefficients, so you
#      apply `step_dct` twice (how do multivariate ts work here?)
#
#   2. You want to use a different selection procedure, so you return
#      `k = length(ts)`.

step_dct_new <- function(terms, role, trained, k, coefs, skip, id) {
  step(
    subclass = "dct",
    terms = terms,
    role = role,
    trained = trained,
    k = k,
    coefs = coefs,
    skip = skip,
    id = id
  )
}

#' @export
colVars <- function(x) {
  n <- nrow(x)
  n / (n - 1) * (colMeans(x * x) - colMeans(x)^2)
}

#' @export
prep.step_dct <- function(x, training, info = NULL) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)
  k <- x$k

  coefs <- list()
  for (col_name in col_names) {
    vars <- dct_transform(training[[col_name]]) %>%
      colVars()

    indices <- 1:length(vars)
    if (!is.null(k)) {
      top_k <- sort(vars, decreasing = TRUE)[1:k] %>%
        min()

      indices <- which(vars >= top_k)
    }

    coefs[[col_name]] <- list(
      .length = length(vars),
      .indices = indices
    )
  }

  step_dct_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    k = x$k,
    coefs = coefs,
    skip = x$skip,
    id = x$id
  )
}

dct_transform <- function(l) {
  l %>%
    simplify2array() %>%
    mvfdct() %>%
    t()
}

#' @export
bake.step_dct <- function(object, new_data, ...) {
  coefs <- object$coefs

  dct_cols <- list()
  for (col_name in names(coefs)) {
    dct <- dct_transform(new_data[[col_name]])
    colnames(dct) <- paste0("dct_", 1:ncol(dct), "_", col_name)

    compressed_dct <- dct[, coefs[[col_name]]$.indices]
    dct_cols[[col_name]] <- compressed_dct
  }

  dct_cols %>%
    purrr::map(tibble::as_tibble) %>%
    dplyr::bind_cols(new_data, .)
}


#' @export
tunable.step_dct <- function(x, ...) {
  tibble::tibble(
    name = "k",
    call_info = list(list(pkg = "dials", fun = "num_terms", range = c(1L, 4L))),
    source = "recipe",
    component = "step_dct",
    component_id = x$id
  )
}

#' @export
fdct <- function(x) {
  N <- length(x)
  P <- exp(complex(imaginary = pi / 2 / N) * (seq(2 * N) - 1))

  w <- c(x, x[N:1])
  0.5 * Re(fft(w) / P)[1:N]
}

#' @export
mvfdct <- function(m) {
  N <- nrow(m)
  P <- exp(complex(imaginary = pi / 2 / N) * (seq(2 * N) - 1))

  w <- rbind(m, m[N:1, ])
  0.5 * Re(mvfft(w) / P)[1:N, ]
}
