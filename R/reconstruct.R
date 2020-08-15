#' @export
invert <- function(step, ...) {
  UseMethod("invert")
}

#' @export
invert.default <- function(step, ...) {
  stop("step is not supported")
}


#' @export
invert.step_dct <- function(step, name, coefs) {
  n <- step$coefs[[name]]$.length
  indices <- step$coefs[[name]]$.indices

  dct <- vector(mode = "numeric", length = n)
  dct[indices] <- coefs

  dtt::dct(dct, inverted = TRUE)
}

#' Reconstruct time series
#'
#' Reconstruct the time series `name` using the trained \link{step_dct}
#' with the columns specified in `...`.
#'
#' May be a lossy reconstruct of the time series.
#'
#' @param x data frame
#'
#' @param name name of the time series to reconstruct (the original column name
#' before `bake`).
#'
#' @param step the trained step from the recipe
#'
#' @param ... tidy-selection of columns to reconstruct from
#'
#' @export
reconstruct <- function(x, name, step, ...) {
  x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(coefs = list(c(dplyr::c_across(...)))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      "{name}_recon" := list(invert(step, name, coefs)),
      n = list(1:step$coefs[[name]]$.length)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-coefs)
}
