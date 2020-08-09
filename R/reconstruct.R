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
    select(-coefs)
}
