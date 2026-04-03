#' Extract discrete Fourier transform coefficients
#'
#' @description In almost all cases, use \link{step_dct} instead.
#' The discrete cosine transform has better compaction in general.
#' DCT coefficients are always real, while DFT returns complex numbers,
#' which increases the number of dimensions.
#'
#' @param k The number of discrete Fourier transform coefficients
#'
#' @param series Name of column to transform,  created once the step has been trained.
#'
#' @references Sayood, K. Introduction to Data Compression
#'
#' @inheritParams recipes::step_bs
#' @export
step_fft <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  k = 4,
  series = NULL,
  skip = FALSE,
  id = recipes::rand_id("tff")
) {

  terms <- recipes::ellipse_check(...)

  recipes::add_step(
    recipe,
    step_fft_new(
      terms = terms,
      trained = trained,
      role = role,
      k = k,
      series = series,
      skip = skip,
      id = id
    )
  )
}

step_fft_new <- function(terms, role, trained, k, series, skip, id) {
  recipes::step(
    subclass = "fft",
    terms = terms,
    role = role,
    trained = trained,
    k = k,
    series = series,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_fft <- function(x, training, info = NULL) {
  col_names <- recipes::recipes_eval_select(
    quos = x$terms,
    data = training,
    info = info
  )

  step_fft_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    k = x$k,
    series = col_names,
    skip = x$skip,
    id = x$id
  )
}

ttf_transform <- function(l, name, k) {
  tff_mat <- t(mvfft(simplify2array(l)))[, 1:k]

  real_mat <- Re(tff_mat)
  colnames(real_mat) <- paste0("fft_re_", 1:ncol(tff_mat), "_", name)

  img_mat <- Im(tff_mat)
  colnames(img_mat) <- paste0("fft_im_", 1:ncol(tff_mat), "_", name)

  purrr::map_dfc(list(real_mat, img_mat), tibble::as_tibble)
}

#' @export
bake.step_fft <- function(object, new_data, ...) {
  col_names <- object$series

  tff_cols <- purrr::imap_dfc(new_data[, col_names], ttf_transform, object$k)

  dplyr::bind_cols(new_data, tff_cols)
}
