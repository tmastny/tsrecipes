step_fft <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  k = 4,
  fns = list(Re = Re, Im = Im),
  series = NULL,
  skip = FALSE,
  id = recipes::rand_id("tff")
) {

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_fft_new(
      terms = terms,
      trained = trained,
      role = role,
      k = k,
      fns = fns,
      series = series,
      skip = skip,
      id = id
    )
  )
}

step_fft_new <- function(terms, role, trained, k, fns, series, skip, id) {
  step(
    subclass = "fft",
    terms = terms,
    role = role,
    trained = trained,
    k = k,
    fns = fns,
    series = series,
    skip = skip,
    id = id
  )
}

prep.step_fft <- function(x, training, info = NULL) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)

  step_fft_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    k = x$k,
    fns = x$fns,
    series = col_names,
    skip = x$skip,
    id = x$id
  )
}

ttf_transform <- function(l, name, k, fns) {
  tff_mat <- l %>%
    simplify2array() %>%
    t() %>%
    fft() %>%
    .[, 1:k]

  colnames(tff_mat) <- paste0(name, "_", 1:ncol(tff_mat))

  purrr::imap(fns, apply_fn, tff_mat) %>%
    purrr::map_dfc(as_tibble)
}

apply_fn <- function(fn, fn_name, mat) {
  colnames(mat) <- paste0(colnames(mat), "_", fn_name)
  fn(mat)
}

bake.step_fft <- function(object, new_data, ...) {
  col_names <- object$series

  tff_cols <- new_data[, col_names] %>%
    purrr::imap_dfc(ttf_transform, object$k, object$fns)

  new_data[, col_names] <- NULL
  dplyr::bind_cols(new_data, tff_cols)
}

