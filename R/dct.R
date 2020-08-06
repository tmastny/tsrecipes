#' @export
step_dct <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  # TODO: if K is null, return all coefficients
  k = 4,
  preserve = FALSE,
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
      preserve = preserve,
      coefs = coefs,
      skip = skip,
      id = id
    )
  )
}

step_dct_new <- function(terms, role, trained, k, preserve, coefs, skip, id) {
  step(
    subclass = "dct",
    terms = terms,
    role = role,
    trained = trained,
    k = k,
    preserve = preserve,
    coefs = coefs,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_dct <- function(x, training, info = NULL) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)
  k <- x$k

  coefs <- list()
  for (col_name in col_names) {
    vars <- dct_transform(training[[col_name]]) %>%
      colVars()

    top_k <- sort(vars, decreasing = TRUE)[1:k] %>%
      min()

    coefs[[col_name]] <- which(vars >= top_k)
  }

  step_dct_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    k = x$k,
    preserve = x$preserve,
    coefs = coefs,
    skip = x$skip,
    id = x$id
  )
}

colVars <- function(x) {
  n <- nrow(x)
  n / (n - 1) * (colMeans(x * x) - colMeans(x)^2)
}

dct_transform <- function(l) {
  l %>%
    simplify2array() %>%
    t() %>%
    apply(1, dtt::dct)
}

#' @export
bake.step_dct <- function(object, new_data, ...) {
  coefs <- object$coefs

  dct_cols <- list()
  for (col_name in names(coefs)) {
    dct <- dct_transform(new_data[[col_name]])
    colnames(dct) <- paste0(col_name, ".", 1:ncol(dct))

    compressed_dct <- dct[, coefs[[col_name]]]
    dct_cols[[col_name]] <- compressed_dct
  }

  if (!object$preserve) {
    new_data[, names(coefs)] <- NULL
  }

  dct_cols %>%
    purrr::map(tibble::as_tibble) %>%
    dplyr::bind_cols(new_data, .)
}

