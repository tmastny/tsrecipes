#' @export
step_dtw <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  k = 4,
  preserve = FALSE,
  dtwclust = NULL,
  options = list(),
  skip = FALSE,
  id = recipes::rand_id("dtw")
) {

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_dtw_new(
      terms = terms,
      trained = trained,
      role = role,
      k = k,
      preserve = preserve,
      dtwclust = dtwclust,
      options = options,
      skip = skip,
      id = id
    )
  )
}

step_dtw_new <- function(
  terms, role, trained, k, preserve, dtwclust, options, skip, id
) {
  step(
    subclass = "dtw",
    terms = terms,
    role = role,
    trained = trained,
    k = k,
    preserve = preserve,
    dtwclust = dtwclust,
    options = options,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_dtw <- function(x, training, info = NULL) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)

  dtwclust <- list()
  for (col_name in col_names) {

    clusters <- do.call(
      dtwclust::tsclust,
      c(list(series = training[[col_name]], k = x$k), x$options)
    )

    dtwclust[[col_name]] <- clusters
  }

  step_dtw_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    k = x$k,
    preserve = x$preserve,
    dtwclust = dtwclust,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_dtw <- function(object, new_data, ...) {
  dtwclust <- object$dtwclust

  cluster_cols <- list()
  for (col_name in names(dtwclust)) {
    new_col_name <- paste0("dtwclust_", col_name)
    cluster_cols[[new_col_name]] <- predict(
      dtwclust[[col_name]], new_data[[col_name]]
    )
  }

  if (!object$preserve) {
    new_data[, names(dtwclust)] <- NULL
  }

  dplyr::bind_cols(new_data, tibble::tibble(!!!cluster_cols))
}
