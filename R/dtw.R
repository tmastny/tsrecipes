#' Clustering time series using dynamic time warping
#'
#' @description Clusters a time series using \code{\link[dtwclust:tsclust]{dtwclust::tsclust}}.
#' The number of clusters is tunable with \code{k}. Additional parameters
#' can be set using \code{options}.
#'
#' Adds a single column to new data (per input column),
#' with integers 1-k identifying the cluster.
#'
#' @param k The number of clusters, tunable.
#'
#' @param dtwclust A list of \code{\link[dtwclust:TSClusters-class]{TClusters}}
#' objects for each list of time series passed to the step, created once
#' the step has been trained.
#'
#' @inheritParams recipes::step_bs
#' @export
step_dtw <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  k = 4,
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
      dtwclust = dtwclust,
      options = options,
      skip = skip,
      id = id
    )
  )
}

step_dtw_new <- function(terms, role, trained, k, dtwclust, options, skip, id) {
  step(
    subclass = "dtw",
    terms = terms,
    role = role,
    trained = trained,
    k = k,
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

  dplyr::bind_cols(new_data, tibble::tibble(!!!cluster_cols))
}

# TODO: need to make tunable and test
