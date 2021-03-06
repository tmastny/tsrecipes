% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dct.R
\name{step_dct}
\alias{step_dct}
\title{Extract discrete cosine transform coefficients}
\usage{
step_dct(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  k = 4,
  coefs = NULL,
  skip = FALSE,
  id = recipes::rand_id("dct")
)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the
sequence of operations for this recipe.}

\item{...}{One or more selector functions to choose which
variables are affected by the step. See \code{\link[recipes:selections]{selections()}}
for more details. For the \code{tidy} method, these are not
currently used.}

\item{role}{For model terms created by this step, what analysis
role should they be assigned?. By default, the function assumes
that the new columns created from the original variables will be
used as predictors in a model.}

\item{trained}{A logical to indicate if the quantities for
preprocessing have been estimated.}

\item{k}{The number of discrete cosine transform coefficients, tunable.}

\item{coefs}{A list of the length and coefficient indices for each
time series passed to the step, created once the step has been trained.}

\item{skip}{A logical. Should the step be skipped when the
recipe is baked by \code{\link[recipes:bake.recipe]{bake.recipe()}}? While all operations are baked
when \code{\link[recipes:prep.recipe]{prep.recipe()}} is run, some operations may not be able to be
conducted on new data (e.g. processing the outcome variable(s)).
Care should be taken when using \code{skip = TRUE} as it may affect
the computations for subsequent operations}

\item{id}{A character string that is unique to this step to identify it.}
}
\description{
Extracts \code{k} discrete cosine transform
coefficients with the most variance across the set of time series.

See \link{reconstruct} to reconstruct the time series using the \code{k}
coefficients.

Adds \code{k} column to new data (per input column).
}
