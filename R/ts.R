step_fft <- function(
  recipe,
  ...,
  role = NA,
  k = 4,
  fns = list(Re, Im)
) {

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_fft_new(
      terms = terms,
      trained = trained,
      role = role,
      k = k,
      fns = fns
    )
  )
}

step_fft_new <- function(terms, role, trained, k, fns) {
  step(
    subclass = "fft",
    terms = terms,
    role = role,
    trained = trained,
    k = k,
    fns = fns
  )
}

prep.step_fft <- function(x, training, info) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)


}
