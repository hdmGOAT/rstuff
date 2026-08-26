compute_error <- function(formula, vars, errors_of_vars) {
  variable_names = names(vars)
  d <- deriv(formula, variable_names)
  derivatives <- eval(d, as.list(vars))
  derivative_values = as.numeric(attr(derivatives, "gradient"))
  s = 0
  for (i in seq_along(derivative_values)){
      rawsum = derivative_values[i] * errors_of_vars[i]
      s = s + abs(rawsum)
  }
  temp = eval(formula[[2]], envir = as.list(vars))
  c(
    actual= temp,
    error = s
  )
}

# test

formula <- ~ F / (h^2 * E)

values_of_vars <- c(
  F = 72,
  h = 0.004,
  E = 70000000000
)

errors_of_vars <- c(
  F = 0.9,
  h = 0.001,
  E = 1500000000
)

compute_error(
  formula,
  values_of_vars,
  errors_of_vars
)

