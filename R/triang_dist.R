#' Density function for the triangular distribution
#' @param x vector of quantiles
#' @param min lower limit
#' @param max upper limit
#' @param mode mode
#' @return A numeric vector of densities
#' @export

dtriang <- function(x, min, max, mode) {

  if (any(min > max)) stop("min must be less than or equal to max")
  if (any(mode < min | mode > max)) stop("mode must be within [min, max]")

  h <- 2 / (max - min)

  pdf <- numeric(length(x))

  idx1 <- x >= min & x < mode
  pdf[idx1] <- h * (x[idx1] - min) / (mode - min)

  idx2 <- x >= mode & x <= max
  pdf[idx2] <- h * (max - x[idx2]) / (max - mode)

  pdf
}

#' Distribution function for the triangular distribution
#' @param q vector of quantiles
#' @param min lower limit
#' @param max upper limit
#' @param mode mode
#' @return A numeric vector of probabilities
#' @export

ptriang <- function(q, min, max, mode) {

  if (any(min > max)) stop("min must be less than or equal to max")
  if (any(mode < min | mode > max)) stop("mode must be within [min, max]")

  res <- numeric(length(q))

  idx1 <- q >= min & q < mode
  res[idx1] <- (q[idx1] - min)^2 / ((max - min) * (mode - min))

  idx2 <- q >= mode & q <= max
  res[idx2] <- 1 - (max - q[idx2])^2 / ((max - min) * (max - mode))

  res[q > max] <- 1

  res
}

#' Quantile function for the triangular distribution
#' @param p vector of probabilities
#' @param min lower limit
#' @param max upper limit
#' @param mode mode
#' @return A numeric vector of quantiles
#' @export

qtriang <- function(p, min, max, mode) {

  if (any(min > max)) stop("min must be less than or equal to max")
  if (any(mode < min | mode > max)) stop("mode must be within [min, max]")
  if (any(p < 0 | p > 1)) stop("p must be in [0,1]")

  res <- numeric(length(p))
  p_mode <- (mode - min) / (max - min)

  idx1 <- p < p_mode
  res[idx1] <- min + sqrt(p[idx1] * (max - min) * (mode - min))

  idx2 <- p >= p_mode
  res[idx2] <- max - sqrt((1 - p[idx2]) * (max - min) * (max - mode))

  res
}

#' Random generation for the triangular distribution
#' @param n number of observations
#' @param min lower limit
#' @param max upper limit
#' @param mode mode
#' @return A numeric vector of random values
#' @export

rtriang <- function(n, min, max, mode) {

  qtriang(runif(n), min, max, mode)
}
