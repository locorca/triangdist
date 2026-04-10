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

  return(pdf)
}
