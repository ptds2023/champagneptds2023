g <- function(x) {
  if (x < 0) {
    return(0)
  } else if (x < .5) {
    return(15)
  } else if (x < 10) {
    return(2)
  } else if (x < 15) {
    return(8 * log2(x - 9) + 2)
  } else if (x <= 20) {
    return(8 * log2(6) + 2)
  } else {
    return(0)
  }
}

f1 <- function(x) {
  y <- double(length(x))
  for (i in seq_along(y)) {
    y[i] <- g(x[i])
  }
  y
}

f2 <- function(x) purrr::map_dbl(x, g)

f3 <- function(x) vapply(X = x, FUN = g, FUN.VALUE = double(1))

f4 <- Vectorize(g, "x")

f5 <- function(x) {
  15 * (0 <= x & x < .5) + 2 * (.5 <= x & x < 10) +
    (8 * log2(abs(x - 9)) + 2) * (x >= 10 & x < 15) + (8 * log2(6) + 2) * (x >= 15 & x <= 20)
}

f <- function(x) {
  structure(
    15 * (0 <= x & x < .5) + 2 * (.5 <= x & x < 10) +
      (8 * log2(abs(x - 9)) + 2) * (x >= 10 & x < 15) + (8 * log2(6) + 2) * (x >= 15 & x <= 20),
    class = "champagne"
  )
}

plot.champagne <- function(x, from = 0, to = 20, ...) {
  curve(x, from = from, to = to, ...)
}
