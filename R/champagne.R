#' @title Champagne glass function
#' @author samorso
#' @description A function that represents the side cut of half of the Champagne glass placed horizontally on an imaginary x-axis as the following piecewise function.
#' \deqn{f(x) = \begin{cases} 15 & 0 \leq x < 0.5 \\ 2 & 0.5 \leq x < 10 \\ 8 \log_2(|x - 9|) + 2 & 10 \leq x < 15 \\ 8 \log_2(6) + 2 & 15 \leq x \leq 20 \\ 0 & \text{otherwise} \end{cases}}
#' @param x A numeric vector.
#' @return A numeric vector.
#' @examples
#' champagne(0)
#' champagne(0.5)
#' champagne(10)
#' champagne(15)
#' champagne(20)
#' champagne(21)
#' @export
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

#' @title Champagne glass function (non-vectorized)
#' @author samorso
#' @description A non-vectorized version of the Champagne glass function.
#' @param x A numeric vector.
#' @return A numeric vector.
#' @examples
#' f1(0)
#' f1(0.5)
#' f1(10)
#' f1(15)
#' f1(20)
#' f1(21)
#' @export
f1 <- function(x) {
  y <- double(length(x))
  for (i in seq_along(y)) {
    y[i] <- g(x[i])
  }
  y
}

#' @title Champagne glass function (vectorized, using `purrr::map_dbl()`)
#' @author samorso
#' @description A vectorized version of the Champagne glass function using `purrr::map_dbl()`.
#' @param x A numeric vector.
#' @return A numeric vector.
#' @importFrom purrr map_dbl
#' @examples
#' f2(0)
#' f2(0.5)
#' f2(10)
#' f2(15)
#' f2(20)
#' f2(21)
#' @export
f2 <- function(x) purrr::map_dbl(x, g)

#' @title Champagne glass function (vectorized, using `vapply()`)
#' @author samorso
#' @description A vectorized version of the Champagne glass function using `vapply()`.
#' @param x A numeric vector.
#' @return A numeric vector.
#' @examples
#' f3(0)
#' f3(0.5)
#' f3(10)
#' f3(15)
#' f3(20)
#' f3(21)
#' @export
f3 <- function(x) vapply(X = x, FUN = g, FUN.VALUE = double(1))

#' @title Champagne glass function (vectorized, using `Vectorize()`)
#' @author samorso
#' @description A vectorized version of the Champagne glass function using `Vectorize()`.
#' @param x A numeric vector.
#' @return A numeric vector.
#' @examples
#' f4(0)
#' f4(0.5)
#' f4(10)
#' f4(15)
#' f4(20)
#' f4(21)
#' @export
f4 <- Vectorize(g, "x")

#' @title Champagne glass function (vectorized, using indicator functions)
#' @author samorso
#' @description A vectorized version of the Champagne glass function using indicator functions.
#' @param x A numeric vector.
#' @return A numeric vector.
#' @examples
#' f5(0)
#' f5(0.5)
#' f5(10)
#' f5(15)
#' f5(20)
#' f5(21)
#' @export
f5 <- function(x) {
  15 * (0 <= x & x < .5) + 2 * (.5 <= x & x < 10) +
    (8 * log2(abs(x - 9)) + 2) * (x >= 10 & x < 15) + (8 * log2(6) + 2) * (x >= 15 & x <= 20)
}

#' @title Champagne glass function (vectorized, using indicator functions and `structure()`)
#' @author samorso
#' @description A vectorized version of the Champagne glass function using indicator functions and `structure()`.
#' @param x A numeric vector.
#' @return A numeric vector.
#' @examples
#' f6(0)
#' f6(0.5)
#' f6(10)
#' f6(15)
#' f6(20)
#' f6(21)
#' @export
f <- function(x) {
  structure(
    15 * (0 <= x & x < .5) + 2 * (.5 <= x & x < 10) +
      (8 * log2(abs(x*(x != 9) - 9)) + 2)*(x >= 10 & x < 15) + (8 * log2(6) + 2) * (x >= 15 & x <= 20),
    class = "champagne"
  )
}

#' @title Plot method for champagne glass function
#' @author samorso
#' @description A plot method for the champagne glass function.
#' @param x A numeric vector.
#' @param from A numeric value.
#' @param to A numeric value.
#' @param ... Additional arguments passed to `curve()`.
#' @examples
#' champagne <- f(seq(0, 20, by = 0.01))
#' plot(champagne, from = 0, to = 20)
#' @export
plot.champagne <- function(x, from = 0, to = 20, ...) {
  curve(f, from = from, to = to, ...)
}
