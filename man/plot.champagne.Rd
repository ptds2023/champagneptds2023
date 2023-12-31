% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/champagne.R
\name{plot.champagne}
\alias{plot.champagne}
\title{Plot method for champagne glass function}
\usage{
\method{plot}{champagne}(x, from = 0, to = 20, ...)
}
\arguments{
\item{x}{A numeric vector.}

\item{from}{A numeric value.}

\item{to}{A numeric value.}

\item{...}{Additional arguments passed to `curve()`.}
}
\description{
A plot method for the champagne glass function.
}
\examples{
champagne <- f(seq(0, 20, by = 0.01))
plot(champagne, from = 0, to = 20)
}
\author{
samorso
}
