\name{dapproxpenalty}
\alias{dapproxpenalty}
\title{First derivative of the locally quadratic approximated penalty.}
\description{This function calculates the first derivative of the
  locally quadratic approximated penalty.}
\usage{
dapproxpenalty(psv, beta, constant)
}
\arguments{
  \item{psv}{penalty structure vector that determines the l-th penalty component when multyplied with \code{beta}.}
  \item{beta}{vector of regression coefficients.}
  \item{constant}{constant that is needed for the locally (in the
    neighborhood of 0) quadratical approximation of the absolute value function.}
}
\details{This function calculates the first derivative of the locally
  quadratic approximated penalty.}
\value{The value of the derivative.}
\author{Holger Reulen}
\examples{
\dontrun{almatrix(psv, beta, constant)}
}