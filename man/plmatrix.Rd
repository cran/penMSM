\name{plmatrix}
\alias{plmatrix}
\title{plmatrix.}
\description{This function establishes the single vectors that set up
  the penalty matrix in function \code{penaltymatrix}.}
\usage{
plmatrix(psv, beta, constant)
}
\arguments{
  \item{psv}{index vector that determines the l-th penalty component
    when multiplied with \code{beta}.}
  \item{beta}{vector of regression coefficients.}
  \item{constant}{constant that is needed for the locally (in the
    neighborhood of 0) quadratical approximation of the absolute value
    function.}
}
\details{This function calculates the value of the l-th penalty
  component, which is a locally (in the neighborhood of 0) quadratical
  approximation of the absolute value of a regression coefficient, or
  the difference between two coefficients, respectively.}
\value{The object \code{result} takes the value of the l-th penalty
  component.}
\author{Holger Reulen}
\examples{
\dontrun{plmatrix(psv, beta, constant)}
}