\name{penaltymatrix}
\alias{penaltymatrix}
\title{Penalty matrix for L1 penalized estimation of multistate models.}
\description{This builds up a penalty matrix needed for the
  penalized estimation of multistate models.}
\usage{
penaltymatrix(lambda, PSM, beta, w, constant)
}
\arguments{
  \item{lambda}{vector with penalty parameters for the respective penalty
    components.}
  \item{PSM}{penalty structure matrix containing the penalty structure vectors \code{psv} as rows.}
  \item{beta}{vector of regression coefficients.}
  \item{w}{vector containing weights for the respective penalty components.}
  \item{constant}{constat that is needed for the locally (in the
    neighborhood of 0) quadratical approximation of the absolute value
    function.}
}
\details{This function calculates the penalty matrix needed for the
  penalized estimation of multistate models.}
\value{A penalty matrix \code{plambda}.}
\author{Holger Reulen}
\examples{
\dontrun{penaltymatrix(lambda, PSM, beta, w, constant)}
}