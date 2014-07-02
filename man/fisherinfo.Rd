\name{fisherinfo}
\alias{fisherinfo}
\title{Fisher information matrix of the log partial likelihood of a
  multistate model.}
\description{This function calculates the Fisher information matrix
  needed for the estimation of multistate models using the Fisher
  scoring algorithm.}
\usage{
fisherinfo(beta, X, risksetlist, event)
}
\arguments{
  \item{beta}{vector of regression coefficients.}
  \item{X}{design matrix.}
  \item{risksetlist}{list of length \code{n} with vectors as list elements,
    with the i-th element being the riskset belonging to the i-th spell.}
  \item{event}{non-censoring event indicator.}
}
\details{This function implements the Fisher scoring matrix (i.e., the
  second partial derivative of the log partial likelihood with respect
  to the components of the regression effect vector \code{beta}).}
\value{Fisher information matrix \code{info}.}
\author{Holger Reulen}
\examples{
\dontrun{fisherinfo(beta, X, risksetlist, event)}
}