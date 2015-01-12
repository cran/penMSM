\name{fisherinfoP}
\alias{fisherinfoP}
\title{Fisher information matrix of the Poisson log likelihood.}
\description{This function calculates the Fisher information matrix
  needed for the estimation of multistate models using the Fisher
  scoring algorithm.}
\usage{
fisherinfoP(mu, X)
}
\arguments{
  \item{mu}{mu.}
  \item{X}{design matrix.}
}
\details{This function implements the Fisher scoring matrix (i.e., the
  second partial derivative of the log partial likelihood with respect
  to the components of the regression effect vector \code{beta}).}
\value{Fisher information matrix \code{info}.}
\author{Holger Reulen}
\examples{
\dontrun{fisherinfo(mu, X)}
}