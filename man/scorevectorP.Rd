\name{scorevectorP}
\alias{scorevectorP}
\title{Score vector of the Poisson log likelihood.}
\description{This function calculates the score vector needed for the
  estimation of multistate models using the Fisher scoring algorithm.}
\usage{
scorevectorP(mu, X, event)
}
\arguments{
  \item{mu}{mu.}
  \item{X}{design matrix.}
  \item{event}{non-censoring event indicator.}
}
\details{This function implements the score vector (i.e., the
  first partial derivative of the Poisson log likelihood with respect
  to the components of the regression effect vector \code{beta}).}
\value{Score vector \code{scorevector}.}
\author{Holger Reulen}
\examples{
\dontrun{scorevectorP(beta, X, event)}
}