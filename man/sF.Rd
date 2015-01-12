\name{sF}
\alias{sF}
\title{Score vector and Fisher information matrix of the Poisson log likelihood.}
\description{This function calculates the score vector and the Fisher information matrix needed for the
  estimation of multistate models using the Fisher scoring algorithm.}
\usage{
sF(mu, X, event)
}
\arguments{
  \item{mu}{mu.}
  \item{X}{design matrix.}
  \item{event}{non-censoring event indicator.}
}
\details{This function implements the score vector and Fisher information matrix.}
\value{s and F.}
\author{Holger Reulen}
\examples{
\dontrun{sF(mu, X, event)}
}