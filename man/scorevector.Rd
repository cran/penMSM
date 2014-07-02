\name{scorevector}
\alias{scorevector}
\title{Score vector of the log partial likelihood of a multistate
  model.}
\description{This function calculates the score vector needed for the
  estimation of multistate models using the Fisher scoring algorithm.}
\usage{
scorevector(beta, X, risksetlist, event)
}
\arguments{
  \item{beta}{vector of regression coefficients.}
  \item{X}{design matrix.}
  \item{risksetlist}{list of length \code{n} with vectors as list
    elements, with the i-th element being the riskset belonging to the
    i-th spell.}
  \item{event}{non-censoring event indicator.}
}
\details{This function implements the score vector (i.e., the
  first partial derivative of the log partial likelihood with respect
  to the components of the regression effect vector \code{beta}).}
\value{Score vector \code{scorevector}.}
\author{Holger Reulen}
\examples{
\dontrun{scorevector(beta, X, risksetlist, event)}
}