\name{llP}
\alias{llP}
\title{Log Likelihood for Poisson Regression.}
\description{Calculates the log likelihood for poisson regression.}
\usage{
llP(beta, X, event, offset)
}
\arguments{
\item{beta}{vector of regression coefficients.}
\item{X}{design matrix.}
\item{event}{non-censoring event indicator.}
\item{offset}{offset.}
}
\details{This function calculates the Poisson log likelihood.}
\value{The values of the Poisson log likelihood.}
\author{Holger Reulen}
\examples{
\dontrun{llP(beta, X, event, offset)}
}