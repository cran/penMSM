\name{lpl}
\alias{lpl}
\title{Log Partial Likelihood.}
\description{Calculates the log partial likelihood.}
\usage{
lpl(beta, X, risksetlist, event)
}
\arguments{
\item{beta}{vector of regression coefficients.}
\item{X}{design matrix.}
\item{risksetlist}{list of length \code{n} with vectors as list elements,
  with the i-th element being the riskset belonging to the i-th spell.}
\item{event}{non-censoring event indicator.}
}
\details{This function calculates the log partial likelihood of a Cox-type multistate model.}
\value{The values of the spell-specific log partial likelihood contributions.}
\author{Holger Reulen}
\examples{
\dontrun{lpl(beta, X, risksetlist, event)}
}