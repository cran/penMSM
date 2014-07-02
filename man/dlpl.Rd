\name{dlpl}
\alias{dlpl}
\title{First derivative of the Log Partial Likelihood.}
\description{Calculates the first partial derivative of the log partial
  likelihood with respect to the linear predictor.}
\usage{
dlpl(event, b, X, Ri, Ci)
}
\arguments{
\item{event}{non-censoring event indicator.}
\item{b}{vector of regression coefficients}
\item{X}{design matrix}
\item{Ri}{list of length \code{n} with vectors as list elements,
  with the i-th element being the riskset belonging to the i-th spell.}
\item{Ci}{list of length \code{n} with vectors as list elements,
  with the i-th element capturing the indexes of risksets in which spell
  \code{i} is included.}}
\details{This function calculates the first derivative of the log partial likelihood of a Cox
  type multistate model.}
\value{A vector with the values of the partial first derivatives of the
log partial likelihood with respect to the regression effects.}
\author{Holger Reulen}
\examples{
\dontrun{dlpl(event, b, X, Ri, Ci)}
}
