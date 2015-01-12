\name{buildrisksets}
\alias{buildrisksets}
\title{Calculation of risksets needed for partial likelihood formulation
  of multistate models.}
\description{This function calculates the risksets needed to calculate
  the partial likelihood of a multistate model, and/or it's derivatives.}
\usage{
buildrisksets(entry, exit, trans, event, trace)
}
\arguments{
  \item{entry}{vector with entry times.}
  \item{exit}{vector with exit times.}
  \item{trans}{vector with transition types.}
  \item{event}{vector with noncensoring event indicators.}
  \item{trace}{logical triggering printout of status information during the fitting process.}
}
\details{This function calculates risksets.}
\value{A list of length 2 with elements \code{Ci} and \code{Ri}, each
  vectors of length \code{n}.
}
\author{Holger Reulen}