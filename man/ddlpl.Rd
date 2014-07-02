\name{ddlpl}
\alias{ddlpl}
\title{ddlpl.}
\description{Second partial derivative of the log partial likelihood
  with respect to the linear predictor.}
\usage{
ddlpl(b, X, Ri, Ci)
}
\arguments{
\item{b}{vector of regression coefficients.}
\item{X}{design matrix.}
\item{Ri}{list of length \code{n} with vectors as list elements,
  with the i-th element being the riskset belonging to the i-th spell.}  
\item{Ci}{list of length \code{n} with vectors as list elements,
  with the i-th element capturing the indexes of risksets in which spell
  \code{i} is included.} 
}
\details{This function calculates the second partial derivative of the log partial likelihood.}
\value{A vector with second gradients.}
\author{Holger Reulen}
\examples{
\dontrun{ddlpl(b, X, Ri, Ci)}
}