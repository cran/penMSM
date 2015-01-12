\name{dpenaltyfunction}
\alias{dpenaltyfunction}
\title{First derivative of the penalty function.}
\description{This function implements the first derivative of the penalty function.}
\usage{
dpenaltyfunction(psv, beta)
}
\arguments{
\item{psv}{penalty structure vector.}
\item{beta}{estimated regression effects.}
}
\details{This function implements the first derivative of the
  penalty function with respect to the penalty.
  The term 'penalty function' is described in detail on p. 4 in Oelker,
  Tutz (2013): A General Family of Penalties for Combining Differing
  Types of Penalties in Generalized Structured Models.}
\value{Value of the first derivative of the penalty function (note: this
  is always 1, since the penalty fucntion p(xi)=xi is just the identity).}
\author{Holger Reulen}
\examples{
\dontrun{dpenaltyfunction(psv, beta)}
}
