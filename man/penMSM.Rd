\name{penMSM}
\alias{penMSM}
\title{penMSM.}
\description{L1 penalized estimation of multistate models.}
\usage{
penMSM(d, X, D, lambda, w, nu, tol, max.iter)
}
\arguments{
\item{d}{data set with variables (mandatory) \code{entry}, \code{exit},
  \code{trans}, and \code{event}.}
\item{X}{design matrix.}
\item{D}{matrix containing the penalty structure vectors \code{dl} as rows.}
\item{lambda}{vector with penalty parameters for the respective penalty
  components.}
\item{w}{vector containing weights for the respective penalty components.}
\item{nu}{numeric value denoting the weight, i.e. a value between 0 and
  1, of the Fisher scoring updates.}
\item{tol}{relative update tolerance for stopping of the estimation algorithm.}
\item{max.iter}{number of maximum iterations if tlerance is not reached.}
}
\details{This function is the core function of this package.
  It implements L1 penalized estimation of multistate models, with
  the penalty applied to absolute effects and absolute effect
  differences on transition-type specific hazard rates.}
\value{A list of length 3 with elemebts \code{B} (matrix with estimated
  effects), \code{aic} (Akaike Information Criterion), and \code{df}
  (degrees of freedom).}
\author{Holger Reulen}
\examples{
\dontrun{penMSM(d, X, D, lambda, w, nu, tol, max.iter)}
}