\name{penMSM}
\alias{penMSM}
\title{penMSM.}
\description{L1 penalized estimation of multistate models.}
\usage{
penMSM(type = "fused", d, X, PSM1, PSM2, lambda1, lambda2, w, betastart, nu = 0.5, 
tol = 1e-10, max.iter = 50, trace = TRUE, diagnostics = TRUE, family = "coxph", 
poissonresponse = NULL, poissonoffset = NULL, constant.approx = 1e-8)
}
\arguments{
\item{type}{character defining the type of penalty, either \code{fused} or \code{lasso}.}
\item{d}{data set with variables (mandatory) \code{entry}, \code{exit},
  \code{trans}, and \code{event}.}
\item{X}{design matrix.}
\item{PSM1}{penalty structure matrix containing the penalty structure vectors \code{psv} as
  rows (lasso part).}
\item{PSM2}{penalty structure matrix containing the penalty structure vectors \code{psv} as
  rows (fusion part).}
\item{lambda1}{vector with penalty parameters for the respective penalty
  components (lasso part).}
\item{lambda2}{vector with penalty parameters for the respective penalty
  components (fusion part).}
\item{w}{vector containing weights for the respective penalty
  components.}
\item{betastart}{vector containing starting values for beta.}
\item{nu}{numeric value denoting the weight, i.e. a value between 0 and
  1, of the Fisher scoring updates.}
\item{tol}{relative update tolerance for stopping of the estimation algorithm.}
\item{max.iter}{number of maximum iterations if tlerance is not reached.}
\item{trace}{logical triggering printout of status information during the fitting process. .}
\item{diagnostics}{logical triggering that Fisher matrix, score vector, and approximated penalty matrix are returned with the results.}
\item{family}{character defining the likelihood to be used.}
\item{poissonresponse}{response values for poisson likelihood (if used).}
\item{poissonoffset}{offset values for poisson likelihood (if used).}
\item{constant.approx}{constant for locally squared approximation of the absolute value penalty function.}
}
\details{This function is the core function of this package.
  It implements L1 penalized estimation of multistate models, with
  the penalty applied to absolute effects and absolute effect
  differences on transition-type specific hazard rates.}
\value{A list with elements \code{B} (matrix with estimated
  effects), \code{aic} (Akaike Information Criterion), \code{gcv} (GCV
  criterion), \code{df} (degrees of freedom), and (if \code{diagnostics} are requested)
  \code{F} (Fisher matrix), \code{s} (score vector), and
  \code{A} (approximated penalty matrix).}
\author{Holger Reulen}
\examples{
\dontrun{penMSMtype = "fused", d, X, PSM1, PSM2, lambda1, lambda2, w, 
betastart, nu = 0.5, tol = 1e-10, max.iter = 50, trace = TRUE, 
diagnostics = TRUE, family = "coxph", poissonresponse = NULL, 
poissonoffset = NULL, constant.approx = 1e-8)}
}