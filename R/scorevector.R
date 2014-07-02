scorevector <- function(beta, X, risksetlist, event){
  n <- length(event)
  f <- as.numeric(X%*%beta)
  ef <- exp(f)
  p <- length(beta)
  scorevector <- rep(0, p)
  riskmatrix <- matrix(nrow=n, ncol=p, 0)
  for (i in 1:n){
    riskset <- risksetlist[[i]]
    ef.riskset <- ef[riskset]
    currentrisk <- sum(ef.riskset)
    for(j in 1:p){
      riskmatrix[i, j] <- sum(ef.riskset*X[riskset,j]/currentrisk)
      }
    }
  for(j in 1:p){
    scorevector[j] <- sum(event*X[, j] - event*riskmatrix[, j])
    }
  return(scorevector)}
