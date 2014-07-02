dapproxpenalty <- function(dl, beta, constant){
  currentpenalty <- as.numeric(t(dl)%*%beta)
  result <- currentpenalty/sqrt((currentpenalty^2)+constant)
  return(result)}
