plmatrix <- function(dl, beta, constant){
  part1 <- dpenaltyfunction(dl=dl, beta=beta)
  part2 <- dapproxpenalty(dl=dl, beta=beta, constant=constant)
  ho <- as.numeric(dl%*%beta)
  if(ho == 0){
      ho <- 1e-05}
  part2 <- part2/ho
  part3 <- dl%*%t(dl)
  result <- part1*part2*part3
  return(result)}
