penaltymatrix <- function(lambda, D, beta, w, constant){
    if(length(lambda)!=nrow(D)){
        stop("check dimensions for lambda and almatrix.")
    }
    p <- ncol(D)
    L <- nrow(D)
    plambda <- matrix(nrow=p, ncol=p, 0)
    for(l in 1:L){
        helpobject <- plmatrix(dl=D[l, ], beta=beta, constant=constant)
        plambda <- plambda + lambda[l]*w[l]*helpobject
    }
    return(plambda)
}
