penMSM <- function(d, X, D, lambda, w, nu = 0.75, tol = 1e-10, max.iter = 200){
    betahatold <- B <- rep(0, ncol(X))
    diff <- Inf
    count <- 1
    lh <- l <- 0
    risksetlist <- buildrisksets(entry = d$entry, exit = d$exit, trans = d$trans, event = d$event)
    risksetlist <- risksetlist$Ri
    cat("start estimation:\n")
    cat("  .")
    while( (diff > tol) & (count < (max.iter+1)) ){
        F <- fisherinfo(beta=betahatold, X=X, risksetlist=risksetlist, event=d$event)
        s <- scorevector(beta=betahatold, X=X, risksetlist=risksetlist, event=d$event)
        A <- penaltymatrix(lambda=lambda, D=D, beta=betahatold, w=w,
                           constant=1e-10)
        betahatnew <- betahatold-nu*solve(-F-A)%*%(s-A%*%betahatold)
        diff <- as.numeric(sum(abs(betahatnew-betahatold))/sum(abs(betahatnew)))
        B <- cbind(B, betahatnew)
        betahatold <- betahatnew
        if(count%%20 != 0){
            cat(".")
        }else{
            cat(paste(" coef: ", paste(round(betahatnew, 2),
                                       collapse=", "), sep=""))
            cat("\n  .")
        }
        count <- count+1
    }
    if(count >= max.iter){
        cat(" estimation stopped because max. number of iterations was reached.")
        cat("\n")
        cat(paste(" relative change in last iteration: ", round(diff, 4), sep=""))
    }
    cat("\n")
    df <- sum(diag(solve(-F-A)%*%(-F)))
    beta <- B[, ncol(B)]
    aic <- 2*(-lpl(beta=beta, X=X, risksetlist=risksetlist, event=d$event) + df)
    return(list(B=B, aic=aic, df=df))}
