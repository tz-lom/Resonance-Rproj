calcCSP <- function(A,B){
  tmp <- t(A) %*% A
  RA <- tmp/sum(diag(tmp))
  
  tmp <- t(B) %*% B
  RB <- tmp/sum(diag(tmp))
  
  Rsum <- RA+RB;

  EV <- eigen(Rsum, symmetric = T)
  tmp <- order(EV$values, decreasing = T)
  EValsum <- EV$values[tmp]
  EVecsum <- EV$vectors[, tmp]

  W <- sqrt(solve(diag(EValsum))) %*% t(EVecsum);

  SA <- W %*% RA %*% t(W)
  SB <- W %*% RB %*% t(W)

  BD <- geigen(SA, SB)
  tmp <- order(BD$values)
  B <- BD$vectors[, tmp]

  t(B) %*% W
}