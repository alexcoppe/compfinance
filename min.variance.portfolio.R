#Given a weight (w) for the first asset X the next function calculates portfolio variance
#for a portfolio of 2 assets (X and Y, with X + Y = 1)
porfolio.variance <- function(w) {
  #Hardcoded covariance matrix
  cov.matrix <- matrix(c(2,0.8,0.8, 1.5), nrow=2)

  x <- matrix(c(w,1-w), ncol=1)
  x.prime <- t(x)
  x.prime %*% cov.matrix %*% x
}

#Vector of different wight for the first asset X
ws <- c(0.211, 0.542, 0.345, 0.213)

#Create variances for all different X weights 
variances <- sapply(ws, porfolio.variance)
#Create a data frame with variance and weight columns
variances.data.frame <- cbind(variances, ws)
#Order the data frame by variance
ordered.by.variance <- variances.data.frame[order(variances.data.frame[,"variances"]),]
#Min variance porfolio in the first row
ordered.by.variance
