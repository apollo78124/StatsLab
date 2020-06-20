install.packages("data.table")
library("data.table")
bitcoin <- read.csv("./R/bitcoin.csv") 

confidenceIntervals <- function(rawData, range) {
  cleanData <-ifelse(rawData<3000, rawData, 0)
  x <- cleanData[range]
  plot(range, x, type='o')
  mu = mean(x)
  s = sd(x)
  n = length(x)
  cat("mean =", round(mu, digits=2), " sd =", round(s, digits=2), '\n')
  xg <- seq(mu-3*s/sqrt(n), mu+3*s/sqrt(n), by=0.06*s/sqrt(n))
  zg <- (xg-mu)/s*sqrt(n)
  
  En <- qnorm(0.975)*s/sqrt(n)
  cat("Normal distribution: E=", En, ", ", mu-En, "<mu<", mu+En, '\n')
  
  Et <-qt(0.975, df=n-1)*s/sqrt(n)
  cat("t-distribution: E=", Et, ", ", mu-Et, "<mu<", mu+Et, '\n')
  
  plot(xg, dnorm(zg), type="l", xlab="x", ylab="Prob. density")
  points(xg, dt(zg, df=n-1), type="l", col="green")
}

confidenceIntervals(bitcoin$Close, 1:1300)
confidenceIntervals(bitcoin$Close, 200:300)
confidenceIntervals(bitcoin$Close, 500:1300)
confidenceIntervals(bitcoin$Close, 1200:1300)
confidenceIntervals(bitcoin$Close, 1295:1300)