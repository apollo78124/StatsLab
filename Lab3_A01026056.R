n <- 1:5000
P<-rep(0, times=length(n))
for(m in n){
  x <- sample(c(0,1), m, replace=TRUE)
  P[m] <- sum( x )/m
}
plot(n, P)
P[length(n)]

#dice
n <- 1:1000
P<-rep(0, times=length(n))
for(m in n){
  x <- sample(c(0,0,0,0,0,1), m, replace=TRUE)
  y <- sample(c(0,1), m, replace=TRUE)
  P[m] <- sum( x*y )/m
}
plot(n, P)
P[length(n)]