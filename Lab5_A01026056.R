#normal distribution
x<-seq(-5,5, by=0.05)
density01 <- dnorm(x, mean=0, sd=1)
plot(x, density01, type="1")
densityA <- dnorm(x,mean=0, sd=2)
points(x, densityA, type="1", col="red")