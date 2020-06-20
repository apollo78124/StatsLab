beaver <- read.csv("~/Rstudio/beaver.csv")
#x <- beaver$temp
x = rnorm(1000)
r <- range(x)
xmin <- round(r[1], 1)-0.1
xmax <- round(r[2], 1)+0.1
#4. xmin is 36.2 xmax is 37.6
#N is a number of classes in the histogram
N<-200
step <- (xmax - xmin)/N
#7. 1 is included in the output when increment is changed to 0.3
boundaries <- seq(xmin, xmax, by=step)
xcut <- cut(x, breaks=boundaries, right=FALSE)
ft <- table(xcut)
plot(ft, type='h')
total = sum(ft)
prob <- function(i){ sum(ft[1:i])/total}
cp <- sapply(1:N, prob)
plot(boundaries[2:(N+1)], cp, type="l", main="Cumulative Probability: P(X < x)", xlab="Cumulative Temperature", ylab="Probability")