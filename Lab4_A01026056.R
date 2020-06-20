#Binomial ditribution
n <- 100 #number of trials (e.g. coin tosses) in an experiment (sample)
p <- 0.9 # probability of getting success (head) in a single trial (coin toss)
ns <- 1000 #number of experiments (sample size)
x <- rep(0,len=ns) # each component is the number of successes in a single experiment. Fill x vector from 1 to 1000 as 0. 
for(i in 1:ns){
  x[i] <- sum( ifelse(runif(n) < p, 1, 0) )
}
rf <- table(x)/ns
plot(rf, ylim=c(0, 1.3*max(rf)))
xlabels <- 0:ns
Ptheory <- choose(n,xlabels)*p^(xlabels)*(1-p)^(n-xlabels)
points(xlabels, Ptheory)

c(mean(x), n*p)
c(sd(x), sqrt(n*p*(1-p)) )

mu <- 2
n <- 100#number of trials
p <- mu/n
ns <- 1000 #number of experiments(sample size)
x <- rep(0,len=ns)
for(i in 1:ns){
  x[i] <- sum( ifelse(runif(n) < p, 1, 0) )
}
pex =table(x)/ns
plot(pex, ylim=c(0,1.3*max(pex)))
xtheory <- 0:ns
ptheory <- mu^xtheory*exp(-mu)/factorial(xtheory)
points(xtheory,ptheory)

c(mean(x), mu)
c(sd(x), sqrt(mu) )