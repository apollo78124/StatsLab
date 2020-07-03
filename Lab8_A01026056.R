# Confidence interval for population mean
xb0 = 21.3 # Sample mean
s0 = 0.7 # Sample s.d. Assume s0 is approximately equal to sigma
n = 4 # sample size (number per sample)
alpha = 0.05 # significance level
ns = 1000 # number of samples to be generated
population = rnorm(3000,mean=xb0,sd=s0) # generate normally distributed random numbers
hist(population,breaks=30)

s=rep(0,ns) # initialize xbar's
x=rep(0,ns)
for(i in 1:ns) s[i] = sd( sample(population, n) ) # Repeat ns times
hist(s,breaks=30) # Display sampling distribution

#histogram for chi square
for(i in 1:ns) 
  x[i] = (n-1)*s[i]^2 / 0.7
  # Repeat ns times
hist(x,breaks=30)

x2l = qchisq(alpha/2, n-1)
x2R = qchisq(1 - alpha/2, n-1)
cat("Left chi square: ", x2l, " Right chi square: ", x2R)
leftBound = mean(s) * sqrt(n-1/x2l)
rightBound = mean(s) * sqrt(n-1/x2R)
cat("Theoretical using chi square ", leftBound, " < mu < ", rightBound)


left = round(ns*alpha/2); right = round(ns*(1-alpha/2))
s.sorted = sort(s,method="quick")
cat("Simulation: ", s.sorted[left], " < mu < ", s.sorted[right])

#Theoretical result for comparison
E = qnorm(1-alpha/2)*s0/sqrt(n) # Margin of error
cat("Theoretical: ", xb0-E, " < mu < ", xb0+E)