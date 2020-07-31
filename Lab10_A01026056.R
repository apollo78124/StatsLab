salary <- read.csv("~/salary.csv")
y = salary$year
s = salary$salary
plot(y, s)
plot(y, log(s))
ls = log(s)

smean = mean(s)
ymean = mean(y)

n = length(s)
temp = 0
for(i in 1:n) {
  temp =+ (s[i]-smean)^2
}
ss = sqrt(temp / n-1)

temp = 0
for(i in 1:n) {
  temp =+ (y[i]-ymean)^2
}
sy = sqrt(temp / n-1)

r=cov(y,s)/sy*ss
cat("Correlation computed using formula: ", r)
cov(y, s, use = "everything",
    method = c("pearson", "kendall", "spearman"))

cor(y, s, use = "everything",
    method = c("pearson", "kendall", "spearman"))
cat("Correlation computed using R function: ", r)

b1 = cov(y,s)/(sd(y))^2
b0 = smean - b1*ymean
ls = b1*y + b0
points(ls)

exp(ls)
