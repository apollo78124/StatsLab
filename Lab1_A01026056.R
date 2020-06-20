beaver1 <- read.csv("~/Rstudio/beaver.csv")
hist(beaver1$temp)
hist(beaver1$temp, breaks=20)
hist(beaver1$temp, main="Beaver 1 Histogram David Lee", xlab="Temperature", ylab="Frequency", breaks=50, border=blues9)

mean(beaver1$temp)
median(beaver1$temp)
sd(beaver1$temp)
boxplot(beaver1$temp, main="Beaver 1 Box Plot David Lee")
boxplot(beaver1$temp, main="Beaver 1 Box Plot David Lee", xlab = "Frequency",
        ylab = "Temperature",
        col = "orange",
        border = "blue",
        notch = TRUE)
