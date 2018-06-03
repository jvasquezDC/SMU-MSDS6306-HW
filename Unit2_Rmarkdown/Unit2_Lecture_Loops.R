#Variables
n <- 50
nsim <- 1000
lotsa.medians <- numeric(nsim)

#Loop of mean calc
for (i in 1:nsim) {
  x <- rnorm (n)
  lotsa.medians[i] <- median(x)
}

#Sanity checks
length(lotsa.medians)
summary(lotsa.medians)

#Plots
hist(lotsa.medians)
abline(v=0, col="red", lwd=2)