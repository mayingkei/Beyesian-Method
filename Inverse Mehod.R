set.seed(10)
m <- 5000 #sample
r <- 10 #success
p <- 0.6 #success probability
x <- 0:100 #where it is enough to include all density
pdf <- factorial(x+r-1) / (factorial(x) * factorial(r-1)) * (1-p)^r * p^x
cdf <- cumsum(pdf)

u <- runif(m)
q1_result <- NULL

for(i in 1:m){
  x_i = min(which(u[i] <= cdf)) - 1
  q1_result <- c(q1_result, x_i)
}

hist(q1_result, breaks = seq(-0.5, 50, by = 1), freq = FALSE)