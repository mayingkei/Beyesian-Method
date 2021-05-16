set.seed(10)
#generate f(x)
m <- 5000 #sample
i <- 0 #counter

M <- 3^(-1/2) * exp(-3) / ((1 - pgamma(3, shape = 1/2, rate = 1)) * gamma(1/2))
fg_ratio <- function(y){(1/(((1 - pgamma(3, shape = 1/2, rate = 1))) * gamma(1/2))) * y^(-1/2) * exp(-y + (y-3))}

q2_result <- NULL

while(i < m){
  y <- rexp(1, 1) + 3 #lambda = 1, shift = 3
  u <- runif(1)
  if(u <= fg_ratio(y)/M){
    q2_result <- c(q2_result,y)
  }
  i <- i + 1
}

#method 1: using result of q2
estimate_1 = sum(cos(q2_result) * ((1 - pgamma(3, shape = 1/2, rate = 1)) * gamma(1/2))) / m
print(paste("estimate of method 1: ", estimate_1))

#method 2: importance sampling
g <- rexp(m,1) + 3
estimate_2 = sum(cos(g) * ((1 - pgamma(3, shape = 1/2, rate = 1)) * gamma(1/2)) * fg_ratio(g)) / m
print(paste("estimate of method 2: ", estimate_2))