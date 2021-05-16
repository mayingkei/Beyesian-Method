set.seed(10)
m <- 5000 #sample
i <- 0 #counter

M <- 3^(-1/2) * exp(-3) / ((1 - pgamma(3, shape = 1/2, rate = 1)) * gamma(1/2))
fg_ratio <- function(y){(1 / (((1 - pgamma(3, shape = 1/2, rate = 1))) * gamma(1/2))) * y^(-1/2) * exp(-y + (y-3))}

q2_result <- NULL

while(i < m){
  y <- rexp(1, 1) + 3 #lamda = 1, shift = 3
  u <- runif(1)
  if(u <= fg_ratio(y)/M){
    q2_result <- c(q2_result,y)
  }
  i <- i + 1
}

print(paste("Theoretical Rate: ",1/M))
print(paste("Actual Rate: ",length(q2_result)/m))
hist(q2_result,breaks = 20, freq = FALSE)