set.seed(1)
data = read.table("dataset1.txt", header = TRUE, sep = " ", dec = ".")

N <- 10000 #iteration number

#initialize parameters
p <- matrix(NA, N, 3)
u <- matrix(NA, N, 6)
z <- matrix(NA, N, 100)

p[1, ] <- c(0.7, 0.1, 0.2)
u[1, ] <- rep(3, 6)
z[1, ] <- sample(1:3, 100, prob = p[1, ], replace = TRUE)

#function to sample from Dirichlet: copy from tutorial 7
rDirichlet <- function(alpha_vec){
  num <- length(alpha_vec)
  temp <- NULL
  for(i in 1:num){
    temp <- c(temp, rgamma(1, shape = alpha_vec[i], rate = 1))
  }
  return(temp/sum(temp))
}  

for(i in 2:N){
  #update pi 
  n1 <- sum(z[i-1, ] == 1)
  n2 <- sum(z[i-1, ] == 2)
  n3 <- sum(z[i-1, ] == 3)
  p[i, ] <-  rDirichlet(c(2 + n1, 2 + n2, 2 + n3))
  
  #update u
  x1 <- 0
  x2 <- 0
  y1 <- 0
  y2 <- 0
  z1 <- 0
  z2 <- 0
  
  for(t in 1:100){
    if(z[i-1, t] == 1){
      x1 = x1 + data[1, t]
      x2 = x2 + data[2, t]
    }
    if(z[i-1, t] == 2){
      y1 = y1 + data[1, t]
      y2 = y2 + data[2, t]
    }
    if(z[i-1, t] == 3){
      z1 = z1 + data[1, t]
      z2 = z2 + data[2, t]
    }
  }
  
  w <- c(x1/n1, x2/n1, y1/n2, y2/n2, z1/n3, z2/n3)
  v <- c(1/n1,1/n1,1/n2,1/n2,1/n3,1/n3)
  
  #generate ui from normal
  u[i, ] <- rnorm(6, w, v)
  
  #update z
  for (j in 1:100){
    k1 = p[i,1] * prod(exp(-0.5 * (data[1, j] - u[i, 1])^2)) * prod(exp(-0.5 * (data[2, j] - u[i, 2])^2))
    k2 = p[i,2] * prod(exp(-0.5 * (data[1, j] - u[i, 3])^2)) * prod(exp(-0.5 * (data[2, j] - u[i, 4])^2))
    k3 = p[i,3] * prod(exp(-0.5 * (data[1, j] - u[i, 5])^2)) * prod(exp(-0.5 * (data[2, j] - u[i, 6])^2))
    
    #find probability of each (zi = k)
    p1 = k1/(k1+k2+k3)
    p2 = k2/(k1+k2+k3)
    p3 = k3/(k1+k2+k3)
    
    m <- c(p1,p2,p3)
    #sample zi by probability obtained above
    z[i, j] <- sample(1:3, 1, prob = m, replace = TRUE)
  }
  
}
B <- 2000

#posterior mean
p1 <- mean(p[(B+1):N,1])
p2 <- mean(p[(B+1):N,2])
p3 <- mean(p[(B+1):N,3])

#posterior mean
u11 <- mean(u[(B+1):N,1])
u21 <- mean(u[(B+1):N,2])
u12 <- mean(u[(B+1):N,3])
u22 <- mean(u[(B+1):N,4])
u13 <- mean(u[(B+1):N,5])
u23 <- mean(u[(B+1):N,6])


#posterior modes 
Z <- c(NA, 100) #store all mode in big Z
for(j in 1:100){
  temp <- table(z[(B+1):N, j])
  ind <- which.max(temp)
  Z[j] <- names(temp)[ind]
}

#Result
print("Posterior mean of pi: ")
print(c(p1, p2, p3))
print("Posterior mean of u(u11, u21, u12, u22, u13, u23): ")
print(c(u11, u21, u12, u22, u13,u23))
print("Posterior mode of Z: ")
print(Z)

#Plot
par(mfrow = c(3,1))

plot(p[ ,1], type = "l")
plot(u[ ,1], type = "l")
plot(z[ ,1], type = "l")
