data = read.table("dataset2.txt", header = TRUE, sep = " ", dec = ".")

N <- 10000 #iteration number
n <- 1500 #sample size which is T in question

#initialize parameters
u <- matrix(NA, N, 3)
k <- rep(NA, N)
l <- rep(NA, N)

u[1, ] <- c(0, 0, 0)
k[1] <- 100
l[1] <- 300

for(i in 2:N){
  #update u
  u[i, 1] <- rnorm(1, 1/(k[i-1] + 1) * (sum(data[1:k[i-1], 1])), 1/(k[i-1] + 1))
  u[i, 2] <- rnorm(1, 1/(l[i-1] - k[i-1] + 1) * (sum(data[(k[i-1]+1):l[i-1], 1])), 1/(l[i-1] - k[i-1] + 1))
  u[i, 3] <- rnorm(1, 1/(n - l[i-1] + 1) * (sum(data[(l[i-1]+1):n, 1])), 1/(n - l[i-1] + 1))
  
  #update k by MH
  if(k[i-1] == 2){
    y = k[i-1] + 1
    p <- min(( exp(-0.5*( sum((data[1:y,1]-u[i, 1])^2)) + sum((data[(y+1):l[i-1],1]-u[i,2])^2) )*1/2 )
             / ( exp(-0.5*( sum((data[1:k[i-1],1]-u[i, 1])^2)) + sum((data[(k[i-1]+1):l[i-1],1]-u[i,2])^2) )*1 ), 1)
  }
  else if(k[i-1] == (l[i-1] - 1)){
    y = k[i-1] - 1
    p <- min(( exp(-0.5*( sum((data[1:y,1]-u[i, 1])^2)) + sum((data[(y+1):l[i-1],1]-u[i,2])^2) )*1/2 )
             / ( exp(-0.5*( sum((data[1:k[i-1],1]-u[i, 1])^2)) + sum((data[(k[i-1]+1):l[i-1],1]-u[i,2])^2) )*1 ), 1)
  }
  else if(k[i-1] == 3){
    y = k[i-1] + sample(c(-1,1),1)
    if(y == 2){
      p <- min(( exp(-0.5*( sum((data[1:y,1]-u[i, 1])^2)) + sum((data[(y+1):l[i-1],1]-u[i,2])^2) )*1 )
               / ( exp(-0.5*( sum((data[1:k[i-1],1]-u[i, 1])^2)) + sum((data[(k[i-1]+1):l[i-1],1]-u[i,2])^2) )*1/2 ), 1)
    }
  }
  else if(k[i-1] == (l[i-1] - 2)){
    y = k[i-1] + sample(c(-1,1),1)
    if(y == (l[i-1] - 1)){
      p <- min(( exp(-0.5*( sum((data[1:y,1]-u[i, 1])^2)) + sum((data[(y+1):l[i-1],1]-u[i,2])^2) )*1 )
               / ( exp(-0.5*( sum((data[1:k[i-1],1]-u[i, 1])^2)) + sum((data[(k[i-1]+1):l[i-1],1]-u[i,2])^2) )*1/2 ), 1)
    }
  }
  else{
    y = k[i-1] + sample(c(-1,1),1)
    p = min(( exp(-0.5*( sum((data[1:y,1]-u[i, 1])^2)) + sum((data[(y+1):l[i-1],1]-u[i,2])^2) ) )
            / ( exp(-0.5*( sum((data[1:k[i-1],1]-u[i, 1])^2)) + sum((data[(k[i-1]+1):l[i-1],1]-u[i,2])^2) ) ), 1)
  }
  
  if(is.nan(p)){
    p = 1
  }
  
  
  if(runif(1) < p){
    k[i] <- y
  }
  else{
    k[i] <- k[i-1]
  }
  
  p <- 0
  y <- 0
  
  #update l by MH
  if(l[i-1] == k[i] + 1){
    y = l[i-1] + 1
    p <- min(( exp(-0.5*( sum((data[(k[i]+1):y,1]-u[i, 2])^2)) + sum((data[(y+1):n,1]-u[i,3])^2) )*1/2 )
             / ( exp(-0.5*( sum((data[(k[i]+1):l[i-1],1]-u[i, 2])^2)) + sum((data[(l[i-1]+1):n,1]-u[i,3])^2) )*1 ), 1)
  }
  else if(l[i-1] == (n - 1)){
    y = l[i-1] - 1
    p <- min(( exp(-0.5*( sum((data[(k[i]+1):y,1]-u[i, 2])^2)) + sum((data[(y+1):n,1]-u[i,3])^2) )*1/2 )
             / ( exp(-0.5*( sum((data[(k[i]+1):l[i-1],1]-u[i, 2])^2)) + sum((data[(l[i-1]+1):n,1]-u[i,3])^2) )*1 ), 1)
  }
  else if(l[i-1] == k[i] + 2){
    y = l[i-1] + sample(c(-1,1),1)
    if(y == k[i] + 1){
      p <- min(( exp(-0.5*( sum((data[(k[i]+1):y,1]-u[i, 2])^2)) + sum((data[(y+1):n,1]-u[i,3])^2) )*1 )
               / ( exp(-0.5*( sum((data[(k[i]+1):l[i-1],1]-u[i, 2])^2)) + sum((data[(l[i-1]+1):n,1]-u[i,3])^2) )*1/2 ), 1)
    }
  }
  else if(l[i-1] == (n - 2)){
    y = l[i-1] + sample(c(-1,1),1)
    if(y == (n-1)){
      p <- min(( exp(-0.5*( sum((data[(k[i]+1):y,1]-u[i, 2])^2)) + sum((data[(y+1):n,1]-u[i,3])^2) )*1 )
               / ( exp(-0.5*( sum((data[(k[i]+1):l[i-1],1]-u[i, 2])^2)) + sum((data[(l[i-1]+1):n,1]-u[i,3])^2) )*1/2 ), 1)
    }
  }
  else{
    y = l[i-1] + sample(c(-1,1),1)
    p = min(( exp(-0.5*( sum((data[(k[i]+1):y,1]-u[i, 2])^2)) + sum((data[(y+1):n,1]-u[i,3])^2) ) )
            / ( exp(-0.5*( sum((data[(k[i]+1):l[i-1],1]-u[i, 2])^2)) + sum((data[(l[i-1]+1):n,1]-u[i,3])^2) ) ), 1)
  }
  
  if(is.nan(p)){
    p = 1
  }
  
  if(runif(1) < p){
    l[i] <- y
  }
  else{
    l[i] <- l[i-1]
  }
}

B <- 2000
#posterior mean
u1 <- mean(u[(B+1):N,1])
u2 <- mean(u[(B+1):N,2])
u3 <- mean(u[(B+1):N,3])

#posterior mode
temp <- table(k[(B+1):N])
ind <- which.max(temp)
K <- names(temp)[ind]

temp <- table(l[(B+1):N])
ind <- which.max(temp)
L <- names(temp)[ind]

print("Posterior mean of u: ")
print(c(u1, u2, u3))
print("Posterior mode of k and l: ")
print(c(K, L))

par(mfrow = c(3,1))

plot(u[ ,1], type = "l")
plot(u[ ,2], type = "l")
plot(u[ ,3], type = "l")
