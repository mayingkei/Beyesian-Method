
  f <- function(x){x^3+3.6*x^2+0.8*x-7.12}
  #checked f(-2) < 0 and f(2) >0, setup initial a and b
  a = f(-2)
  b = f(2)
  m = (a+b)/2
  y1 = f(a)
  y2 = f(b)
  y3 = f(m)
  
  #iteration
  while(abs(y3) > 0.0000001){
    y1 = f(a)
    y2 = f(b)
    y3 = f(m)
    
    if(y1*y3 > 0)
      a = m
    else
      b = m
    
    m = (a+b)/2
  }
  print(paste("Final Answer:", m))




  data = read.table("PoisRegData.txt", header = TRUE, sep = "", dec = ".")
  x <- data$x
  y <- data$y
  alpha = 1
  beta = 1
  gamma = 1
  i = 0
  
  repeat{
    i = i+1
    
    #Setup the function and find the error
    da = sum(-exp(alpha+beta*x+gamma*x^2)+y)
    db = sum(-exp(alpha+beta*x+gamma*x^2)*x+x*y)
    dc = sum(-exp(alpha+beta*x+gamma*x^2)*x^2+(x^2)*y)
    da2 = sum(-exp(alpha+beta*x+gamma*x^2))
    db2 = sum(-exp(alpha+beta*x+gamma*x^2)*x^2)
    dc2 = sum(-exp(alpha+beta*x+gamma*x^2)*x^4)
    dadb = sum(-exp(alpha+beta*x+gamma*x^2)*x)
    dadc = sum(-exp(alpha+beta*x+gamma*x^2)*x^2)
    dbdc = sum(-exp(alpha+beta*x+gamma*x^2)*x^3)
    
    r1 = c(da2,dadb,dadc)
    r2 = c(dadb,db2,dbdc)
    r3 = c(dadc,dbdc,dc2)
    
    q = rbind(r1,r2,r3)
    m = solve(q)
    c = c(da,db,dc)
    v0 = c(alpha,beta,gamma)
    v1 = v0-(m%*%c)
    e = abs(v1-v0)
    
    # print(paste("Time of Iteration:", i))
    # print("Current Solution")
    # print(v1)
    
    
    if(isTRUE(e[1] < 0.0000001) & isTRUE(e[2] < 0.0000001) & isTRUE(e[3] < 0.0000001)){
      break
    }
    else{
      alpha = v1[1]
      beta = v1[2]
      gamma = v1[3]
    }
  }
  print("Done, Final Answer is:")
  print(v1)

  
  data = read.table("LogitRegData.txt", header = TRUE, sep = "", dec = ".")
  x <- data$x
  y <- data$y
  alpha = 1
  beta = 1
  i = 0
  
  repeat{
    i = i+1
    
    #Setup the function and find the error
    da = sum(y - exp(alpha+beta*x)/(1+exp(alpha+beta*x)))
    db = sum(x*y - exp(alpha+beta*x)*x/(1+exp(alpha+beta*x)))
    da2 = sum(-exp(alpha+beta*x)/(1+exp(alpha+beta*x))^2)
    db2 = sum(-exp(alpha+beta*x)*x^2/(1+exp(alpha+beta*x))^2)
    dadb = sum(-exp(alpha+beta*x)*x/(1+exp(alpha+beta*x))^2)

    r1 = c(da2,dadb)
    r2 = c(dadb,db2)

    q = rbind(r1,r2)
    m = solve(q)
    c = c(da,db)
    v0 = c(alpha, beta)
    v1 = v0 - (m%*%c)
    e = abs(v1-v0)

    # print(paste("Time of Iteration:", i))
    # print("Current Solution")
    # print(v1)
    
    #Stopping Rule
    if(isTRUE(e[1] < 0.000001) & isTRUE(e[2] < 0.000001)){
      break
    }
    else{
      alpha = v1[1]
      beta = v1[2]
    }
  }
  print("Done, Final Answer is:")
  print(v1)


