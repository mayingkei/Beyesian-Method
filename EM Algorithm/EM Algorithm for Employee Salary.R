  data = read.table("SalaryData.txt", header = TRUE, sep = "", dec = ".")
  x <- data$x
  p1 = 0.6
  p2 = 0.2
  u1 = 5000
  u2 = 10000
  u3 = 20000
  var1 = 1250
  var2 = 2500
  var3 = 5000
  i = 0
  
  repeat{
    i = i+1
    flag1 = FALSE
    flag2 = FALSE
    flag3 = FALSE
    
    #E-Step
    a = p1*(1/sqrt(2*pi*var1))*exp(-(x-u1)^2/(2*var1))
    b = p2*(1/sqrt(2*pi*var2))*exp(-(x-u2)^2/(2*var2))
    c = (1-p1-p2)*(1/sqrt(2*pi*var3))*exp(-(x-u3)^2/(2*var3))
    
    
    z1 = a/(a+b+c)
    z2 = b/(a+b+c)
    z3 = c/(a+b+c)
    
    z1[is.na(z1)] <- 0
    z2[is.na(z2)] <- 0
    z3[is.na(z3)] <- 0
    
    #M-Step
    new_u1 = sum(z1*x)/sum(z1)
    new_u2 = sum(z2*x)/sum(z2)
    new_u3 = sum(z3*x)/sum(z3)
    
    new_p1 = sum(z1)/8000
    new_p2 = sum(z2)/8000
    
    new_var1 = sum(z1*(x-u1)^2)/sum(z1)
    new_var2 = sum(z2*(x-u2)^2)/sum(z2)
    new_var3 = sum(z3*(x-u3)^2)/sum(z3)
    
    # print(paste("Time of Iteration:", i))
    
    #Stopping Rule
    if(isTRUE(abs(new_u1-u1) < 0.000001) & isTRUE(abs(new_u2-u2) < 0.000001) & isTRUE(abs(new_u3-u3) < 0.000001))
      flag1 = TRUE
    if(isTRUE(abs(new_p1-p1) < 0.000001) & isTRUE(abs(new_p2-p2) < 0.000001))
      flag2 = TRUE
    if(isTRUE(abs(new_var1-var1) < 0.000001) & isTRUE(abs(new_var2-var2) < 0.000001) & isTRUE(abs(new_var3-var3) < 0.000001))
      flag3 = TRUE
    
    if(flag1 & flag2 & flag3)
      break
    
    #Iteration Replacement
    else{
      u1 = new_u1
      u2 = new_u2
      u3 = new_u3
      p1 = new_p1
      p2 = new_p2
      var1 = new_var1
      var2 = new_var2
      var3 = new_var3
    }
  }
  
  p = c(p1, p2)
  u = c(u1, u2, u3)
  var = c(var1, var2, var3)
  print("Done, Final Answer is: ")
  print("[p1,p2]: ")
  print(p)
  print("[u1,u2,u3]: ")
  print(u)
  print("[var1,var2,var3]: ")
  print(var)
  
  #first 50 employees salary level
  z1[z1>0.99] <- 1
  z1[z1 < 0.01] <- 0
  z2[z2>0.99] <- 2
  z2[z2< 0.01] <- 0
  z3[z3>0.99] <- 3
  z3[z3 < 0.01] <- 0
  z = z1+z2+z3
  
  print("first 50 employees' salary level are: ")
  print(z)
