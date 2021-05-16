set.seed(10)
data <- read.table("lifetime_data.txt", header = TRUE, sep = "", dec = ".")
m <- 10000 #data size

#four strata
s1 <- data[1:4000,] #female non-smoker
s2 <- data[4001:6000,] #male non-smoker
s3 <- data[6001:6500,] #female smoker
s4 <- data[6501:10000,] #male smoker

#population abundance of each strata
u1 = nrow(s1)/m
u2 = nrow(s2)/m
u3 = nrow(s3)/m
u4 = nrow(s4)/m

#Task 1
p <- 100 #pilot sample

#draw samples in proportion to population abundance
x1 = s1[sample(nrow(s1), u1 * p),]
x2 = s2[sample(nrow(s2), u2 * p),]
x3 = s3[sample(nrow(s3), u3 * p),]
x4 = s4[sample(nrow(s4), u4 * p),]

#find sample sd of each strata
sd1 = sd(x1$lifetime)
sd2 = sd(x2$lifetime)
sd3 = sd(x3$lifetime)
sd4 = sd(x4$lifetime)

print("Task 1")
print(paste("SD of Female Non-Smoker: ", sd1))
print(paste("SD of Male Non-Smoker: ", sd2))
print(paste("SD of Female Smoker: ", sd3))
print(paste("SD of Male Smoker: ", sd4))

#Task 2
n <- 1000 #sample size

#find sample number of each strata
n1 = n * u1
n2 = n * u2
n3 = n * u3
n4 = n * u4

print("Task 2")
print(paste("sample number of Female Non-Smoker: ", n1))
print(paste("sample number of Male Non-Smoker: ", n2))
print(paste("sample number of Female Smoker: ", n3))
print(paste("sample number of Male Smoker: ", n4))

#Task 3
#draw samples according to sample number in Task 2
y1 = s1[sample(nrow(s1), n1),]
y2 = s2[sample(nrow(s2), n2),]
y3 = s3[sample(nrow(s3), n3),]
y4 = s4[sample(nrow(s4), n4),]

#find sample mean of each strata
h1 = sum(y1$lifetime)/n1
h2 = sum(y2$lifetime)/n2
h3 = sum(y3$lifetime)/n3
h4 = sum(y4$lifetime)/n4

#find sample mean of whole population by weighting
res = u1*h1 + u2*h2 + u3*h3 + u4*h4

print("Task 3")
print(paste("Population mean: ", sum(data$lifetime)/m))
print(paste("Estimated mean: ", res))