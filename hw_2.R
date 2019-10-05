# Q (1b)
set.seed(7)
uni = function(x){dunif(x,0,10)}
x <- seq(-5,15,0.001)
x
plot(x, uni(x), typ = 'b', col = 'dark red')

# Q.1b

cdf = function(x){punif(x,0,10)}
plot(x,cdf(x),type='l', col = 'dark green', lwd = 3)

# Q.1c (i)

m = runif(50,0,10)
hist(m, probability = T, breaks = 20)
curve(dunif(x,0,10), col= 2, lwd=2, add=TRUE)


# Q.1c (ii)
plot(ecdf(m))
curve(punif(x,0,10), col= 2, lwd=2, add=TRUE)


# Q.1d 
j = runif(1000,0,10)
hist(j, probability = T, breaks = 15)
curve(dunif(x,0,10), col= 'blue', lwd=2, add=TRUE)

# Q.1d (ii)
plot(ecdf(j))
curve(punif(x,0,10), col= 2, lwd=2, add=TRUE)

# Q. 2d(i)

set.seed(7001)
#generic //my.samples = matrix(runif(n*r,min,max),r)
# r = Number of random samples; n = sample size; min = a and max = b for a uniform distribution.
n <- 30
r <- 1000
sample.space = matrix(runif(n*r,0,10),r)

theta1 = 2*(apply(sample.space,1,mean)) #compute the means of each row

# l = mean(sample.space[1,])
# l

theta2 = apply(sample.space,1,max) #Compute the max of each row

theta3 = ((n+1)/n)*(apply(sample.space,1,max)) #Compute the max of each row


# Q. 2d(ii)
set.seed(7001)

par(mfcol = c(2,2)) #2 rows by 2 col output space

plot(ecdf(theta1), lwd = 2)
curve(pnorm(x,10,1.054093), 
      add=TRUE,
      col="red")

plot(ecdf(theta2), lwd = 2)
curve((x^30)/(10^30), col="red", lwd=2, add=TRUE)

plot(ecdf(theta3), lwd = 2)
curve(((x^30)/(10^30))*((30/31)^30), col="red", lwd=2, add=TRUE)


# Q. 2d(iii)

bias1 = mean(theta1) - 10 #from bias  = E(theta) - theta, where theta = 10 in U(0,10)

var1 <- var(theta1) #Computes the variance of theta 1

MSE1 = var1 + (bias1^2)

bias2 = mean(theta2) - 10

var2 = var(theta2) #Compute the variance of theta 2

MSE2 = var2+(bias2^2)

bias3 = mean(theta3) - 10

var3 = var(theta3) #Compute the variance of each row

MSE3 = var3+(bias3^2)

MSE1 > MSE2 #i.e., MSE2 is less than MSE1

MSE1 > MSE3 #i.e., MSE3 is also less than MSE1

MSE2 > MSE3 #MSE3 is less than both MSE1 and MSE2

