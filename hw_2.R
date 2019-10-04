# Q. 1b(i)

set.seed(7)
U = function(x){dunif(x,0,10)}
x <- seq(-5,15,0.001)
plot(x, U(x), type = 'b', col = 'dark red', main = 'PDF of a Uniform Distribution')


# Q.1b(ii)

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


