# Example of the Poisson-Gamma model that 
# was covered in class

# Goals scored by men's olympic hockey team
men <- c(5, 6, 5, 2, 6, 2)

# Goals scored by women's olympic hockey team
women <- c(5, 5, 5, 6, 5, 5, 2)

# First visualize the poisson pmf for theta = 4
y_vals <- 0:20
theta <- 4
plot(y_vals, dpois(y_vals, theta), type='h')

# Next visaulize the likelihood for Y=10 
# (the Poisson pmf as a functino of theta)
theta_vals <- seq(0,30, length=1001)
y <- 10
plot(theta_vals, dpois(y, theta_vals), type='l')

# Now plot the poisson likelihood for men's hockey data
like_func <- function(x){prod(dpois(men, x))} 
# Note that the "prod" function is used because
# we are assuming iid observations

like_vals <- sapply(theta_vals, like_func)

plot(theta_vals, like_vals, type='l')

# Next visualize the gamma prior
# plot the gamma prior for 
# a in {1/2, 1, 10} - shape
# b in {1/2, 1, 10} - scale

par(mfrow=c(1,3))
plot(theta_vals, dgamma(theta_vals, shape=1/2, scale=1/2), type='l')
lines(theta_vals, dgamma(theta_vals, shape=1/2, scale=1), type='l', col='red')
lines(theta_vals, dgamma(theta_vals, shape=1/2, scale=10), type='l', col='blue')

plot(theta_vals, dgamma(theta_vals, shape=1, scale=1/2), type='l')
lines(theta_vals, dgamma(theta_vals, shape=1, scale=1), type='l', col='red')
lines(theta_vals, dgamma(theta_vals, shape=1, scale=10), type='l', col='blue')

plot(theta_vals, dgamma(theta_vals, shape=10, scale=1/2), type='l')
lines(theta_vals, dgamma(theta_vals, shape=10, scale=1), type='l', col='red')
lines(theta_vals, dgamma(theta_vals, shape=10, scale=10), type='l', col='blue')


# Now we will carry out the analysis of rate of per game goal
# scoring for the Men's hockey team

# Prior elicitation.  These values are motivated by prior guess is
# 4 goals per game and we have b = 5 games worth of prior information.
# This results in a = 5x4 
# this prior is quite diffuse
a <- 20
b <- 5

# plot this prior
plot(theta_vals, dgamma(theta_vals, shape=20, rate=5), type='l',
	xlab=expression(theta),
	ylab="prior density")

# Posterior inference.  We know with a Poisson data model
# and a gamma prior on the rate of the Poisson, the posterior
# is theta|y1, y2, y3, y4 ~ Gamma(shape=astar, rate=bstar)
# astar = a + sum(y)
# bstar = b + n
sumy <- sum(men)
n <- length(men)

astar <- sumy + a
bstar <- n + b

# Start with posterior mean (mean of gamma)
astar/bstar
# mean of 4.18 goals scored per game

# posterior variance (variance of gamma)
astar/bstar^2
# variance of 0.38 

# credible interval 
qgamma(c(0.025, 0.975), shape=astar, rate=bstar)
# 95% probability that theta is between (3,06, 5.47)

# Carry out prediction regarding how many goals men's hockey team
# will score in their next game.

# we can use Monte Carlo here to quickly predict.  We will do both
ppred <- function(ynew, astar, bstar){
  lout <- -lfactorial(ynew) + astar*log(bstar) - lgamma(astar) +
           lgamma(ynew + astar) - (ynew + astar)*log(1 + bstar)
  exp(lout)         
} 

plot(0:10, ppred(0:10, astar, bstar), type='h')
# use the mode as the prediction
c(0:10)[which.max(ppred(0:10, astar, bstar))] # predict they score four goals

# We can use Monte Carlo to approxmiate the posterior predictive
# first sample from the posterior
theta_vals <- rgamma(100000, shape=astar, rate=bstar)
# then sampel from the poisson likelihood given posterior samples
ppred_vals <- rpois(100000, theta_vals)

table(ppred_vals)

plot(table(ppred_vals)/100000, type='h')

# compare this to the theoretical distribution
lines(0:17+0.1, ppred(0:17, astar,bstar), type='h', col='red')


# Compare men olympic hockey team goal scoring rate
# to the women olympic hockey team goal scoaring rate
# We will use Monte Carlo to do that
# 
# First sample from the posterior distribution for men
th_men <- rgamma(10000, shape=astar, rate=bstar)

# Next sample from the posterior distribution of womens hockey
astar_w <- a + sum(women)
bstar_w <- b + length(women)

th_women <- rgamma(10000, shape=astar_w, rate=bstar_w)

# Plot the posterior distribution of theta_men - theta_women
hist(th_women - th_men)

# This is the Pr(theta_women > theta_mn | y_men, y_women)
mean(th_women > th_men)
# This is 0.608, women and men lots of overlap in the posterior
# but better than a coin flip probability of being correct if
# you select women as scoring more goals.






