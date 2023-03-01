#==============================================================================
# H0: lambda <= 10
# H1: lambda > 10
#==============================================================================
# Data
Y <- 11

# Define one prior
a  <- c(0.5, 1, 10)
b  <- c(0.5, 1, 2) 
lab <- c("a=b=0.5", "a=b=1", "a=10, b=2")

# posterior parameters
A <- a+Y
B <- b+1

# parameter
lambda <-10

# compute priors 
prior.h0 <- pgamma(lambda, shape = a, rate = b)
prior.h1 <- 1 - prior.h0
prior.odds <- prior.h1 / prior.h0

# compute posteriors
posterior.h0 <- pgamma(lambda,shape=A,rate=B) 
posterior.h1 <- 1-posterior.h0
posterior.odds <- posterior.h1 / posterior.h0

# compute Bayes Factor
bayes.factor <- posterior.odds / prior.odds

# output results
output <- cbind(H0.prior=prior.h0,H0.post=posterior.h0,BF10=bayes.factor)

#==============================================================================
# H0: lambda = 10
# H1: lambda =/= 10
#==============================================================================
# Data
Y <- 11

# priors
a <- c(0.5, 1, 10)
b <- c(0.5, 1, 2)
lab <- c("a=b=0.5", "a=b=1", "a=10, b=2")

# posteriors
A <- a+Y
B <- b+1

# parameters and perturbation
lambda <- 10
epsilon <- 0.000001

# compute priors
prior.h0 <- pgamma(lambda+epsilon,shape=a,rate=b) - pgamma(lambda-epsilon,shape=a,rate=b)
prior.h1 <- 1-prior.h0
prior.odds <- prior.h1 / prior.h0

# compute posteriors
posterior.h0 <- pgamma(lambda+epsilon,shape=A,rate=B) - pgamma(lambda-epsilon,shape=A,rate=B) 
posteriror.h1 <- 1-posterior.h0
posterior.odds <- posteriror.h1 / posterior.h0

# compute bayes factor
bayes.factor <- posterior.odds / prior.odds

# output results
output <- cbind(H0.prior=prior.h0,H0.post=posterior.h0,BF10=bayes.factor)