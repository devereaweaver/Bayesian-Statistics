beta_binom <- function(Y, N, a = 1, b = 1, title = "Prior and Posterior Distribution")
{
  # beta_binom() - returns a list of posterior mean and variance values where 
  # the prior distribution follows Beta and the posterior is Binomial
  
  # Arguments:
  # Y = number of success for Binomial
  # N = number of trials for Binomial
  # a, b are size and shape parameters for prior Beta distribution 
  
  theta <- seq(0,1,length=10000)    # values of theta following beta distribution
  
  # only plotting one prior 
  prior = dbeta(theta, a, b)
  posterior = dbeta(theta, Y + a, Y + b)
  
  # plot the actual priors  
  label = c("Prior", "Posterior")
  plot.new()
  plot(theta,prior,
       type="l",
       col = 4,
       xlab=expression(theta),
       ylab="Prior", 
       ylim = c(0, max(posterior))
       )
  lines(theta,prior,col=1)
  lines(theta, posterior, col = 2)
  title(main = title)
  legend("topright",label,col=c(1,2), lty=1, cex=.9)
  
  # compute posterior mean and posterior standard deviation
  post_mean <- (Y + a) / (N + a + b)
  post_var <- ((Y + a)*(N - Y + b)) / ((a + N + b)^2 * (a + N + b + 1))
  post_sd <- sqrt(post_var)
  
  # return posterior mean and variance 
  mylist <- list("posterior mean" = post_mean, "posterior standard deviation" = post_sd)
  return(mylist)
}

# 6 (b)
# The values of a=1 and b=1 would be good parameters for the prior that are 
# uninformative and thus the default values are set to 1 for both parameters. 

# 6 (c) 
# The values for a and b that give prior mean 0.7 and prior standard deviation 0.2
# are approximately 2.98 and 1.28 respectively. 

# 6 (d) - conduct Bayesian analysis 
a <- beta_binom(20, 30, title = "Uninformative Prior")   # default uninformative 
b <- beta_binom(20, 30, 7, 3, title = "Informative Prior") # informative

# Using the informative distribution we can see the way in which the distribution 
# has shifted visually based upon seeing data. This differs from the frequentest
# analysis in that we were able to hopefully get a better understanding of the 
# data generating process by allowing us to update our knowledge on the process. 
# This also differs from classical in that we are now able to analyze the 
# difference in what we assumed to be true and what the data are telling us. 