# Question 2 Scratchwork 
# Y ~ Poisson(lambda)  (data distribution)
# lambda ~ Gamma(a,b) where a = shape, b = rate

pois_gamma <- function(Y, a = 1, b = 1, title = "Prior and Posterior Distribution")
{
  # pois_gamma() - returns a list of posterior mean and variance values where 
  # the prior distribution follows Beta and the posterior is Binomial
  
  # Arguments:
  # Y = observations of random variable
  # a  = shape parameter for Gamma distribution 
  # b = rate parameter for Gamma distribution 
  
  lambda <- seq(0,1,length=10000)    # values of theta following beta distribution
  
  # only plotting one prior 
  prior = dgamma(lambda, a, b)
  posterior = dgamma(lambda, Y + a, Y + b)
  
  # plot the actual priors  
  label = c("Prior", "Posterior")
  plot.new()
  plot(lambda,prior,
       type="l",
       col = 4,
       xlab=expression(lambda),
       ylab="Prior", 
       ylim = c(0, max(posterior)),
       xlim = c(0, 1.1)
  )
  lines(lambda,prior,col=1)
  lines(lambda, posterior, col = 2)
  title(main = title)
  legend("topright",label,col=c(1,2), lty=1, cex=.9)
  
  # compute posterior mean and posterior standard deviation
  posterior_mean <- (Y + a) / (b + 1)
  posterior_var <- a / b^2
  posterior_sd <- sqrt(posterior_var)
 
  # return posterior mean and variance 
  mylist <- list("posterior mean" = posterior_mean, "posterior standard deviation" = posterior_sd)
  return(mylist)
}


# (e) Y = 11 use default uninformative and then use the informative prior

# uninformative prior
pois_gamma(Y = 11)

# informative prior 
pois_gamma(Y = 11, a = 15.81139, b = 3.152278)

# (f) summarize results 
# The results above show that the informative prior distribution is a bit more accurate than 
# using the uninformative prior. 




















































































