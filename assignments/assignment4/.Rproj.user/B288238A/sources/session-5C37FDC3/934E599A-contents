# Write function that takes Y, N, a, b, theta0 (hypothesized value of parameter) and 
# type (left, right, two sided) for the type of hypothesis testing to be conducted

beta_binom <- function(Y, N, a = 1, b = 1, prob = .95, 
                       title = "Prior and Posterior Distribution")
{
  # beta_binom() - returns a list of posterior mean and variance values where 
  # the prior distribution follows Beta and the posterior is Binomial
  
  # Arguments:
  # Y = number of success for Binomial
  # N = number of trials for Binomial
  # a, b are size and shape parameters for prior Beta distribution 
  # prob = probability to compute credible interval 
  
  theta <- seq(0,1,length=10000)    # values of theta following beta distribution
  
  # compute prior and posterior distribution using given parameters
  prior = dbeta(theta, a, b)
  posterior = dbeta(theta, Y + a, Y + b)
  
  # plot prior and posterior distributions 
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
  post_mode <- (a - 1) / (a + b - 2)
  post_median <- (a - (1/3)) / (a + b - (2/3))
  
  # compute credible interval tails 
  A <- Y + a
  B <- N - Y + b
  quantile.low<-(1-prob)/2# \alpha/2=(1-ci.level)/2
  quantile.up<-1-quantile.low
  Q05  <- qbeta(quantile.low, A, B)
  Q95  <- qbeta(quantile.up,A,B)
  output <- cbind(Q05, Q95)
  output <- round(output,3)
  
  # return posterior mean and variance 
  mylist <- list("posterior mean" = post_mean, "posterior standard deviation" = post_sd,
                 "posterior mode" = post_mode, "posterior median" = post_median, 
                 "endpoint" = output)
  return(mylist)
}