library(GA)

# Import script from the library
# Constraint function (add as penalization to the target function)
# Given vector w set maximum, minimum and sum up to 1
constraint = function(w) {
  boundary_constr = (sum(w)-1)^2   # "sum x = 1" constraint
  
  for (i in 1:length(w)) {
    boundary_constr = boundary_constr +
      max(c(0,w[i]-1))^2 +  # "x <= 1" constraint
      max(c(0,-w[i]))^2     # "x >= 0" constraint
  }
  
  return (boundary_constr)
}

# Markowitz portfolio mean and variance
# Here we leave the possibility to set the
# lambda (risk adversion parameter).
# w vector of weights, m vector of returns,
# M varcov matrix, l lambda risk adversion
markowitz_ptlf = function(w,m,M,l) l*as.numeric(t(w)%*%M%*%(w)) - 
  (1-l)*as.numeric((w)%*%m) + 100*constraint(w)


prices = mib40_data$adjusted
log_returns = diff(log(prices))[-1]
log_ret_short = log_returns[,1:15]
log_ret_short = log_ret_short[complete.cases(log_ret_short)]
M = cov(log_ret_short)
m = colMeans(log_ret_short)
l = 0.5
# p = rep(1,15) # as.numeric(prices['2022-07-18',1:15])
# C = 1

ga_res = ga(
  # Tell the genetic algorithm that the 
  # weights are real variables
  type="real-valued", 
  
  # "ga" function performs maximization, so we must
  # multiply the objective function by -1
  function(w){-markowitz_ptlf(w, m = m, M = M, l = l)}, 
  
  # x_i >= 0
  lower = rep(0,ncol(log_ret_short)), 
  
  # x_i <= 1
  upper = rep(1,ncol(log_ret_short)), 
  
  # Maximum number of iterations 
  maxiter = 50000, 
  
  # If the maximum fitness remains the same for 50
  # consecutive transactions, stop the algorithm
  run=500, 
  popSize = 100,
  
  # Exploit multi-core properties of your CPU
  # parallel=TRUE,
  
  # We want to see the partial results of the process
  # while it performs
  monitor=TRUE,
  
  # Seed useful for replicating the results
  seed=1
)

# Solution for the final weights
w = as.vector(ga_res@solution)

