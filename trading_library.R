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
markowitz_ptlf = function(w,m,M,l) l*as.numeric(t(w)%*%M%*%w) - 
  (1-l)*as.numeric(w%*%m) + 100*constraint(w)

# Compute startegy given 
# data: dataset of adjusted returns (from stockDataDownload)
# k_weeks: when update the weights (weeks)
# back_weeks: number of weeks to calculate PCA
# l: lambda parameters for Markowitz portfolio
# kp: number of PCA considered to create portfolio 
compute_startegy = function(data, k_weeks = 1, back_weeks = 10, l = 0.3, kp = 20,...){
  
  # Import dataset and create returns time series
  prices = data$adjusted
  log_returns = diff(log(prices))[-1]

  # Select every k weeks with xts
  ep1 = endpoints(log_returns,on= "weeks",k=k_weeks)
  dtp = data.frame(markowitz = 0,
                   pca_nn = 0)
  ts_week0 = xts(dtp,
                 order.by = index(log_returns[ep1[back_weeks]]))
  ts = ep1[back_weeks:length(ep1)]
  
  # Create structure to store the weights
  weights = vector("list",length(ts)-1)
  for(i in 1:(length(ts)-1)){ 
    
    # Create time series where to calculate PCA
    log_ret_short = log_returns[(ts[i]-back_weeks):ts[i]]
    comp_portfolio = which(sapply(log_ret_short, function(x) sum(!is.na(x))!=nrow(log_ret_short)))
    if(length(comp_portfolio) == 0) log_ret_short = log_ret_short
    else log_ret_short = log_ret_short[, -comp_portfolio]
    
    # Calculate vector of means, varcov matrix, initialize weights
    M = cov(log_ret_short)
    m = colMeans(log_ret_short)
    w = rep(1/NCOL(log_ret_short),NCOL(log_ret_short))
    
    # Compute Markowitz weights
    w_markowitz = optim(rep(1/NCOL(log_ret_short),NCOL(log_ret_short)), 
                        markowitz_ptlf,
                        method = "L-BFGS-B",
                        m = m, M = M, l = l)$par
    
    # Compute PCA non negative weights
    w_pca_nn = nsprcomp(log_ret_short,nneg = T,
                        ncomp = min(kp,NCOL(log_ret_short)))$rotation
    
    # Create time series to apply (week forward)
    tmp_ts = log_returns[paste0(index(log_returns)[ts[i]+1],"/",
                                index(log_returns)[ts[i+1]]),
                         names(log_ret_short)]
    
    # Create the NAV with both of the tecniques
    portfolio = data.frame(pca_nn = tmp_ts %*% (w_pca_nn[,min(kp,NCOL(log_ret_short))]/
                                                  sum(w_pca_nn[,min(kp,NCOL(log_ret_short))])),
                           markovitz = tmp_ts %*% w_markowitz)
    
    # Store weights (PCA and Markowitz) and dates
    tmp_w = cbind(w_pca_nn,w_markowitz)
    weights[[i]]$weights = tmp_w
    weights[[i]]$week = index(tmp_ts)
    
    # Bind with past returns
    ts_week = xts(portfolio, order.by = index(tmp_ts))
    ts_week0 = rbind.xts(ts_week0,ts_week)
  }
  
  # Return real NAV + weights and date
  return(list(nav = exp(cumsum(ts_week0)),
              weights = weights))
}


