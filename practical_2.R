f <- function (x, y) {
  z <- x^2 + y^2
  return ( c(cos(z), sin(z)))
}

# Simulates B sets of linear regressions of Y = X Beta + errors_gen(nrows(X))
# Reeturns an p x B matrix - B sets of T-statistics, one for each component of each Beta hat
LinMod_sim <- function(X, Beta = rep(0, p), errors_gen = rnorm, B = 10000) {
  n <- nrow(X)
  p <- ncol(X)
  y_mat <- as.vector(X %*% Beta) + matrix(errors_gen(n*B), n, B)
  inv_gram <- solve(t(X) %*% X)
  P <- X %*% inv_gram %*% t(X)
  Beta_hat_mat <- inv_gram %*% t(X) %*% y_mat
  resid_mat <- y_mat - P %*% y_mat
  sigma_tilde_vec <- colSums(resid_mat^2) / (n-p)
  t_stat_mat <- Beta_hat_mat / sqrt(diag(inv_gram))
  t_stat_mat <- t_stat_mat / rep(sqrt(sigma_tilde_vec), each=p)
  return (t_stat_mat)
}

qqplot_F <- function (f_stat, df1, df2) {
  f_stat <- sort(f_stat) # -> a flattened vector of elements from smallest to largest 
  B <- length(f_stat)
  theoretical_quantiles <- qf((1:B) / (B+1), df1, df2)
  plot(theoretical_quantiles, f_stat)
  abline(0, 1, col="red")
  return(mean(f_stat <= qf(0.95, df1, df2)))
}

X = matrix(runif(36), 9, 4) # n=9, p=4
# output2 <- LinMod_sim(X, errors_gen = rcauchy)
# size2 <- qqplot_F(output2, 1, 5)


# Simulates B sets of linear regressions of Y = X Beta + errors_gen(nrows(X))
# Reeturns an p x B matrix - B sets of T-statistics, one for each component of each Beta hat
LinMod_sim2 <- function(X, Beta = rep(0, p), errors_gen = rnorm, B = 10000, G_0=NULL) {
  n <- nrow(X)
  p <- ncol(X)
  # G_0 is the subset of componenets of Beta / columns of X to form Beta_0, X_0
  if (is.null(G_0)) {
    G_0 = 1:p
  }
  X_0 = X[, G_0]

  p_0 = length(G_0)
  
  
  y_mat <- as.vector(X %*% Beta) + matrix(errors_gen(n*B), n, B)
  inv_gram <- solve(t(X) %*% X)
  P <- X %*% inv_gram %*% t(X)
  inv_gram_0 <- solve(t(X_0) %*% X_0)
  P_0 <- X_0 %*% inv_gram_0 %*% t(X_0)
  
  if (p_0 < p) {
    top <- colSums( ((P - P_0) %*% y_mat )^2 ) / (p - p_0)
    I <- diag(1, ncol(P), ncol(P))
    bot <- colSums( ((I - P) %*% y_mat )^2 ) / (n - p)
    F_stat = top / bot
  }
  
  Beta_hat_mat <- inv_gram %*% t(X) %*% y_mat
  resid_mat <- y_mat - P %*% y_mat
  sigma_tilde_vec <- colSums(resid_mat^2) / (n-p)
  t_stat_mat <- Beta_hat_mat / sqrt(diag(inv_gram))
  t_stat_mat <- t_stat_mat / rep(sqrt(sigma_tilde_vec), each=p)
  
  new_list = list(t_stat_mat, F_stat)
  return (new_list)
}

# stuff = LinMod_sim2(X, G_0 = c(1, 3))
# qqplot_F(stuff[[2]], 2, 5) # p - p_0 = 4 - length(G_0) = 2
