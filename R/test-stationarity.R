test_stationarity <- function(stationary_model, non_stationary_models) {
  
  # Extract log-likelihoods from both models -----------------------------------
  ll_stat <- as.numeric(logLik(stationary_model))
  
  ll_nonstat <- sum(sapply(non_stationary_models, logLik))
  
  # Extract coefficients from both models --------------------------------------
  coef_stat <- c(t(coef(stationary_model)))
  
  coef_nonstat <- unlist(lapply(
    non_stationary_models, function(m) c(t(coef(m)))
    ))
  
  # Calculate difference -------------------------------------------------------
  # Ensure coefs are of the same length
  min_length <- min(length(coefs_stat), length(coefs_nonstat))
  coef_diff <- coefs_nonstat[1:min_length] - coefs_stat[1:min_length]
  
  # Create a covariance matrix -------------------------------------------------
  vcov_nonstat <- Matrix::bdiag(lapply(non_stationary_models, vcov))
  
  # Add small ridge regularization to make matrix invertible
  vcov_reg <- as.matrix(vcov_nonstat) + 1e-6*diag(nrow(vcov_nonstat))
  
  # Subset matrix
  vcov_subset <- vcov_reg[1:min_length, 1:min_length]
  
  # Calculate test statistics --------------------------------------------------
  # Likelihood ratio test
  LR_test <- -2 * (ll_stat - ll_nonstat)
  
  # Wald test
  wald_stat <- tryCatch({
    
    t(coef_diff) %*% solve(vcov_subset) %*% coef_diff
  }, error = function(e) {
    
    MASS::ginv(vcov_subset) %*% coef_diff
    
    t(coef_diff) %*% MASS::ginv(vcov_subset) %*% coef_diff
  })
  
  # Degrees of freedom
  df_LR <- length(coef_nonstat) - length(coef_stat)
  df_Wald <- min_length
  
  list(
    LR = LR_test,
    df_LR = length(coef_nonstat) - length(coef_stat),
    Wald = wald_stat,
    df_Wald = length(coef_nonstat)
  )
}
