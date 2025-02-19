# pool results following Rubin's rules

pool_results_lm <- function(summary_list) {
  
  D = 10
 
  output <- summary_list %>% 
    mutate(W_d = SE^2) %>% 
    group_by(term) %>% 
    mutate(B_D = var(Estimate)) %>% 
    dplyr::summarize(B_D = mean(B_D), # B: Variance of estimates (between imputation variance)
                     W_D = mean(W_d), # \bar{V}: average of V_d over D imputed datasets
                     theta_D = mean(Estimate) # \bar{\theta}: mean of estimates,
                     # dfcom = mean(dfcom)  # Uncomment if dfcom_coxph is defined
    ) %>% 
    mutate(T_D = W_D + (1 + 1/D)*B_D, # Var(\theta|Y_{0}) ~ improved approximation of posterior variance [\bar{V} + B] 
           gamma_D = (1 + 1/D)*(B_D/T_D), # \hat{\gamma}_D = between imputation : total variance --> fraction of missing information
           nu = (D-1)*((1+ (1/(D+1))*(W_D/B_D))^2), # degrees of freedom of t-distribution
           nu2 = (D-1)/(gamma_D)^2 # equivalent to mice:::pool.fitlist >> mice:::barnard.rubin()'s dfold; (D/(D+1)) and not (1/(D+1))
           # nu_improved = mice:::barnard.rubin(D,B_D,T_D,dfcom = dfcom)  # Uncomment if applicable
    ) %>% 
    mutate(L = theta_D + qt(p = 0.025, df = nu2) * sqrt(T_D),
           U = theta_D + qt(p = 0.975, df = nu2) * sqrt(T_D),
           sqrt_T_D = sqrt(T_D)
    ) %>% 
    mutate(estimate = paste0(round(theta_D, 2), " \t (",
                       round(L, 2), ", ",
                       round(U, 2), ")"),
           mean = theta_D,
           lci = L,
           uci = U
    ) %>% 
    rename(iv = term)
  
  # Return the final output
  return(output)
}