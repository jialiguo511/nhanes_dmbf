
extract_summary <- function(model_list) {
  # Inner function to extract coefficients and create a data frame
  extract_summary <- function(summary_svyglm, model_id) {
    coefs <- summary_svyglm$coefficients
    data.frame(
      Model = model_id,
      term = rownames(coefs),
      Estimate = coefs[, "Estimate"],
      SE = coefs[, "Std. Error"],
      Pr = coefs[, "Pr(>|t|)"]
    )
  }
  
  # Combine the summaries using the inner function
  combined_summaries <- do.call(rbind, lapply(seq_along(model_list), function(i) {
    extract_summary(model_list[[i]], paste("M", i))
  }))
  
  return(combined_summaries)
}
