rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(pROC)
  library(readr)
  library(tibble)
})

cat("\n=== dbwse07: Incremental value assessment ===\n")

# This analysis intentionally uses ordinary (unweighted) logistic regression for
# ROC/AUC estimation, matching the prior implementation. Survey-weighted
# descriptive and association analyses are handled elsewhere in the pipeline.

analysis_dir <- file.path("complete cases", "analysis")
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

configured_data_dir <- Sys.getenv("NHANES_ADIPOSITY_DIR", unset = "")
data_roots <- unique(c(
  configured_data_dir,
  "/Users/jialiguo/jialiguo/NHANES Adiposity",
  getwd()
))
data_roots <- data_roots[nzchar(data_roots)]

rds_candidates <- unique(unlist(lapply(data_roots, function(root) {
  c(
    file.path(root, "working", "cleaned", "dbwse02_weighted df with complete cases.RDS"),
    file.path(root, "dbwse02_weighted df with complete cases.RDS")
  )
})))

rds_path <- rds_candidates[file.exists(rds_candidates)][1]
if (is.na(rds_path)) {
  stop(
    "Could not find dbwse02_weighted df with complete cases.RDS. ",
    "Set NHANES_ADIPOSITY_DIR to the folder containing the data file."
  )
}

cat("Loading: ", rds_path, "\n", sep = "")
nhanes_svy_dfs <- readRDS(rds_path)
nhanes_data <- nhanes_svy_dfs[[1]]$variables

required_cols <- c(
  "dm", "age", "female", "race_eth", "bmi", "fat_percentage",
  "visceral_fat", "waistcircumference", "WHtR"
)
missing_cols <- setdiff(required_cols, names(nhanes_data))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Preserve the previous standardization convention: z-scores are based on the
# overall analytic sample, before forming either pairwise outcome comparison.
adiposity_vars <- c(
  "bmi", "fat_percentage", "visceral_fat", "waistcircumference", "WHtR"
)
for (var in adiposity_vars) {
  nhanes_data[[paste0("z_", var)]] <-
    (nhanes_data[[var]] - mean(nhanes_data[[var]], na.rm = TRUE)) /
    sd(nhanes_data[[var]], na.rm = TRUE)
}

model_specs <- tribble(
  ~model_id, ~model_type, ~added_predictor, ~formula_text,
  "base", "base", "bmi_only", "outcome_binary ~ age + female + race_eth + z_bmi",
  "fat", "incremental", "fat_percentage", "outcome_binary ~ age + female + race_eth + z_bmi + z_fat_percentage",
  "vf", "incremental", "visceral_fat", "outcome_binary ~ age + female + race_eth + z_bmi + z_visceral_fat",
  "wc", "incremental", "waistcircumference", "outcome_binary ~ age + female + race_eth + z_bmi + z_waistcircumference",
  "whtr", "incremental", "WHtR", "outcome_binary ~ age + female + race_eth + z_bmi + z_WHtR"
)

wilson_interval <- function(events, total, z = qnorm(0.975)) {
  proportion <- events / total
  denominator <- 1 + z^2 / total
  center <- (proportion + z^2 / (2 * total)) / denominator
  half_width <- z * sqrt(
    proportion * (1 - proportion) / total + z^2 / (4 * total^2)
  ) / denominator
  cbind(
    lower = pmax(0, center - half_width),
    upper = pmin(1, center + half_width)
  )
}

analyse_comparison <- function(data, case_level, outcome_label) {
  # IMPORTANT: physically subset the ordinary data frame before defining the
  # binary outcome. Filtering a tbl_svy and then extracting $variables can keep
  # all rows for domain estimation, causing the third glycaemic group to be
  # incorrectly coded as a control.
  comparison_data <- data %>%
    filter(dm %in% c(case_level, "NoDM")) %>%
    mutate(outcome_binary = as.integer(dm == case_level)) %>%
    droplevels()

  expected_n <- sum(data$dm %in% c(case_level, "NoDM"), na.rm = TRUE)
  stopifnot(
    nrow(comparison_data) == expected_n,
    all(comparison_data$dm %in% c(case_level, "NoDM")),
    setequal(unique(comparison_data$outcome_binary), c(0L, 1L))
  )

  cat(
    sprintf(
      "%s: N = %d (%d cases, %d NoDM controls)\n",
      outcome_label,
      nrow(comparison_data),
      sum(comparison_data$outcome_binary == 1),
      sum(comparison_data$outcome_binary == 0)
    )
  )

  fitted_models <- lapply(model_specs$formula_text, function(formula_text) {
    glm(as.formula(formula_text), data = comparison_data, family = binomial())
  })
  names(fitted_models) <- model_specs$model_id

  predictions <- lapply(fitted_models, predict, type = "response")
  roc_objects <- lapply(predictions, function(predicted) {
    roc(comparison_data$outcome_binary, predicted, quiet = TRUE, direction = "<")
  })

  base_auc <- as.numeric(auc(roc_objects$base))

  auc_rows <- lapply(seq_len(nrow(model_specs)), function(i) {
    model_id <- model_specs$model_id[i]
    roc_object <- roc_objects[[model_id]]
    auc_value <- as.numeric(auc(roc_object))
    auc_ci <- as.numeric(ci.auc(roc_object, method = "delong"))

    if (model_id == "base") {
      delta_auc <- NA_real_
      delta_ci <- c(NA_real_, NA_real_)
      p_delong <- NA_real_
    } else {
      comparison <- roc.test(
        roc_objects$base,
        roc_object,
        method = "delong",
        paired = TRUE,
        conf.int = TRUE
      )
      # pROC reports the CI for AUC(base) - AUC(incremental). Reverse its
      # bounds to report AUC(incremental) - AUC(base).
      delta_auc <- auc_value - base_auc
      delta_ci <- c(-comparison$conf.int[2], -comparison$conf.int[1])
      p_delong <- comparison$p.value
    }

    tibble(
      outcome = outcome_label,
      model_id = model_id,
      model_type = model_specs$model_type[i],
      added_predictor = model_specs$added_predictor[i],
      AUC = auc_value,
      AUC_ci_lower = auc_ci[1],
      AUC_ci_upper = auc_ci[3],
      AUC_diff = delta_auc,
      AUC_diff_ci_lower = delta_ci[1],
      AUC_diff_ci_upper = delta_ci[2],
      p_delong = p_delong,
      Brier = mean(
        (comparison_data$outcome_binary - predictions[[model_id]])^2
      ),
      N = nrow(comparison_data),
      N_case = sum(comparison_data$outcome_binary == 1),
      N_control = sum(comparison_data$outcome_binary == 0)
    )
  })

  roc_rows <- lapply(seq_len(nrow(model_specs)), function(i) {
    model_id <- model_specs$model_id[i]
    curve <- as_tibble(coords(
      roc_objects[[model_id]],
      x = "all",
      ret = c("specificity", "sensitivity"),
      transpose = FALSE
    ))
    curve %>%
      transmute(
        outcome = outcome_label,
        model_id = model_id,
        added_predictor = model_specs$added_predictor[i],
        false_positive_rate = 1 - specificity,
        sensitivity = sensitivity
      ) %>%
      arrange(false_positive_rate, sensitivity)
  })

  calibration_rows <- lapply(seq_len(nrow(model_specs)), function(i) {
    model_id <- model_specs$model_id[i]
    calibration <- tibble(
      observed = comparison_data$outcome_binary,
      predicted = as.numeric(predictions[[model_id]])
    ) %>%
      mutate(prediction_decile = ntile(predicted, 10)) %>%
      group_by(prediction_decile) %>%
      summarise(
        mean_predicted = mean(predicted),
        observed_events = sum(observed),
        observed_rate = mean(observed),
        n = n(),
        .groups = "drop"
      )

    intervals <- wilson_interval(
      calibration$observed_events,
      calibration$n
    )

    calibration %>%
      mutate(
        observed_ci_lower = intervals[, "lower"],
        observed_ci_upper = intervals[, "upper"],
        outcome = outcome_label,
        model_id = model_id,
        added_predictor = model_specs$added_predictor[i]
      ) %>%
      select(
        outcome, model_id, added_predictor, prediction_decile,
        mean_predicted, observed_rate, observed_ci_lower,
        observed_ci_upper, n
      )
  })

  list(
    auc = bind_rows(auc_rows),
    roc = bind_rows(roc_rows),
    calibration = bind_rows(calibration_rows)
  )
}

dm_results <- analyse_comparison(nhanes_data, "DM", "DM vs NoDM")
predm_results <- analyse_comparison(nhanes_data, "PreDM", "PreDM vs NoDM")

auc_results <- bind_rows(dm_results$auc, predm_results$auc)
roc_curve_data <- bind_rows(dm_results$roc, predm_results$roc)
calibration_curve_data <- bind_rows(
  dm_results$calibration,
  predm_results$calibration
)

brier_results <- auc_results %>%
  select(outcome, model_id, model_type, added_predictor, Brier, N, N_case, N_control)

write_csv(auc_results, file.path(analysis_dir, "dbwse07_auc_results.csv"))
write_csv(brier_results, file.path(analysis_dir, "dbwse07_calibration_brier.csv"))
write_csv(roc_curve_data, file.path(analysis_dir, "dbwse07_roc_curve_data.csv"))
write_csv(
  calibration_curve_data,
  file.path(analysis_dir, "dbwse07_calibration_curve_data.csv")
)

cat("\n=== Corrected AUC summary ===\n")
print(
  auc_results %>%
    mutate(
      across(
        c(
          AUC, AUC_ci_lower, AUC_ci_upper, AUC_diff,
          AUC_diff_ci_lower, AUC_diff_ci_upper, p_delong, Brier
        ),
        ~ round(.x, 4)
      )
    )
)

cat("\nSaved corrected model summaries and curve data to ", analysis_dir, "\n", sep = "")
