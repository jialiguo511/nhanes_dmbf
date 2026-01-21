rm(list=ls());gc();source(".Rprofile")

library(survey)
library(ggplot2)
library(dplyr)

# Load imputed datasets
nhanes_svy_dfs <- readRDS(
  paste0(path_nhanes_dmbf_folder,
         "/working/cleaned/dbwse02_weighted df with complete cases.RDS")
)

# Step 1: Extract data from first imputation (for simplicity)
des <- nhanes_svy_dfs[[1]]
df <- des$variables

# Step 2: Calculate means and deciles by group
results <- list()

for (female_val in c(0, 1)) {
  for (dm_val in c("NoDM", "PreDM", "DM")) {
    
    subset_data <- df %>%
      dplyr::filter(female == female_val & dm == dm_val) %>%
      dplyr::pull(bmi) %>%
      na.omit()
    
    if (length(subset_data) == 0) next
    
    # Mean
    mean_val <- mean(subset_data)
    sd_val <- sd(subset_data)
    se_val <- sd_val / sqrt(length(subset_data))
    
    # Min and Max
    min_val <- min(subset_data)
    max_val <- max(subset_data)
    
    results[[paste0(female_val, "_", dm_val, "_mean")]] <- data.frame(
      female = female_val,
      dm = dm_val,
      type = "mean",
      prob = NA_real_,
      value = mean_val,
      se = se_val,
      min_val = min_val,
      max_val = max_val
    )
    
    # Deciles
    deciles <- quantile(subset_data, probs = seq(0.1, 0.9, 0.1))
    
    for (i in seq_along(deciles)) {
      prob_val <- seq(0.1, 0.9, 0.1)[i]
      results[[paste0(female_val, "_", dm_val, "_q", i)]] <- data.frame(
        female = female_val,
        dm = dm_val,
        type = "quantile",
        prob = prob_val,
        value = as.numeric(deciles[i]),
        se = NA_real_,
        min_val = min_val,
        max_val = max_val
      )
    }
  }
}

# Step 3: Combine results
plot_data <- bind_rows(results)

cat("Plot data:\n")
print(head(plot_data, 15))
cat("Unique combinations:\n")
print(plot_data %>% dplyr::filter(type == "mean"))

# Step 4: Prepare for plotting
mean_data <- plot_data %>%
  dplyr::filter(type == "mean") %>%
  mutate(
    sex_label = ifelse(female == 0, "Male", "Female"),
    dm_label = factor(dm, levels = c("NoDM", "PreDM", "DM"),
                      labels = c("No DM", "Pre-DM", "DM"))
  ) %>%
  arrange(sex_label, dm_label)

# Create x positions
mean_data$x <- as.numeric(factor(interaction(mean_data$sex_label, mean_data$dm_label)))
mean_data$group_label <- paste(mean_data$sex_label, mean_data$dm_label, sep = " | ")

decile_data <- plot_data %>%
  dplyr::filter(type == "quantile") %>%
  mutate(
    sex_label = ifelse(female == 0, "Male", "Female"),
    dm_label = factor(dm, levels = c("NoDM", "PreDM", "DM"),
                      labels = c("No DM", "Pre-DM", "DM"))
  ) %>%
  arrange(sex_label, dm_label, prob)

# Map x positions from mean_data to decile_data
x_map <- mean_data %>% dplyr::select(sex_label, dm_label, x) %>% distinct()
decile_data <- decile_data %>%
  left_join(x_map, by = c("sex_label", "dm_label"))

cat("Mean data:\n")
print(mean_data)
cat("\nDecile data (first 15):\n")
print(head(decile_data, 15))

# Step 5: Create plot
p <- ggplot() +
  # Columns showing min to max range
  geom_rect(
    data = mean_data,
    aes(xmin = x - 0.3, xmax = x + 0.3, ymin = min_val, ymax = max_val),
    fill = "steelblue",
    alpha = 0.7,
    color = NA
  ) +
  # Horizontal lines for deciles
  geom_segment(
    data = decile_data,
    aes(x = x - 0.25, xend = x + 0.25, y = value, yend = value),
    color = "darkred",
    size = 0.6,
    alpha = 0.8
  ) +
  # Mean point
  geom_point(
    data = mean_data,
    aes(x = x, y = value),
    color = "black",
    size = 2
  ) +
  # Labels and formatting
  scale_x_continuous(
    breaks = mean_data$x,
    labels = mean_data$group_label,
    expand = c(0.02, 0.02)
  ) +
  labs(
    x = NULL,
    y = "BMI",
    title = "Mean BMI with deciles (10th-90th) by Sex and DM Status"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

print(p)

# ggsave("figure_deciles.png", p, width = 11, height = 6, dpi = 300)
