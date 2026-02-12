rm(list=ls());gc();source(".Rprofile")

library(magick)

# -------------------------------
# File paths for the individual figures
# -------------------------------
fig_dir <- paste0(path_nhanes_dmbf_folder, "/figures/complete cases/")

# List all PNG files in the directory
all_files <- list.files(fig_dir, pattern = ".*_deciles_by_sex_dm\\.png$", full.names = TRUE)

cat("Found files in", fig_dir, ":\n")
print(all_files)

# Check if we found the expected 5 files
if (length(all_files) < 5) {
  cat("\nWARNING: Expected 5 PNG files, but found", length(all_files), "\n")
  cat("Make sure you've run figures_deciles by dm and sex separate.R first\n")
}

# Determine order (BMI, Body Fat, Visceral Fat, Waist Circumference, Waist-to-Height Ratio)
pattern_order <- c("BMI", "Body_Fat", "Visceral_Fat", "Waist_Circumference", "Waist_Circumference_to_Height")

# Sort files by matching the pattern order
ordered_files <- c()
for (pattern in pattern_order) {
  matching_file <- all_files[grep(pattern, all_files, ignore.case = TRUE)]
  if (length(matching_file) > 0) {
    ordered_files <- c(ordered_files, matching_file[1])
  }
}

cat("\nOrdered files for combining:\n")
print(ordered_files)

# Check we have all 5
if (length(ordered_files) != 5) {
  cat("\nERROR: Could not find all 5 expected files. Found:", length(ordered_files), "\n")
  stop("Missing plot files")
}

# Read all images
images <- lapply(ordered_files, image_read)

cat("\nLoaded", length(images), "images\n")

# Combine into 3x2 grid
# First row: BMI, Body Fat, Visceral Fat
row1 <- c(images[[1]], images[[2]], images[[3]])
combined_row1 <- image_append(row1, stack = FALSE)

# Second row: Waist Circumference, Waist-to-Height Ratio, and placeholder
row2 <- c(images[[4]], images[[5]])
combined_row2 <- image_append(row2, stack = FALSE)

# Combine rows vertically
combined_plot <- image_append(c(combined_row1, combined_row2), stack = TRUE)

# Save combined figure
combined_filename <- "combined_deciles_by_sex_dm.png"
image_write(combined_plot, 
            paste0(fig_dir, combined_filename))

cat("\n\nSaved combined figure:", combined_filename)
