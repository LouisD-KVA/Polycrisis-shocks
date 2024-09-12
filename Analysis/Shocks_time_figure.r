# Install ggplot2 and tidyverse if not installed
install.packages("tidyverse")  # tidyverse includes ggplot2 and dplyr
library(tidyverse)

# Set working directory (adjust the path to your own file location)
setwd("C:\\Users\\loudel30")

# 1. Load the dataset from CSV
if (file.exists("Shocks.csv")) {
  shock_fac.df <- read.csv("Shocks.csv")
  print("Data loaded successfully:")
  print(head(shock_fac.df))  # Check the first few rows of the dataset
} else {
  stop("File 'Shocks.csv' not found in the specified directory.")
}

# 2. Filter data (removing empty 'Shock.category' rows and date range)
shock_fac.df <- shock_fac.df %>%
  filter(nchar(Shock.category) > 0) %>%
  filter(Year >= 1970 & Year <= 2019)  # Filter for the years 1970 to 2019

print(paste("Number of rows after filtering:", nrow(shock_fac.df)))

# 3. Summarise data (summarizing by Year, Shock.category, and Shock.type)
shock_fac_sum.df <- shock_fac.df %>%
  group_by(Year, Shock.category, Shock.type) %>%
  summarise(sum = sum(count), .groups = 'drop')

print("Summarized data:")
print(head(shock_fac_sum.df))

# 4. Calculate relative sum (relative sum of each shock type)
shock_fac_sum.df <- shock_fac_sum.df %>%
  group_by(Shock.category, Shock.type) %>%
  mutate(rel_sum = sum / max(sum, na.rm = TRUE))

print("Data after calculating relative sum:")
print(head(shock_fac_sum.df))

# 5. Combine 'Shock.category' and 'Shock.type' for the plot labels
shock_fac_sum.df <- shock_fac_sum.df %>%
  mutate(Shock_comb = paste(substring(Shock.category, 1, 4), Shock.type, sep = ":"))

print("Data with combined shock labels:")
print(head(shock_fac_sum.df))

# 6. Plot (Wrap in print to ensure rendering in RStudio)
# Define a custom function to format the x-axis labels as years
format_years <- function(x) {
  # Format numbers as years
  format(x, nsmall = 0, big.mark = "", trim = TRUE)
}

# Plot 1: Faceting by Shock.category with smaller x-axis labels
# Plot 1: Faceting by Shock.category with smaller x-axis labels and grey theme
print(
  ggplot(shock_fac_sum.df, aes(x = Year, y = rel_sum, color = Shock_comb)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_wrap(vars(Shock.category)) +
    scale_x_continuous(
      breaks = seq(1970, 2019, by = 10),
      labels = format_years  # Ensure correct formatting if needed
    ) +
    theme_gray() +  # Apply the grey theme
    theme(
      axis.text.x = element_text(size = 7)  # Adjust text size for x-axis labels
    )
)

# Plot 2: Faceting by Shock_comb with smaller x-axis labels and grey theme
print(
  ggplot(shock_fac_sum.df, aes(x = Year, y = rel_sum)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_wrap(vars(Shock_comb)) +
    scale_x_continuous(
      breaks = seq(1970, 2019, by = 10),
      labels = format_years  # Ensure correct formatting if needed
    ) +
    theme_gray() +  # Apply the grey theme
    theme(
      axis.text.x = element_text(size = 7)  # Adjust text size for x-axis labels
    )
)


