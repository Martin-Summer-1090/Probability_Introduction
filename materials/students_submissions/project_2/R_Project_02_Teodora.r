# Step 1:
# 1. To determine if the leading digits of revenues and expenditures conform to Benford’s Law.
# 2. To interpret deviations, particularly in expenditure data, which may suggest anomalies such as fraud or manipulation.

# Benford's Law is a statistical principle that states that in many naturally occurring collections
# of numbers, the leading digit (the first non-zero digit) is likely to be small. 

# Step 2: Obtain and Inspect the Dataset
library(readr)

# Load the dataset
file_path <- "C:/Users/Teodora/Downloads/MCF/R_Project/Project_02/company_financials.csv"
data <- read_csv(file_path)

# Inspect the dataset
head(data)     # Display the first few rows
str(data)      # Get a summary of the data structure
summary(data)  # Statistical summary of numerical columns

# Step 3: Prepare the Data

# Filter valid data
# Exclude rows with zero or negative values and missing data
filtered_data <- subset(data, Revenue > 0 & Expenditure > 0 & !is.na(Revenue) & !is.na(Expenditure))

# Extract leading digits
extract_leading_digit <- function(value) {
  value_str <- as.character(value)
  leading_digit <- as.numeric(substr(value_str, regexpr("[1-9]", value_str), regexpr("[1-9]", value_str)))
  return(leading_digit)
}

# Create new columns for leading digits and apply the function on the Revenue and Expenditure columns
filtered_data$Revenue_Leading_Digit <- sapply(filtered_data$Revenue, extract_leading_digit)
filtered_data$Expenditure_Leading_Digit <- sapply(filtered_data$Expenditure, extract_leading_digit)

head(filtered_data)  # Verify the new columns

# Step 4: Analyze the Data

# Compute empirical frequencies for leading digits
compute_frequencies <- function(column) {
  freq_table <- table(column)
  freq_table <- freq_table / sum(freq_table)  # Normalize to probabilities
  return(freq_table)
}

# Compute frequencies for revenue and expenditure leading digits
revenue_freq <- compute_frequencies(filtered_data$Revenue_Leading_Digit)
expenditure_freq <- compute_frequencies(filtered_data$Expenditure_Leading_Digit)

# Theoretical Benford's Law distribution
benford_law <- function() {
  return(sapply(1:9, function(digit) log10(1 + 1/digit)))
}

benford_freq <- benford_law()

# Convert to data frame for comparison
comparison_df <- data.frame(
  Digit = 1:9,
  Benford = benford_freq,
  Revenue = sapply(1:9, function(digit) ifelse(digit %in% names(revenue_freq), revenue_freq[as.character(digit)], 0)),
  Expenditure = sapply(1:9, function(digit) ifelse(digit %in% names(expenditure_freq), expenditure_freq[as.character(digit)], 0))
)

print(comparison_df)

# Visualize the Results
library(ggplot2)

# Set up the plotting area to display 2 plots side by side (1 row, 2 columns)
par(mfrow = c(1, 2))

# Revenue Plot (Base R)
height_matrix_revenue <- rbind(comparison_df$Benford, comparison_df$Revenue)

# Create the side-by-side bar chart for Revenue data
barplot(
  height = height_matrix_revenue,
  beside = TRUE,
  col = c("blue", "red"),
  names.arg = comparison_df$Digit,
  legend.text = c("Benford", "Revenue"),
  args.legend = list(x = "topright"),
  main = "Revenue Leading Digit Distribution",
  xlab = "Leading Digit",
  ylab = "Frequency",
  ylim = c(0, max(height_matrix_revenue) * 1.1)
)

# Expenditure Plot (Base R)
height_matrix_expenditure <- rbind(comparison_df$Benford, comparison_df$Expenditure)

# Create the side-by-side bar chart for Expenditure data
barplot(
  height = height_matrix_expenditure,
  beside = TRUE,
  col = c("blue", "red"),
  names.arg = comparison_df$Digit,
  legend.text = c("Benford", "Expenditure"),
  args.legend = list(x = "topright"),
  main = "Expenditure Leading Digit Distribution",
  xlab = "Leading Digit",
  ylab = "Frequency",
  ylim = c(0, max(height_matrix_expenditure) * 1.1)
)

# Reset the plotting layout to default (single plot per window)
par(mfrow = c(1, 1))

# Step 5: Interpret the Results

# Evaluate conformity
if (all(abs(comparison_df$Revenue - comparison_df$Benford) < 0.01)) {
  print("Revenue data conforms closely to Benford's Law.")
} else {
  print("Revenue data shows deviations from Benford's Law.")
}

if (all(abs(comparison_df$Expenditure - comparison_df$Benford) < 0.01)) {
  print("Expenditure data conforms closely to Benford's Law.")
} else {
  print("Expenditure data shows significant deviations from Benford's Law.")
}

# Hypothesize causes for expenditure deviations
print("Possible causes for expenditure deviations:")
print("1. Rounded or artificial values in the data.")

# Possible causes for expenditure deviations include anomalies arising from rounded numbers, 
# which are often found in the Expenditure column, an indication that the data is either rounded
# in a certain way or inserted, as in the real world, such values are not always so "clean" and natural,
# especially in the context of finance.

# Discuss implications of probabilities and sample sizes
print("Large sample sizes increase the reliability of empirical frequencies.")




