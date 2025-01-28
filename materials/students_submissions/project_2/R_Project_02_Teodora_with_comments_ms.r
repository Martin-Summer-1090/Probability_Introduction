# Step 1:
# 1. To determine if the leading digits of revenues and expenditures conform to Benford’s Law.
# 2. To interpret deviations, particularly in expenditure data, which may suggest anomalies such as fraud or manipulation.

# Benford's Law is a statistical principle that states that in many naturally occurring collections
# of numbers, the leading digit (the first non-zero digit) is likely to be small. 

# Step 2: Obtain and Inspect the Dataset
library(readr)

# Load the dataset
file_path <- "C:/Users/Teodora/Downloads/MCF/R_Project/Project_02/company_financials.csv"



# Comment MS:

# This does not work because read_csv is an R function from the readr package. So you either
# need to load readr by using library(readr) or you need to use a baseR function such as
# read.csv.
# As a matter of principle, whenever you write a piece of code - any code for that matter -
# always test whether it runs and does what you want it to do. This is a fantastic immediate
# feedback property of coding, which you very rarely have in any other real life situation.
# So never miss it. Here you would have recognized that written like this just
# nothing has been written into your data object

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


# Here are some further comments you might find useful:
  

# 1. Code Organization:
#    The code is well-structured and logically broken into steps, making it easy to follow. 
#    Each section of the analysis is clearly defined with comments, which is excellent practice for 
#    both clarity and reproducibility.
# 2. Data Preparation**:
#    The `extract_leading_digit` function is well-designed and effectively extracts 
#    leading digits from the dataset. However:
#    Consider adding a comment or explanation on how the regular expression `[1-9]` works 
#    for extracting the leading digit, as it might not be immediately clear to all readers.
#    Ensure the function handles edge cases gracefully, such as input with no digits or unexpected formats.
#
# 3. Empirical and Theoretical Comparison:
#    You computed and compared empirical and theoretical frequencies systematically. However, consider 
#    explicitly calculating the absolute differences between Benford’s Law and the observed frequencies for 
#   both revenue and expenditure. For instance:
#  
# comparison_df$Abs_Diff_Revenue <- abs(comparison_df$Revenue - comparison_df$Benford)
# comparison_df$Abs_Diff_Expenditure <- abs(comparison_df$Expenditure - comparison_df$Benford)
#
# This would quantify the deviations and make it easier to identify digits with the largest discrepancies.
#
# 4. Visualization:
# The side-by-side bar plots in base R are functional and clear. To further improve the plots:
# Add labels for each bar group to show exact frequency values.
#
# 5. Interpretation of Results:
# Your evaluation of conformity to Benford’s Law is clear and well-presented. 
# However, the criteria used to determine conformity (`abs(diff) < 0.01`) might benefit from more context. For example:
# Why was this threshold chosen? Is it based on experience or literature?
# Would a statistical test (e.g., Chi-Square Goodness-of-Fit) provide a more formal measure of conformity?
#
# 6. Real-World Implications:
#
# The discussion of potential causes for deviations in expenditure is a strong addition. 
# You might expand on this by considering:
# Whether rounding or artificial values might also be present in revenue.
# How these insights could be applied in real-world financial forensic investigations.
#
# Sample Size and Reliability**:
# You correctly note that large sample sizes increase reliability. To enhance this point, you could 
# briefly explain how small sample sizes might lead to unreliable conformity assessments due to 
# higher variability in observed frequencies.
#
# Suggested Enhancements:
# Commenting Best Practices:
# While the comments are good, consider adding a short high-level explanation 
# before each major block of code to outline the purpose of the step (e.g., "This section 
# calculates the observed frequencies for revenue and expenditure leading digits and compares them to Benford's Law.").
# Further Exploration:
# Highlight any specific digits (e.g., leading digit 1 or 5) in expenditure 
# that show large deviations. This could lead to a deeper analysis of anomalies.
# Statistical Testing
# While not required, incorporating a statistical test (e.g., Chi-Square) 
# would add rigor and support your conclusions.


