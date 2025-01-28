# Project_03 -> Evaluating Credit Risk Using Conditional Probabilities

# Parameters of the problem
P_D <- 0.04 # Probability of default
P_not_D <- 1 - P_D       # Probability of non-default
P_L_given_D <- 0.7       # Probability of a low credit score given default
P_L_given_not_D <- 0.1   # Probability of a low credit score given non-default

# Step 1: Compute P(D|L) using Bayes' Rule
# Bayes' formula: P(D|L) = P(L|D) * P(D) / [P(L|D) * P(D) + P(L|not D) * P(not D)]

# Calculate the overall probability of having a low credit score, P(L)
# P(L) = P(L∣D) * P(D) + P(L∣¬D) * P(¬D)
P_L <- P_L_given_D * P_D + P_L_given_not_D * P_not_D

# Calculate the posterior probability of default given a low credit score, P(D|L)
P_D_given_L <- (P_L_given_D * P_D) / P_L

# Print the theoretical value of P(D|L)
cat("Theoretical probability P(D|L):", P_D_given_L, "\n")

# Step 2: Simulate customer data
set.seed(123)  # Set seed for reproducibility
n_customers <- 10000  # Number of customers in the simulation

# Generate default status for each customer using P(D)
# For each customer, default is modeled as a Bernoulli random variable with probability P(D)
defaults <- rbinom(n_customers, 1, P_D)

# Generate low credit scores based on default status
low_credit_scores <- ifelse(
  # Conditional statement to determine low credit score based on default status
  defaults == 1, 
  # If the customer defaults, draw low credit score using P(L|D)
  rbinom(n_customers, 1, P_L_given_D), 
  # If the customer does not default, draw low credit score using P(L|not D)
  rbinom(n_customers, 1, P_L_given_not_D)
)

# Combine the generated data into a data frame for easier analysis
data <- data.frame(
  Default = defaults,                 # Default status (1 = default, 0 = non-default)
  LowCreditScore = low_credit_scores  # Low credit score status (1 = low score, 0 = normal score)
)

# Step 3: Calculate P(D|L) from the simulated data
# Count cases where a customer has both defaulted and has a low credit score
# Divide this by the total number of customers with a low credit score to estimate P(D|L)
simulated_P_D_given_L <- sum(data$Default == 1 & data$LowCreditScore == 1) / 
  sum(data$LowCreditScore == 1)

# Print the simulated value of P(D|L)
cat("Simulated probability P(D|L):", simulated_P_D_given_L, "\n")

# Step 4: Visualization of theoretical vs simulated P(D|L)
# Create a bar plot to compare the theoretical and simulated probabilities
barplot(
  c(Teoretical = P_D_given_L, Simulated = simulated_P_D_given_L), # Values to compare
  beside = TRUE,                     # Plot bars side by side
  col = c("blue", "red"),            # Assign colors to bars
  main = "Comparison of Theoretical and Simulated P(D|L)", # Title of the plot
  ylab = "Probability",              # Label for the y-axis
  ylim = c(0, 1)                     # Set y-axis limits
)


# Comments MS:
#
# Strengths:
# 1. Logical and Correct Implementation:
#    - Your solution is logically sound and follows the problem's requirements step by step.
#    - Both the theoretical calculation using Bayes' Rule and the simulation of the customer data 
#      are well-structured and accurate.
#    - The code is free of major errors, and the use of comments provides a helpful guide to the
#      logic behind each step.
#
# 2. Simulation and Visualization:
#    - The simulation of customer data and the comparison of theoretical and simulated results 
#      are well-executed. The inclusion of a bar plot adds clarity to the comparison and enhances 
#      the overall presentation.
#   - It is great that you found this as a clear use case for the binomial distribution. In my worked solution
#     I suggest a different, somewhat more clumsy implementation because I was not assuming that the binomial
#     disrtibution is a concept already known.
# Areas for Improvement:
# 1. Readability and Human Engagement:
#    - While the submission is correct, it is quite dense and hard to follow for a human reviewer.
#      This suggests that it might have been generated or heavily influenced by an LLM.
#    - While I encourage using tools like ChatGPT to assist your learning, a human-centered 
#      approach should still guide your submission. Here’s how:
#      - Add high-level explanations before diving into code. For example, before Step 2 (Simulation),
#        briefly explain *why* you’re simulating customer data and how it connects to the problem.
#      - Simplify comments. For instance, the comment on the conditional statement for low credit 
#        scores could be condensed to:
#        > "Simulate low credit scores based on the customer’s default status."
#      - Use consistent formatting for your comments to make them more readable.

# 2. Active Learning and Refinement:
#    - When using an LLM to assist with the project, consider taking the time to rephrase or refine 
#      its output. This will help you better understand the code and ensure the final result reflects 
#      your voice and style. Ask yourself:
#      - Does every step make sense to me?
#      - Can I rewrite this section to be clearer or more concise?
#      - Would I structure the code differently to improve readability?

# 3. Exploring Edge Cases:
#    - The submission handles the core problem well but does not discuss or test potential edge cases.
#      For example:
#      - What happens if n_customers is very small (e.g., 10)?
#      - How might rounding errors impact the results if probabilities are very close 
#        (e.g., P_D = 0.0401)?
#    - Including a brief discussion or test of such scenarios could demonstrate deeper understanding 
#      and analytical thinking.

# 4. Real-World Relevance:
#    - While the project focuses on theoretical and simulated probabilities, connecting these results 
#      to real-world implications would strengthen the analysis. For instance:
#      - What do the results imply for a bank evaluating a customer’s credit risk?
#      - How might the probabilities change if additional factors (e.g., income, loan type) were included?

# Suggestions for Improvement:
# - Simplify and Personalize:
#   - After generating code with an LLM, rewrite and simplify sections to make them your own. 
#     This will not only improve readability but also enhance your understanding of the material.
#
# - Focus on Explanation:
#   - High-level summaries for each section will help readers (and reviewers) understand the 
#     broader purpose of each step without getting lost in the technical details.
#
# - Explore Additional Insights:
#   - Expand the project by experimenting with different probabilities or visualizations 
#     (e.g., plotting conditional probabilities across different groups of customers). 
#     This would demonstrate creativity and a deeper engagement with the problem.

# Overall, your submission shows a strong understanding of the problem and a clear mastery of the 
# required techniques. By focusing on refining your communication and presentation, you can make your 
# work even more impactful and engaging.
