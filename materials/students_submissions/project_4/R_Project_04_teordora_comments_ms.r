# Project_04 -> Portfolio Simulation and Analysis with Discrete Random Variables

# Step 1: Define the Portfolio and Parameters

# Define economic states and probabilities
states <- c("High", "Medium", "Low")
probabilities <- c(0.3, 0.5, 0.2)

# Define asset returns under each economic state
asset_returns <- list(
  "Asset 1" = c(0.08, 0.04, -0.02),
  "Asset 2" = c(0.12, 0.03, -0.05),
  "Asset 3" = c(0.05, 0.02, 0.01)
)

# Define portfolio weights
weights <- c(
  "Asset 1" = 0.5,
  "Asset 2" = 0.3,
  "Asset 3" = 0.2
)

# ---------------------------------------------------------------

# Step 2: Simulate Economic State
simulate_state <- function(probabilities) {
  sample(states, size = 1, prob = probabilities)
}

# ---------------------------------------------------------------

# Step 3: Retrieve Asset Returns for a Simulated State
get_asset_returns <- function(state) {
  state_index <- which(states == state)
  sapply(asset_returns, function(returns) returns[state_index])
}

# ---------------------------------------------------------------

# Step 4: Compute Portfolio Return
compute_portfolio_return <- function(asset_returns, weights) {
  sum(asset_returns * weights)
}

# ---------------------------------------------------------------

# Step 5: Simulate Portfolio Performance 

# Number of simulation periods
n_periods <- 1000  
# Initialize list to store simulation results
simulation_results <- list()

# Store portfolio returns directly in a separate list
portfolio_returns <- c()

for (i in 1:n_periods) {
  state <- simulate_state(probabilities)  
  returns <- get_asset_returns(state)     
  portfolio_return <- compute_portfolio_return(returns, weights)  
  
  cat(sprintf("Simulation %d - State: %s, Returns: %s, Portfolio Return: %.4f\n", 
              i, state, paste(returns, collapse = ", "), portfolio_return))
  
  # Append the portfolio return to the portfolio_returns list
  portfolio_returns <- c(portfolio_returns, portfolio_return)
  
  # Store the results in a list
  simulation_results[[i]] <- list(
    "State" = state,
    "Asset Returns" = returns,
    "Portfolio Return" = portfolio_return
  )
}

# Step 6: Analyze Portfolio Performance

# Ensure portfolio_returns is numeric
portfolio_returns <- as.numeric(portfolio_returns)

# Calculate summary statistics
mean_return <- mean(portfolio_returns, na.rm = TRUE)
variance_return <- var(portfolio_returns, na.rm = TRUE)

# Print calculated summary statistics
cat("\nSummary Statistics:\n")
cat(sprintf("Simulated Mean Return: %.4f\n", mean_return))
cat(sprintf("Simulated Variance of Return: %.4f\n", variance_return))

# Step 7: Visualize Results

# Create a histogram of the simulated portfolio returns
hist(portfolio_returns, breaks = 30, col = rgb(0, 0, 1, 0.7), 
     main = "Histogram of Simulated Portfolio Returns", 
     xlab = "Portfolio Return", 
     ylab = "Frequency")

# Calculate and display the frequency of each economic state
state_frequencies <- sapply(states, function(state) {
  mean(sapply(simulation_results, function(result) result$State == state))
})

# Display state frequencies to verify correct probability implementation
cat("\nState Frequencies (as percentages):\n")
for (i in 1:length(states)) {
  cat(sprintf("%s: %.2f%%\n", states[i], state_frequencies[i] * 100))
}

# The histogram clearly illustrates how the probability of economic states influences the portfolio's performance:

# The dominance of the return value in the "Medium" state (0.04) is a result of its high probability (50%).
# The combination of returns across all states contributes to reducing the portfolio's risk (diversification).

# Step 8
# 1. Relationship Between Diversification and Risk
# Diversification reduces risk by combining assets with different return profiles. If asset returns are not perfectly correlated, losses in one asset can be offset by gains in another, decreasing overall portfolio variance.

# 2. Impact of Adding a Fourth Asset
# Adding a fourth asset introduces more opportunities for diversification, which can further reduce portfolio variance if the new asset’s returns have low or negative correlation with existing assets.

# 3. Effect of Changing Portfolio Weights
# Altering portfolio weights impacts the expected return and variance:

# Increasing weights for higher-return assets raises the portfolio’s expected return but may increase variance.
# Shifting weights toward lower-risk assets reduces variance but may lower the expected return.
# Balancing weights is key to optimizing the risk-return tradeoff.


#Let me give my comments directly here in the file:
#  
#  

# General Feedback
# - The code is well-structured and follows a logical flow from defining the problem to simulation and analysis.
# - Function decomposition is handled correctly, making the solution modular and readable.
# - The approach to simulating economic states and retrieving returns is sound.
# - The analysis and visualization provide valuable insights into portfolio performance.

# Specific Comments
#
# 1. Function: `simulate_state`
#
# The function correctly samples an economic state based on probabilities. 
# However, it would be better to pass `states` as an argument to make the 
# function more reusable. This would allow flexibility if more states were added later.
#
#Suggested Improvement:

simulate_state <- function(states, probabilities) {
  sample(states, size = 1, prob = probabilities)
}

# Then, call it as:

state <- simulate_state(states, probabilities)

# 2. Function: `compute_portfolio_return`
# 
# The function correctly computes the weighted sum, but it assumes that 
# `asset_returns` and `weights` are always aligned in order. While this works 
# here, it might be safer to use explicit vector names for matching.
#
# Suggested Improvement:

compute_portfolio_return <- function(asset_returns, weights) {
  sum(asset_returns[names(weights)] * weights)
}

# This ensures that the correct weights are always applied to the correct 
# assets, even if the order changes.
#
# 3. Efficiency in Storing Simulation Results**
#
# The current approach stores each simulation result in a list, which is 
# flexible but can be inefficient in R due to list growth during 
# iteration (`simulation_results[[i]] <- ...`). Instead, pre-allocating a 
# data frame would improve performance.
#
# Suggested Improvement:
#Instead of:

simulation_results <- list()

# Consider using:

simulation_results <- data.frame(State = character(n_periods),
                                 PortfolioReturn = numeric(n_periods),
                                 stringsAsFactors = FALSE)

# Then, store results as:

simulation_results[i, ] <- c(state, portfolio_return)


# This reduces memory reallocation overhead.

# 4. Avoid Growing `portfolio_returns` in a Loop**
#
# Appending elements iteratively (`portfolio_returns <- c(portfolio_returns, portfolio_return)`) 
# leads to performance issues because R creates a new vector each time. Instead, pre-allocate a 
# numeric vector:
#
# Suggested Improvement:

portfolio_returns <- numeric(n_periods)

for (i in 1:n_periods) {
  state <- simulate_state(probabilities)
  returns <- get_asset_returns(state)
  portfolio_returns[i] <- compute_portfolio_return(returns, weights)
}

# This is much faster and avoids unnecessary memory operations.
#
#
# 5. Improve Economic State Frequency Calculation**
#
# The state frequency calculation is correct but can be simplified using `table()` for more clarity.
#
# Instead of:

state_frequencies <- sapply(states, function(state) {
  mean(sapply(simulation_results, function(result) result$State == state))
})

# Use:

state_frequencies <- table(sapply(simulation_results, `[[`, "State")) / n_periods


# This approach is more concise and computationally efficient.

#
# 7. Clarify Diversification Discussion**
#  
# The discussion on diversification is insightful. However, students might 
# benefit from a more precise explanation of how correlations between asset returns 
# affect variance reduction. Consider illustrating the impact of correlation explicitly by calculating:


cor_matrix <- cor(do.call(rbind, lapply(simulation_results, function(x) x$`Asset Returns`)))

# This shows how asset correlations influence risk.



# Final Thoughts
# - Great job on structuring the project in a modular and clear way!
# - Optimizing memory management (pre-allocating vectors, using data frames) would make the code more efficient.
# - Enhancing the visualization with density curves and mean indicators would improve interpretability.
# - The theoretical discussion could be expanded with a direct look at correlations.
# - Thanks for your efforts and your diligent work. I wish you and the group much success 
# in the program and in your respective careers