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