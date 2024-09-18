library(psych)
library(MASS)

# Set seed for reproducibility
set.seed(123)

# Number of respondents
n <- 200

# Correlation matrix between the two underlying dimensions (Confidence and Attractiveness)
cor_matrix <- matrix(c(1, 0.5,   # Confidence correlated with itself and Attractiveness (moderate correlation)
                       0.5, 1),  # Attractiveness correlated with itself and Confidence
                     nrow = 2)

# Define loadings for the 10 questions on the two dimensions (Confidence and Attractiveness)
loadings <- matrix(c(0.8, 0.3,   # Q1 loads highly on Confidence, slightly on Attractiveness
                     0.7, 0.2,   # Q2 similarly loads more on Confidence
                     0.6, 0.4,   # Q3 loads moderately on both
                     0.9, 0.1,   # Q4 loads highly on Confidence
                     0.75, 0.25, # Q5 loads moderately on Confidence
                     0.2, 0.8,   # Q6 loads highly on Attractiveness
                     0.3, 0.7,   # Q7 loads similarly on Attractiveness
                     0.4, 0.6,   # Q8 loads moderately on both
                     0.1, 0.9,   # Q9 loads highly on Attractiveness
                     0.25, 0.75  # Q10 loads moderately on Attractiveness
), nrow = 10, byrow = TRUE)

# Generate latent factor scores for Confidence and Attractiveness
latent_factors <- mvrnorm(n, mu = c(0, 0), Sigma = cor_matrix)

# Generate responses based on loadings and latent factors
responses <- latent_factors %*% t(loadings) + matrix(rnorm(n * 10), nrow = n)

# Convert responses to realistic 1-5 scale
response_data <- round(pmin(pmax(responses + 3, 1), 5))

# Show raw data (first few rows)
head(response_data)

# Perform Exploratory Factor Analysis (EFA) using Principal Axis Factoring (PAF) with oblique rotation (Promax)
efa <- fa(response_data, nfactors = 2, rotate = "promax", fm = "pa")

# Rename the factors to 'Confidence' and 'Attractiveness'
colnames(efa$loadings) <- c("Confidence", "Attractiveness")
colnames(efa$Structure) <- c("Confidence", "Attractiveness")
rownames(efa$Phi) <- colnames(efa$Phi) <- c("Confidence", "Attractiveness")

# Display the pattern matrix (factor loadings)
# Indicate the unique contribution of each factor to the
# observed variable
cat("\nPattern Matrix (Factor Loadings):\n")
print(efa$loadings)

# Display the structure matrix
cat("\nStructure Matrix:\n")
print(efa$Structure)

# Display the factor correlation matrix
cat("\nFactor Correlation Matrix:\n")
print(efa$Phi)
