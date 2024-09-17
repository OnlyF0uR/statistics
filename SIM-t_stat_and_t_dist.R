install.packages("ggplot2")
library(ggplot2)

# Population
mu = 12
sigma = 2.245

# Single sample
n = 3200
x = 12.12
sd = 2.145

# =========================
# Sim
sim_mean <- vector()
sim_sd <- vector()
sim_se <- vector()

for (i in 1:10000) {
  # Generate array of random numbers, size n
  # following a distribution given mean and sd
  rand_arr <- rnorm(n, mu, sigma)
  
  sim_mean[i] <- mean(rand_arr)
  sim_sd[i] <- sd(rand_arr)
  sim_se[i] <- sim_sd[i] / sqrt(n)
}

hist(sim_mean,
     main = "Distribution of Sample Means",
     xlab = "Sample Mean",
     breaks = 20,
     col = "lightblue",
     border = "black"
     )

# This is the actual standard error computed from
# the simulated sample distribution
se = sd(sim_mean)

# This is the estimated standard error given the
# standard deviation of and number of participants
# of the collected sample
se2 = sd / sqrt(n)

# se and se2 should be somewhat similar given that sigma and sd
# are relatively close, so our single sample seems to be
# somewhat representative of the population data

# =========================
t_stat <- (x - mu) / se2
cohen_d <- t_stat / sqrt(n) # Effect size
sim_t_stat <- (sim_mean - mu) / sim_se

sim_t_stat_df <- data.frame(df_t_stat = sim_t_stat)

# This is for plotting the theoretical distribution
theoretical_density <- dt(sim_t_stat, df = n-1)
theoretical_df <- data.frame(t_stat = sim_t_stat, density = theoretical_density)
# Actually plot the density and theoretical distribution
ggplot(sim_t_stat_df, aes(x = df_t_stat)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 20,  # Adjust the number of bins as needed
                 fill = "pink", 
                 color = "black", 
                 alpha = 0.5) +
  geom_line(data = theoretical_df, aes(x = t_stat, y = density), color = "black", size = 1, alpha = 0.8) +
  labs(title = "Distribution of Simulated t-Statistics with Theoretical t-Distribution",
       x = "t-value",
       y = "Density") +
  theme_minimal()

