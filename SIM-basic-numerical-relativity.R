c <- 299792458  # Speed of light in m/s
G <- 6.67430e-11  # Gravitational constant in m^3 kg^-1 s^-2

# Calculate Schwarzschild metric
schwarzschild_metric <- function(r, M) {
  g_tt <- -(1 - 2*G*M/(r*c^2))
  g_rr <- 1 / (1 - 2*G*M/(r*c^2))
  g_theta_theta <- r^2
  g_phi_phi <- r^2 * sin(theta)^2
  
  return(list(g_tt = g_tt, g_rr = g_rr, g_theta_theta = g_theta_theta, g_phi_phi = g_phi_phi))
}

# Calculate Christoffel symbols (first kind)
christoffel_symbols <- function(r, M) {
  Gamma_ttr <- G*M / (r^2 * (1 - 2*G*M/(r*c^2)))
  Gamma_rtt <- (G*M*c^2 / r^2) * (1 - 2*G*M/(r*c^2))
  Gamma_rrr <- -G*M / (r^2 * (1 - 2*G*M/(r*c^2)))
  Gamma_rtheta_theta <- -r * (1 - 2*G*M/(r*c^2))
  Gamma_rphi_phi <- -r * sin(theta)^2 * (1 - 2*G*M/(r*c^2))
  Gamma_theta_r_theta <- 1/r
  Gamma_phi_r_phi <- 1/r
  Gamma_phi_theta_phi <- cot(theta)
  
  return(list(
    Gamma_ttr = Gamma_ttr, Gamma_rtt = Gamma_rtt, Gamma_rrr = Gamma_rrr,
    Gamma_rtheta_theta = Gamma_rtheta_theta, Gamma_rphi_phi = Gamma_rphi_phi,
    Gamma_theta_r_theta = Gamma_theta_r_theta, Gamma_phi_r_phi = Gamma_phi_r_phi,
    Gamma_phi_theta_phi = Gamma_phi_theta_phi
  ))
}

# Basic time evolution (using Euler method)
simple_evolution <- function(initial_r, initial_v, M, dt, steps) {
  r <- initial_r
  v <- initial_v
  results <- data.frame(time = 0, r = r, v = v)
  
  for (i in 1:steps) {
    # Calculate acceleration using geodesic equation (simplified)
    a <- -G * M / r^2
    
    # Update position and velocity
    r <- r + v * dt
    v <- v + a * dt
    
    results <- rbind(results, data.frame(time = i*dt, r = r, v = v))
  }
  
  return(results)
}

# Usage
M <- 1.989e30  # Mass of the Sun in kg
initial_r <- 1e8  # Initial radius in meters
initial_v <- 1000  # Initial velocity in m/s
dt <- 0.1  # Time step in seconds
steps <- 1000  # Number of time steps

results <- simple_evolution(initial_r, initial_v, M, dt, steps)

plot(results$time, results$r, type = "l", xlab = "Time (s)", ylab = "Radial distance (m)")
abline(h = 0, col = "red", lty = 2)

collision_index <- which(results$r <= 0)[1]
if (!is.na(collision_index)) {
  collision_time <- results$time[collision_index]
  print(paste("The object 'collides' with the center at time:", collision_time, "seconds"))
} else {
  print("The object never 'collides' with the center within the simulated time.")
}
