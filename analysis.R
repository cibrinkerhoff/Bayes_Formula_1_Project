# Poisson-Gamma Bayesian Analysis
# Question: Do drivers take more pit stops on street circuits
#           vs. permanent race tracks?
# Each row = one driver in one race, with their stop count
#            and circuit type (Street or Permanent)


library(ggplot2)

# --- Load Data ---
df <- read.csv("stop_type_df.csv")%>%
  filter(stop<8)

unique(df$stop)
table(df$stop)

street    <- df$stop[df$Type == "Street"]
permanent <- df$stop[df$Type == "Permanent"]

cat("Street circuit races:   ", length(street), "\n")
cat("Permanent circuit races:", length(permanent), "\n")
cat("Street mean stops:      ", mean(street), "\n")
cat("Permanent mean stops:   ", mean(permanent), "\n")



# PRIOR ELICITATION
# Prior belief: drivers average about 2 pit stops per race.
# We'll say our prior is worth ~10 races of information.
# So: a = prior_mean * b = 2 * 10 = 20, b = 10
#
# This is a moderately informative prior — feel free to
# adjust a and b to reflect stronger or weaker prior beliefs.

a <- 3  # shape
b <- 2   # rate  (prior mean = a/b = 2 stops)

theta_vals <- seq(0, 6, length = 1001)

# Visualize the prior
plot(theta_vals, dgamma(theta_vals, shape = a, rate = b),
     type = 'l', col = 'purple', lwd = 2,
     xlab = expression(theta),
     ylab = "Density",
     main = "Gamma Prior on Mean Pit Stops (a=3, b=2)")


# POSTERIOR UPDATES
# Poisson-Gamma conjugacy:
#   Prior:     theta ~ Gamma(a, b)
#   Likelihood: Y_i | theta ~ Poisson(theta)
#   Posterior: theta | y ~ Gamma(a + sum(y), b + n)


# Street Circuits
n_s     <- length(street)
sum_s   <- sum(street)
astar_s <- a + sum_s
bstar_s <- b + n_s

cat("\n--- Street Circuit Posterior ---\n")
cat("Posterior mean:    ", astar_s / bstar_s, "\n")
cat("Posterior variance:", astar_s / bstar_s^2, "\n")
cat("95% Credible Interval: ",
    qgamma(c(0.025, 0.975), shape = astar_s, rate = bstar_s), "\n")


# Permanent Circuits
n_p     <- length(permanent)
sum_p   <- sum(permanent)
astar_p <- a + sum_p
bstar_p <- b + n_p

cat("\n--- Permanent Circuit Posterior ---\n")
cat("Posterior mean:    ", astar_p / bstar_p, "\n")
cat("Posterior variance:", astar_p / bstar_p^2, "\n")
cat("95% Credible Interval: ",
    qgamma(c(0.025, 0.975), shape = astar_p, rate = bstar_p), "\n")


# VISUALIZE PRIOR VS POSTERIORS

plot(theta_vals, dgamma(theta_vals, shape = a, rate = b),
     type = 'l', col = 'gray50', lwd = 2, lty = 2,
     xlab = expression(theta), ylab = "Density",
     main = "Prior vs. Posterior: Mean Pit Stops by Circuit Type",
     ylim = c(0, 15))

lines(theta_vals, dgamma(theta_vals, shape = astar_s, rate = bstar_s),
      col = 'steelblue', lwd = 2)

lines(theta_vals, dgamma(theta_vals, shape = astar_p, rate = bstar_p),
      col = 'tomato', lwd = 2)

legend("topright",
       legend = c("Prior", "Street (Posterior)", "Permanent (Posterior)"),
       col    = c("gray50", "steelblue", "tomato"),
       lwd    = 2, lty = c(2, 1, 1))


# MONTE CARLO COMPARISON
# Pr(theta_street > theta_permanent | data)

set.seed(42)
N <- 100000

th_street    <- rgamma(N, shape = astar_s, rate = bstar_s)
th_permanent <- rgamma(N, shape = astar_p, rate = bstar_p)

prob_street_higher <- mean(th_street > th_permanent)

cat("\n--- Monte Carlo Comparison ---\n")
cat("Pr(theta_street > theta_permanent | data): ", prob_street_higher, "\n")
cat("Posterior mean difference (Street - Permanent): ",
    mean(th_street - th_permanent), "\n")
cat("95% CI on difference: ",
    quantile(th_street - th_permanent, c(0.025, 0.975)), "\n")


# Histogram of the posterior difference
hist(th_street - th_permanent,
     breaks = 100,
     col    = "lightblue",
     border = "white",
     main   = "Posterior Distribution of (theta_Street - theta_Permanent)",
     xlab   = expression(theta[Street] - theta[Permanent]))

abline(v = 0, col = "red", lwd = 2, lty = 2)
abline(v = mean(th_street - th_permanent), col = "darkblue", lwd = 2)

legend("topright",
       legend = c("Posterior mean diff"),
       col    = c("darkblue"),
       lwd    = 2, lty = c(2, 1))



# POSTERIOR PREDICTIVE: How many stops in a NEW race?


# Monte Carlo posterior predictive samples
ppred_street    <- rpois(N, th_street)
ppred_permanent <- rpois(N, th_permanent)

cat("\n--- Posterior Predictive ---\n")
cat("Most likely stops (street):   ", as.integer(names(which.max(table(ppred_street)))), "\n")
cat("Most likely stops (permanent):", as.integer(names(which.max(table(ppred_permanent)))), "\n")

# Plot side-by-side predictive distributions
par(mfrow = c(1, 2))

plot(table(ppred_street) / N, type = 'h', col = 'steelblue', lwd = 3,
     xlab = "Pit Stops", ylab = "Probability",
     main = "Predictive: Street Circuit")

plot(table(ppred_permanent) / N, type = 'h', col = 'tomato', lwd = 3,
     xlab = "Pit Stops", ylab = "Probability",
     main = "Predictive: Permanent Circuit")

par(mfrow = c(1, 1))

