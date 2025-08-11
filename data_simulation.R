
# Simulate Plant Biomass Data ---------------------------------------------

set.seed(123)  # for reproducibility

# Number of grassland plots
n <- 50

# Explanatory variable: Average annual rainfall in mm/year
# We simulate realistic rainfall values across plots: roughly 400–1400 mm/year.
rain_min <- 400
rain_max <- 1400
Rainfall <- runif(n, min = rain_min, max = rain_max)

# True (unknown) relationship used to simulate biomass
# Intercept (beta0) and slope (beta1)
beta0 <- 80          # baseline biomass when rainfall = 0 (not observed in data)
beta1 <- 0.55        # expected increase (g/m^2) per 1 mm increase in rainfall

# Random noise: other unmeasured processes (soil, species mix, grazing, etc.)
sigma <- 80          # standard deviation of residual variation (g/m^2)

# Response variable: mean annual plant biomass (g/m^2)
# Y_i = beta0 + beta1 * Rainfall_i + Normal(0, sigma^2)
Biomass <- beta0 + beta1 * Rainfall + rnorm(n, mean = 0, sd = sigma)

# Put everything in a data.frame with clear units in names
plant_biomass <- data.frame(
  plot_id = factor(seq_len(n)),
  Rainfall_mm = Rainfall,
  Biomass_gm2 = Biomass
)




# Simulate New Plant Biomass Data ---------------------------------------------

set.seed(123)  # for reproducibility

# Number of grassland plots
n <- 50

# Explanatory variable: Average annual rainfall in mm/year
# We simulate realistic rainfall values across plots: roughly 400–1400 mm/year.
rain_min <- 400
rain_max <- 1700
Rainfall <- runif(n, min = rain_min, max = rain_max)

# True (unknown) relationship used to simulate biomass
# Intercept (beta0) and slope (beta1)
beta0 <- 50          # baseline biomass when rainfall = 0 (not observed in data)
beta1 <- 0.4        # expected increase (g/m^2) per 1 mm increase in rainfall

# Random noise: other unmeasured processes (soil, species mix, grazing, etc.)
sigma <- 80          # standard deviation of residual variation (g/m^2)

# Response variable: mean annual plant biomass (g/m^2)
# Y_i = beta0 + beta1 * Rainfall_i + Normal(0, sigma^2)
Biomass <- beta0 + beta1 * Rainfall + rnorm(n, mean = 0, sd = sigma)

# Put everything in a data.frame with clear units in names
plant_biomass_new <- data.frame(
  plot_id = factor(seq_len(n)),
  Rainfall_mm = Rainfall,
  Biomass_gm2 = Biomass
)


# Bernoulli Data   - Disease Presence -------------------------------------


set.seed(123)

n <- 250                                    # number of animals
Age <- round(runif(n, 0, 12), 1)            # ages between 0 and 12 years

# True parameters used to simulate (unknown to the analyst)
beta0 <- -2.0                                # baseline log-odds at Age = 0
beta1 <-  0.28                               # per-year change in log-odds

# Compute true probability of disease at each age via inverse-logit
# Note: plogis(x) = 1 / (1 + exp(-x))
eta  <- beta0 + beta1 * Age                  # linear predictor
pi   <- plogis(eta)                          # probability of infection

# Simulate Bernoulli outcomes: 1 = infected, 0 = not infected
Disease <- rbinom(n, size = 1, prob = pi)

disease_presence <- data.frame(Age, Disease)
head(disease_presence)

# Quick sanity check: overall prevalence
mean(disease_presence$Disease)



# Poisson Data ------------------------------------------------------------

n <- 150                                   # number of sites
VegDensity <- runif(n, 0, 100)             # % cover from 0 to 100

# "True" relationship used to generate data (unknown to the analyst)
beta0 <- log(0.8)                          # baseline mean nests when VegDensity = 0
beta1 <- 0.018                             # change in log mean per +1% cover
# Interpretation of beta1: exp(0.018) ≈ 1.018 -> ~1.8% more nests per +1% cover

eta  <- beta0 + beta1 * VegDensity         # linear predictor on log scale
mu   <- exp(eta)                           # expected nests (must be > 0)

# Simulate Poisson counts
Nests <- rpois(n, lambda = mu)

birds <- data.frame(Site = factor(seq_len(n)),
                    VegDensity = VegDensity,
                    Nests = Nests)


# Mixed Binomial/Poisson data ---------------------------------------------

set.seed(2025)

n <- 300
veg   <- runif(n, 0, 100)                       # % cover (continuous, >= 0)
water <- rgamma(n, shape = 3, rate = 1/200)     # distances, right-skewed (mean ~600 m)
preds <- rlnorm(n, meanlog = log(2), sdlog = 0.5)   # predator index, right-skewed

# True data-generating mechanisms (unknown to an analyst)
# Presence model (logistic): logit(pi) = b0 + b1*veg + b2*water + b3*preds
b0 <- -1.4;  b1 <- 0.028;  b2 <- -0.0012;  b3 <- -0.22
eta_occ <- b0 + b1*veg + b2*water + b3*preds
pi_occ  <- plogis(eta_occ)
presence <- rbinom(n, size = 1, prob = pi_occ)

# Abundance model (Poisson, only when present): log(mu) = g0 + g1*veg + g2*water + g3*preds
g0 <- log(0.9); g1 <- 0.014; g2 <- -0.0007; g3 <- -0.10
mu <- exp(g0 + g1*veg + g2*water + g3*preds)
nests <- ifelse(presence == 1, rpois(n, lambda = mu), 0)  # zeros if absent

wetlands <- data.frame(site = seq_len(n), presence, nests, veg, water, preds)


