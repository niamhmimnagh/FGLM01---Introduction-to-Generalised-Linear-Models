# ============================================================
# Normal Model as a GLM: Ecological Example in R
# ------------------------------------------------------------
# Topic: Mean annual plant biomass (g/m^2) vs. average annual rainfall (mm/year)
# Goal: Fit and interpret the same linear model using both lm() and glm()
#       with Gaussian family (identity link), and show they are equivalent.
# ============================================================

# ---------------------------
# 0) Clean environment
# ---------------------------
rm(list = ls())

# ---------------------------
# 1) Import data
# ---------------------------
url <- "https://raw.githubusercontent.com/niamhmimnagh/FGLM01---Introduction-to-Generalised-Linear-Models/refs/heads/main/data/plant_biomass.txt"
plant_biomass <- read.table(url, header = TRUE, sep = "\t")
head(plant_biomass)

# ---------------------------
# 2) Visualise the data
# ---------------------------
# Creating a scatterplot of biomass vs rainfall to assess linear trend
# We expect that biomass increases with rainfall, with variance due to other factors
# Scatterplot with a least-squares line (from lm) to see the trend.

plot(plant_biomass$Rainfall_mm, plant_biomass$Biomass_gm2,
     pch = 19, col = "grey40",
     xlab = "Average annual rainfall (mm/year)",
     ylab = expression(paste("Mean annual biomass (g/", m^2, ")")),
     main = "Grassland biomass vs. rainfall")
grid()
abline(lm(Biomass_gm2 ~ Rainfall_mm, data = plant_biomass), lwd = 2)

# ---------------------------
# 3) Fit the model using lm()
# ---------------------------
# We fit a standard linear model using lm() in R
# This fits the model: Biomass = beta0 + beta1*rainfall+error

fit_lm <- lm(Biomass_gm2 ~ Rainfall_mm, data = plant_biomass)

# Summarise the model
print(summary(fit_lm))
# Coefficients table:
# (Intercept): Estimated biomass when rainfall = 0 mm/year (56.40 g/m^2).
#               Here, it's an extrapolation since rainfall in data is much higher.
# Rainfall_mm: For each 1 mm increase in rainfall, biomass increases by ~0.58 g/m^2.
# Std. Error:   How much the estimate is expected to vary due to sampling.
# t value:      Estimate divided by its Std. Error (test statistic).
# Pr(>|t|):     p-value for the hypothesis that the coefficient = 0.

# Residual standard error:
# This tells us how far our observed values are from the 
# Interpretation of RSE (~75 g/m^2):
# Biomass values range from ~100 to 800 g/m^2, so the typical prediction error
# is about 10–15% of the observed range. This is moderate: rainfall explains
# most variation (R^2 ≈ 0.84), but other ecological factors still contribute.


# Multiple R-squared:
# 0.8404 means rainfall explains ~84% of the variation in biomass.
# Adjusted R-squared adjusts for number of predictors (almost the same here).
# Adjusted R^2 is close to Multiple R^2 here because we have only 1 predictor.
# Adding predictors will increase adjusted R^2 only if they improve the model
# enough to offset the penalty for extra terms; otherwise, it will decrease.


# F-statistic:
# Tests whether the model explains significantly more variation than a null model (intercept only).
# Very large F value with tiny p-value (< 2.2e-16) means the model is highly significant overall.

# 95% confidence intervals for coefficients
print(confint(fit_lm))
# If we repeated the sampling many times, 95% of such intervals would contain the true coefficient
# a wider interval reflects more uncertainty


# ---------------------------
# 4) Fit the same model using glm() with Gaussian(identity)
# ---------------------------

fit_glm <- glm(Biomass_gm2 ~ Rainfall_mm,
               family = gaussian(link = "identity"),
               data = plant_biomass)

# Summarise the GLM
print(summary(fit_glm)); cat("\n")



# ---------------------------
# 5) Show that lm() and glm() give the same results here
# ---------------------------
# Coefficients
coef(fit_lm)
coef(fit_glm)
# Fitted values (The model’s predictions for the mean response at each observed rainfall value.)
plot(fitted(fit_lm), fitted(fit_glm))
# Residuals (Residuals = observed value – fitted value. They represent the error left unexplained by the model for each observation.)
plot(residuals(fit_lm), residuals(fit_glm))

# Residual sum of squares equals GLM deviance for Gaussian(identity)
# It’s the total squared difference between observed and fitted values. a measure of how much variation is not explained by the model. 
# Smaller RSS means better fit.
rss_lm <- sum(residuals(fit_lm)^2)
# Deviance in GLMs is a generalisation of RSS for non-Gaussian models.
# For Gaussian with identity link, the deviance formula simplifies exactly to the RSS.
dev_glm <- deviance(fit_glm)
print(c(RSS_lm = rss_lm, Deviance_glm = dev_glm))


# 6) Basic diagnostic checks
# ---------------------------


par(mfrow = c(1, 2))  # show two plots side-by-side

# 1) Residuals vs Fitted
# What it shows:
#   - Residuals (errors) on the y-axis vs the model's fitted values on the x-axis.
# What to expect under a good linear model:
#   - A random cloud of points centered around 0 (no visible pattern).
#   - Roughly constant vertical spread across the x-axis (homoscedasticity).
# Red flags:
#   - Curved pattern -> linearity may be violated (relationship not straight-line).
#   - Funnel/wedge shape -> heteroscedasticity (non-constant variance).
#   - Isolated extreme points -> potential outliers/influential observations.
plot(fitted(fit_lm), resid(fit_lm),
     pch = 19, col = "grey40",
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, lty = 2)  # reference line at zero residual

# 2) Normal Q-Q Plot of residuals
# What it shows:
#   - Compares residual quantiles to a theoretical Normal distribution.
# What to expect if residuals are approximately Normal:
#   - Points fall close to the straight reference line.
# Red flags:
#   - S-shaped curve -> skewness.
#   - Systematic deviations at the ends -> heavy/light tails.
#   - Strong departures throughout -> residuals not Normal (affects t/F inferences).
qqnorm(resid(fit_lm), pch = 19, col = "grey40",
       main = "Normal Q-Q Plot (lm residuals)")
qqline(resid(fit_lm), lwd = 2)

par(mfrow = c(1, 1))  # reset plotting layout

# If you see strong curvature or funnelling:
# - Consider transforming the response or predictors,
# - Or moving to an alternative GLM/variance structure appropriate to the data.


# ---------------------------
# 7) Predictions for new rainfall values (with uncertainty)
# ---------------------------
url<-"https://raw.githubusercontent.com/niamhmimnagh/FGLM01---Introduction-to-Generalised-Linear-Models/refs/heads/main/data/plant_biomass_new.txt"
plant_biomass_new <- read.table(url, header = TRUE, sep = "\t")
head(plant_biomass_new)


# Get predictions on the response scale
pred_glm <- predict(fit_glm, newdata = plant_biomass_new, type = "link", se.fit = TRUE)
summary(pred_glm)
# Fit_mean = predicted values (same as lm() for Gaussian identity)
fit_mean <- pred_glm$fit
se_mean  <- pred_glm$se.fit
plot(fit_mean, plant_biomass_new$Biomass_gm2, xlab='fitted', ylab='observed')



# Bernoulli GLM - Disease Presence ----------------------------------------

# ============================================================
# Logistic (Bernoulli) GLM Example: Disease ~ Age
# ------------------------------------------------------------
# Model:
#   Y_i ~ Bernoulli(pi_i)
#   logit(pi_i) = log(pi_i / (1 - pi_i)) = beta0 + beta1 * Age_i
#
# beta0: log-odds of infection at age 0
# beta1: change in log-odds of infection for each additional year of age
# exp(beta1): multiplicative change in the odds per +1 year (odds ratio)
# ============================================================

# 0) Housekeeping -------------------------------------------------------------
rm(list = ls())
# 1) Import Data ------------------------------------------------------------
url <- "https://raw.githubusercontent.com/niamhmimnagh/FGLM01---Introduction-to-Generalised-Linear-Models/refs/heads/main/data/disease_presence.txt"
disease_presence <- read.table(url, header = TRUE, sep = "\t")
head(disease_presence)

# 2) Visualise Raw Data -------------------------------------------------------
# Plot the binary outcomes against age with jitter so points don't overlap
plot(disease_presence$Age, jitter(disease_presence$Disease, amount = 0.06),
     pch = 19, col = "grey40",
     xlab = "Age (years)",
     ylab = "Disease (1=yes, 0=no; jittered)",
     main = "Disease status vs Age (with fitted logistic curve)")
grid()


# 3) Fit Logistic GLM ---------------------------------------------------------
# family = binomial(link = "logit") fits the Bernoulli/logistic model.
fit <- glm(Disease ~ Age, data = disease_presence, family = binomial(link = "logit"))

summary(fit)
# Interpretation:
# - Intercept = -2.350:
#     At Age = 0, the estimated log-odds of disease are -2.35.
#     Converting to probability: p = exp(-2.35) / (1 + exp(-2.35)) ≈ 0.087,
#     meaning about an 8.7% chance of disease at birth.
#
# - Age coefficient = 0.317:
#     For each additional year of age, the log-odds of disease increase by 0.317.
#     In odds terms: exp(0.317) ≈ 1.37, meaning the odds of disease are ~37% higher
#     for each extra year of age.
#
# - p-values (< 0.001 for both intercept and slope) show both parameters are
#   statistically significant. The strong significance for Age (p ≈ 6.9 × 10⁻¹¹)
#   suggests a clear positive association between age and disease probability.
#
# - Null deviance (339.48) vs residual deviance (286.70):
#     The drop of ~52.78 indicates the model with Age fits much better than the
#     intercept-only model. This is strong evidence that Age explains part of the
#     variation in disease status.
#
# - AIC = 290.7:
#     A lower AIC means a better trade-off between fit and complexity. Here, it
#     reflects that the Age model is a better fit than the null model.


# 4) Effect Size as Odds Ratio ------------------------------------------------
or     <- exp(coef(fit)["Age"])                       # odds ratio per +1 year
ci_log <- confint(fit, parm = "Age")                  # CI on log-odds scale
ci_or  <- exp(ci_log)                                 # CI on odds ratio scale

or; ci_or
# Interpretation:
# - 'or' = multiplicative change in the odds of disease for +1 year of age.
#   e.g., OR = 1.32 means each additional year multiplies the odds by 1.32 (~32% ↑).
# - 'ci_or' is the 95% CI for that odds ratio.

# 5) Model Fit Summaries ------------------------------------------------------
# Pseudo R^2 (McFadden): 1 - (resid deviance / null deviance)
pseudoR2 <- 1 - deviance(fit) / fit$null.deviance
pseudoR2
# Interpretation:
# measure of relative fit
# Here, the model explains ~15.5% of the deviance compared to a model without Age.
# In logistic regression, 0.2–0.4 is considered excellent fit; 0.1–0.2 is decent, 
# especially for binary biological data where there’s natural variability.
# 0.155 means Age is a meaningful predictor, but there’s still a lot of unexplained variation — other factors likely influence disease presence.
# - Higher values (closer to 1) indicate better improvement over the intercept-only model.
#   Values around 0.2–0.4 are often considered decent for logistic models (context matters).

# Likelihood ratio test vs intercept-only model (equivalent to drop-in-deviance test)
lr_stat <- fit$null.deviance - deviance(fit)
lr_df   <- fit$df.null - fit$df.residual
lr_p    <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
c(LR_stat = lr_stat, df = lr_df, p_value = lr_p)
# Interpretation:
# - Tests whether adding Age significantly improves fit over the null (no Age).
# df is the number of additional parameters in the larger model
#   Small p-value is very small = Age is a significant predictor.
# the probability of seeing such an improvement in fit by chance is very small

# 7) Predicted probabilities at specific ages --------------------------------
ages_of_interest <- data.frame(Age = c(2, 5, 8, 12))
pp_link <- predict(fit, newdata = ages_of_interest, type = "link", se.fit = TRUE) #type=link calls the linear predictor (log odds)

pp_fit <- plogis(pp_link$fit) # inverse logit transform - maps real numbers to (0,1)


pred_table <- data.frame(
  Age = ages_of_interest$Age,
  Prob_mean = round(pp_fit, 3)
)
pred_table
# Interpretation:
# - 'Prob_mean' gives the estimated probability of disease at that age.
# - The CI is for the *mean* probability (not for an individual animal).



# Poisson GLM - Nest counts -----------------------------------------------

# ============================================================
# Poisson GLM Example: Bird nests ~ Vegetation density
# ------------------------------------------------------------
# Question: Do sites with denser vegetation have more bird nests?
# Response: Count of nests (non-negative integers)
# Predictor: Vegetation density (e.g., % cover or a vegetation index)
# Model:    Y_i ~ Poisson(mu_i),  log(mu_i) = beta0 + beta1 * VegDensity_i
#           exp(beta1) = multiplicative change in expected nests per 1-unit
#           increase in vegetation density (an incidence-rate ratio).
# ============================================================

# 0) Housekeeping -------------------------------------------------------------
rm(list = ls())
set.seed(123)

# 1) Simulate a realistic dataset --------------------------------------------
url<-"https://raw.githubusercontent.com/niamhmimnagh/FGLM01---Introduction-to-Generalised-Linear-Models/refs/heads/main/data/birds.txt"
birds <- read.table(url, header = TRUE, sep = "\t")

head(birds)

# Quick check: mean increases with vegetation?
tapply(birds$Nests, cut(birds$VegDensity, breaks = 5), mean)

# 2) Visualise raw data -------------------------------------------------------
# Jitter the counts so overlapping points are visible
plot(birds$VegDensity, jitter(birds$Nests, amount = 0.15),
     pch = 19, col = "grey40",
     xlab = "Vegetation density (% cover)",
     ylab = "Number of nests (jittered)",
     main = "Bird nests vs vegetation density")


# 3) Fit a Poisson GLM (log link) -------------------------------------------
fit_pois <- glm(Nests ~ VegDensity, family = poisson(link = "log"),
                data = birds)

summary(fit_pois)
# Interpretation (read alongside summary):
# - (Intercept) = -0.314: This is the log of the expected number of nests
#   when vegetation density is 0%.  exp(-0.314) ≈ 0.73 nests on average at 0% cover.
#
# - VegDensity = 0.0204: For each +1% increase in vegetation density, the log
#   of the expected nest count increases by 0.0204. On the original (count) scale,
#   exp(0.0204) ≈ 1.021, meaning about a 2.1% increase in the expected number
#   of nests per additional 1% vegetation cover.
#
# - The very small p-value (< 2e-16) for VegDensity provides strong evidence
#   that vegetation density is positively associated with nest counts.
#
# - Model deviance dropped from 305.93 (null model) to 194.11, suggesting that
#   vegetation density explains a substantial amount of variation in nest numbers.
#
# - The dispersion parameter is 1, as assumed for a Poisson model, indicating
#   no strong evidence of overdispersion in this fit.


# Report incidence-rate ratio (IRR) with 95% CI
irr <- exp(coef(fit_pois)["VegDensity"])
ci_log <- confint(fit_pois, parm = "VegDensity")      # CI on log scale
irr_ci <- exp(ci_log)
irr; irr_ci
# Interpretation:
#   - irr = exp(beta1): multiplicative change in expected nests per +1% cover.
# IRR = 1.0206 means that for each +1% increase in vegetation cover, the expected number of nests is
# multiplied by 1.0206, i.e., increases by about 2.06%.
# The 95% confidence interval (1.0167, 1.0247) tells us that the true increase per 1% vegetation cover 
# is likely between 1.67% and 2.47%.
# If we compare two sites that differ by 1% in vegetation cover, we expect the site with more vegetation 
# to have about 2% more bird nests, on average, holding everything else constant. This relationship is very 
# unlikely to be due to random chance, given the narrow confidence interval and extremely small p-value.

# 4) Predictions at specific vegetation levels -------------------------------
new_sites <- data.frame(VegDensity = c(10, 30, 60, 90))

# Mean predictions + 95% CI (again via link scale -> response)
p_link <- predict(fit_pois, newdata = new_sites, type = "link", se.fit = TRUE)
pred_mean <- exp(p_link$fit)

pred_table <- data.frame(
  VegDensity = new_sites$VegDensity,
  Mean_nests = round(pred_mean, 2)
)
pred_table
# Interpretation:
# - 'Mean_nests' is the expected number of nests at that vegetation level.
# "0.90 nests" doesn’t mean we expect less than one physical nest to be sitting there — 
# you can’t have 0.9 of a nest in real life.
# It means: If we had many sites with vegetation density = 10%, the average number of nests per site
# would be 0.90.
# Some of those sites would have 0 nests, some would have 1 or more, but the average across all sites 
# would be about 0.9.

# 6) Simple overdispersion check ---------------------------------------------
# Poisson assumes Var(Y) = E(Y). If residual variance >> mean, we see overdispersion.
pearson_chisq <- sum(residuals(fit_pois, type = "pearson")^2) # these are pearsons residuals squared and summed,
# which gives the chi squared statistic. this is a measure of how far the data deviates from model assumptions
dispersion    <- pearson_chisq / df.residual(fit_pois)
# Dividing by residual degrees of freedom standardises the statistic by the model’s residual degrees of freedom. The result is called the dispersion parameter.
# pearson chi squared is a big number that depends on sample size. dividing it by residual df (# obs - # params) standardises it to your dataset size
dispersion
# Rule of thumb:
#   ~1   -> OK for Poisson
#   >1.5 -> potential overdispersion (consider quasi-Poisson or negative binomial)




# Joint Binomial/Poisson Example ------------------------------------------

# ============================================================
# Case study: Presence (binomial/logit) and Abundance (Poisson/log)
# ------------------------------------------------------------
# Data fields per site:
#   presence (0/1), nests (count), veg (% cover), water (m), preds (predator index)
# Questions:
#   A) Which site features influence presence vs absence?
#   B) Given presence, how do features affect nest counts?

# ============================================================

rm(list = ls())


# ------------------------------------------------------------
# Step 1: Explore Your Data  
# ------------------------------------------------------------
url<-"https://raw.githubusercontent.com/niamhmimnagh/FGLM01---Introduction-to-Generalised-Linear-Models/refs/heads/main/data/wetlands.txt"
wetlands <- read.table(url, header = TRUE, sep = "\t")
head(wetlands)
# --- EDA (as per Step 1 slide) ---
summary(wetlands)                        # quick ranges / missingness
# Histograms to see distributions and zero-inflation in counts
par(mfrow = c(2,3))
hist(wetlands$presence, main = "Presence (0/1)", xlab = "", col = "grey80")
hist(wetlands$nests, main = "Nest counts", xlab = "", col = "grey80")
hist(wetlands$veg, main = "Vegetation %", xlab = "", col = "grey80")
hist(wetlands$water, main = "Distance to water (m)", xlab = "", col = "grey80")
hist(wetlands$preds, main = "Predator index", xlab = "", col = "grey80")
par(mfrow = c(1,1))

# Quick relationships (scatter + jitter for binary/count outcomes)
par(mfrow = c(1,3))
plot(wetlands$veg,    jitter(wetlands$presence, 0.06), pch=19, col="#666666",
     xlab="% vegetation cover", ylab="Presence (jittered)", main="Presence ~ veg")
plot(wetlands$water,  jitter(wetlands$presence, 0.06), pch=19, col="#666666",
     xlab="Distance to water (m)", ylab="Presence (jittered)", main="Presence ~ water")
plot(wetlands$preds,  jitter(wetlands$presence, 0.06), pch=19, col="#666666",
     xlab="Predator index", ylab="Presence (jittered)", main="Presence ~ preds")
par(mfrow = c(1,1))

# Interpretation (EDA):
# - Response types align with GLM families: presence is binary; nests are counts.
# - We expect positive effect of veg; negative effects of distance & predators.
# - Nests show many zeros (absent sites) and variance rising with mean -> typical count data.

# ------------------------------------------------------------
# Step 2: Choose the Model Family 
# ------------------------------------------------------------
# - Presence/absence -> binomial family with logit link
# - Nest counts (given presence) -> Poisson family with log link
# (If we later detect overdispersion in counts, consider quasi-Poisson or Negative Binomial.)

# ------------------------------------------------------------
# Step 3: Fit the Model
# ------------------------------------------------------------
# 3A) Logistic regression (presence)
fit_occ <- glm(presence ~ veg + water + preds,
               family = binomial(link = "logit"), data = wetlands)
summary(fit_occ)

# 3B) Poisson regression (abundance, restrict to sites where present)
dat_present <- subset(wetlands, presence == 1)
fit_abund <- glm(nests ~ veg + water + preds,
                 family = poisson(link = "log"), data = dat_present)
summary(fit_abund)

# ------------------------------------------------------------
# Step 4: Interpret the Output
# ------------------------------------------------------------
# Logistic: coefficients are on the log-odds scale -> exponentiate to odds ratios
or     <- exp(coef(fit_occ))
or_ci  <- exp(confint(fit_occ))
round(cbind(OR = or, `2.5%` = or_ci[,1], `97.5%` = or_ci[,2]), 3)

# Interpretation (presence):
# - OR > 1 means the predictor increases the odds of presence; OR < 1 decreases odds.
# - Example sentence template:
#   "Each +10% veg increases the odds by OR^10; or roughly ((OR^10 - 1)*100)%."

# Poisson: coefficients are on the log mean-count scale -> exponentiate to IRRs
irr    <- exp(coef(fit_abund))
irr_ci <- exp(confint(fit_abund))
round(cbind(IRR = irr, `2.5%` = irr_ci[,1], `97.5%` = irr_ci[,2]), 3)

# Interpretation (abundance given presence):
# - IRR = multiplicative change in expected nests per 1-unit increase in predictor.
# - Example: IRR = 1.015 -> ~1.5% more nests per +1 unit (≈ % cover point or 1 m if water).

# ------------------------------------------------------------
# Step 5: Make Predictions (and plot them)
# ------------------------------------------------------------
crit <- qnorm(0.975)

# 5A) Presence probability vs vegetation (water/preds fixed at medians)
veg_seq <- seq(0, 100, length.out = 300)
med_water <- median(wetlands$water); med_preds <- median(wetlands$preds)
nd_occ <- data.frame(veg = veg_seq, water = med_water, preds = med_preds)
pred_occ_link <- predict(fit_occ, newdata = nd_occ, type = "link", se.fit = TRUE)
p_hat <- plogis(pred_occ_link$fit)
p_lwr <- plogis(pred_occ_link$fit - crit * pred_occ_link$se.fit)
p_upr <- plogis(pred_occ_link$fit + crit * pred_occ_link$se.fit)

plot(veg_seq, p_hat, type = "l", lwd = 3,
     xlab = "% vegetation cover", ylab = "Predicted presence probability",
     main = "Logistic GLM: presence ~ vegetation")
polygon(c(veg_seq, rev(veg_seq)), c(p_lwr, rev(p_upr)),
        border = NA, col = rgb(0,0,0,0.12))
lines(veg_seq, p_hat, lwd = 3)
# Interpretation: Curve shows how the *mean* probability of presence changes with veg.
# The shaded ribbon is a 95% CI for the mean (not individual sites).

# 5B) Expected nests vs vegetation (given presence; other vars at medians)
nd_ab <- data.frame(veg = veg_seq,
                    water = median(dat_present$water),
                    preds = median(dat_present$preds))
pred_ab_link <- predict(fit_abund, newdata = nd_ab, type = "link", se.fit = TRUE)
mu_hat <- exp(pred_ab_link$fit)
mu_lwr <- exp(pred_ab_link$fit - crit * pred_ab_link$se.fit)
mu_upr <- exp(pred_ab_link$fit + crit * pred_ab_link$se.fit)

plot(veg_seq, mu_hat, type = "l", lwd = 3,
     xlab = "% vegetation cover", ylab = "Expected nests (given presence)",
     main = "Poisson GLM: nests ~ vegetation (given present)")
polygon(c(veg_seq, rev(veg_seq)), c(mu_lwr, rev(mu_upr)),
        border = NA, col = rgb(0,0,0,0.12))
lines(veg_seq, mu_hat, lwd = 3)
# Interpretation: Expected nest count rises with veg; ribbon is a 95% CI for the mean.

# Scenario predictions (management-friendly table)
scen <- expand.grid(veg = c(20, 50, 80),
                    water = c(200, 600, 1000),
                    preds = c(1, 3))
# Presence probs
sc_occ <- predict(fit_occ, newdata = scen, type = "link", se.fit = TRUE)
scen$p_presence <- plogis(sc_occ$fit)
scen$p_lwr <- plogis(sc_occ$fit - crit*sc_occ$se.fit)
scen$p_upr <- plogis(sc_occ$fit + crit*sc_occ$se.fit)
# Expected nests (given presence)
sc_ab <- predict(fit_abund, newdata = scen, type = "link", se.fit = TRUE)
scen$mu_nests <- exp(sc_ab$fit)
scen$mu_lwr   <- exp(sc_ab$fit - crit*sc_ab$se.fit)
scen$mu_upr   <- exp(sc_ab$fit + crit*sc_ab$se.fit)

print(within(scen, {
  p_presence <- round(p_presence, 3); p_lwr <- round(p_lwr, 3); p_upr <- round(p_upr, 3)
  mu_nests <- round(mu_nests, 2);     mu_lwr <- round(mu_lwr, 2); mu_upr <- round(mu_upr, 2)
})[, c("veg","water","preds","p_presence","p_lwr","p_upr","mu_nests","mu_lwr","mu_upr")],
row.names = FALSE)

# Interpretation:
# - Combine presence and abundance if you want expected nests *per site overall*:
#   expected_overall = p_presence * mu_nests (approximation).

# ------------------------------------------------------------
# Step 6: Check Model Diagnostics
# ------------------------------------------------------------
# Logistic: binned residuals (should hover near 0 without pattern)
f <- fitted(fit_occ); r <- residuals(fit_occ, type="response")
bins <- cut(f, breaks = quantile(f, probs = seq(0,1,length=21)), include.lowest = TRUE)
bin_fit <- tapply(f, bins, mean); bin_res <- tapply(r, bins, mean)
plot(bin_fit, bin_res, pch=19,
     xlab="Mean fitted probability (bin)", ylab="Mean residual (bin)",
     main="Logistic GLM: binned residuals"); abline(h=0, lty=2)

# Optional: AUC/ROC (uncomment to compute)
# if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
# library(pROC)
# roc_obj <- roc(wetlands$presence, fitted(fit_occ)); auc(roc_obj)

# Poisson: simple overdispersion check
pearson_chisq <- sum(residuals(fit_abund, type = "pearson")^2)
dispersion <- pearson_chisq / df.residual(fit_abund)
dispersion
# Rule of thumb: ~1 good; >1.5 suggests overdispersion -> try quasi-Poisson or NegBin.
# Optional NegBin:
# if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")
# fit_nb <- MASS::glm.nb(nests ~ veg + water + preds, data = dat_present)
# summary(fit_nb)



# ------------------------------------------------------------
# Step 7: Communicate Results Clearly 
# ------------------------------------------------------------
# Presence :
b1_or <- exp(coef(fit_occ)["veg"])
cat("\nPresence (logistic):\n",
    sprintf("Each +10%% in vegetation cover multiplies the odds of presence by %.2f (≈ %d%% increase).\n",
            b1_or^10, round((b1_or^10 - 1)*100)), sep = "")
# Abundance :
g1_irr <- exp(coef(fit_abund)["veg"])
cat("Abundance (Poisson, given present):\n",
    sprintf("Each +10%% in vegetation cover multiplies the expected nest count by %.2f (≈ %d%% increase).\n",
            g1_irr^10, round((g1_irr^10 - 1)*100)), sep = "")



