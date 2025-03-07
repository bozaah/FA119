# Load necessary libraries
library(brms)
library(cmdstanr)

# set brms backend
options(brms.backend = "cmdstanr")

# Load paired temperature data
# paired_data

# Fit linear models to get priors for Bayesian models
reg_rectal <- lm(rectalTemp ~ tagTemp, data = paired_data)
reg_ear <- lm(earTemp ~ tagTemp, data = paired_data)

# Extract coefficients as priors
rectal_priors <- coef(summary(reg_rectal))
ear_priors <- coef(summary(reg_ear))

# Define Bayesian models with these priors
rectal_fit <- brm(
  formula = rectalTemp ~ tagTemp, 
  data = paired_data,
  prior = c(
    prior(normal(rectal_priors[1, "Estimate"], abs(rectal_priors[1, "Std. Error"]*10)), class = "Intercept"),
    prior(normal(rectal_priors[2, "Estimate"], abs(rectal_priors[2, "Std. Error"]*10)), class = "b")
  ),
  iter = 8000,
  sample_prior = "yes"
)

ear_fit <- brm(
  formula = earTemp ~ tagTemp, 
  data = paired_data,
  prior = c(
    prior(normal(ear_priors[1, "Estimate"], abs(ear_priors[1, "Std. Error"]*10)), class = "Intercept"),
    prior(normal(ear_priors[2, "Estimate"], abs(ear_priors[2, "Std. Error"]*10)), class = "b")
  ),
  iter = 8000,
  sample_prior = "yes"
)

# Extract Posterior Samples and create summaries
rectal_posterior <- posterior_samples(rectal_fit)
ear_posterior <- posterior_samples(ear_fit)

rectal_summary <- summary(rectal_posterior)
ear_summary <- summary(ear_posterior)

# Print summaries
rectal_summary
ear_summary

# Get coeffs
# Extract summary of the fitted model
rectal_summary <- summary(rectal_fit)
ear_summary <- summary(ear_fit)

# Extract mean coeff values for rectal model
rectal_intercept_mean <- rectal_summary$fixed["Intercept", "Estimate"]
rectal_slope_mean <- rectal_summary$fixed["tagTemp", "Estimate"]

# Extract mean coeff values for ear model
ear_intercept_mean <- ear_summary$fixed["Intercept", "Estimate"]
ear_slope_mean <- ear_summary$fixed["tagTemp", "Estimate"]

# Printing extracted coefficients
cat("Rectal Model - Intercept:", rectal_intercept_mean, "Slope:", rectal_slope_mean, "\n")
cat("Internal Ear Model - Intercept:", ear_intercept_mean, "Slope:", ear_slope_mean, "\n")

# Calibrated XioT tag temperatures
# Function to apply calibration coefficients to tag temp
apply_calibration <- function(tag_temp, 
                              calib_intercept, 
                              calib_slope, 
                              is_rectal) {
  
  if (is_rectal) {
    # Calculate estimated rectal temperature
    estimated_temp <- calib_intercept + (calib_slope * tag_temp)
  } else {
    # Calculate estimated ear temperature
    estimated_temp <- calib_intercept + (calib_slope * tag_temp)
  }
  
  return(estimated_temp)
}

# Run estimation
rectal_intercept <- 36.5  
rectal_slope <- 0.95      
ear_intercept <- 35.5    
ear_slope <- 1.02       

#  Apply calibration
new_tag_temp <- 37.8

# Apply calibration
calibrated_temps <- apply_calibration(new_tag_temp, 
                                      rectal_intercept, 
                                      rectal_slope, 
                                      ear_intercept, 
                                      ear_slope)
calibrated_temps

