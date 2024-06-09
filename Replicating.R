# Date: 2024-05-28

# Setup
# Loading Libraries
library(foreign)
library(quantreg)
library(ggplot2)
library(dplyr)

# Setting Working Directory and Loading Data
setwd("~/Desktop/Misc Projects/Happiness")
# set up working directory, you will need to change into your own.
dat <- read.dta("2012_hse_and_shes_combined.dta") # load the data
subd <- subset(dat, eqvinc > 0 & wemwbs > 0)

# Median Regression
# Count observations above and below eqvinc = 50,000
subd$income_group <- ifelse(subd$eqvinc > 50000, "Above £50k", "Below £50k")
table(subd$income_group)

# Changing the Reference Category
subd$income_group <- as.factor(subd$income_group)
subd$income_group <- relevel(subd$income_group, ref = "Below £50k")

# Global Model with Interactions
model_basic <- rq(formula = wemwbs ~ log(eqvinc) * income_group, tau = 0.5, data = subd)
print(model_basic)
print(summary(model_basic))

# Difference in medians of happiness at household incomes of £15,000 and £250,000
wellbeing_high <- 26.53139 + 11.74048 + (2.54601 - 1.13046) * log(250000)
wellbeing_low <- 26.53139 + 2.54601 * log(15000)
wellbeing_diff <- wellbeing_high - wellbeing_low
print(wellbeing_diff)

# Getting Standardised Coefficients
# Standardising the Data
data_standardised <- subd %>%
  mutate(
    wemwbs_scaled = scale(wemwbs),
    log_eqvinc_scaled = scale(log(eqvinc))
  )

model_standardised <- rq(formula = wemwbs_scaled ~ log_eqvinc_scaled * income_group, tau = 0.5, data = data_standardised)
print(summary(model_standardised))
data_standardised$predicted <- predict(model_standardised, data = data_standardised)

# Getting Coefficients and Plotting
# Dataframe for metrics
slope_rich <- 0.22813 - 0.10129
intercept_rich <- 0.10562 + 0.03125

metrics_low_standardized <- data.frame(
  slope = 0.22813,
  intercept = 0.10562,
  slope_t_value = 11.54772
)

metrics_high_standardized <- data.frame(
  slope = slope_rich,
  intercept = intercept_rich,
  interaction_t_value = -1.91751
)

subd <- subd %>%
  mutate(predicted = predict(model_basic, newdata = subd))

adjust <- -0.5

# Create the plot
plot1 <- ggplot(data_standardised, aes(x = eqvinc, y = wemwbs_scaled)) +
  geom_point(size = 0.5, shape = 16, color = "grey", alpha = 0.4) +
  geom_line(aes(y = predicted), size = 1, color = "blue") +
  geom_vline(xintercept = 50000, linetype = "dashed", color = "black") +
  annotate("text", x = 50000, y = max(data_standardised$wemwbs_scaled), label = "£50,000", color = "black", angle = 90, vjust = -0.5, size = 5.5) +
  annotate("text", x = 10000, y = metrics_low_standardized$intercept + metrics_low_standardized$slope * log(1) + adjust,
           label = paste0('atop(bold("Slope = ', round(metrics_low_standardized$slope, 2),
                          '"), "\nT-Score = ', round(metrics_low_standardized$slope_t_value, 2), '")'),
           color = "black", size = 5, hjust = 0, parse = TRUE) +
  annotate("text", x = 65000, y = metrics_high_standardized$intercept + metrics_high_standardized$slope * log(1),
           label = paste0('atop(bold("Slope = ', round(metrics_high_standardized$slope, 2),
                          '"), "\nInteraction T-Score = ', round(metrics_high_standardized$interaction_t_value, 2), '")'),
           color = "black", size = 5, hjust = 0, parse = TRUE) +
  theme_bw() +
  labs(x = "Income (£)", y = "Well-being Scale (Z-Scored)") +
  scale_x_continuous(trans = "log10", breaks = scales::log_breaks(n = 8)) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.position = "none")  # Remove legend

# Display the plot
print(plot1)

# Percentile Regression
# First, with the same percentiles as KK (2022).
taus <- c(0.15, 0.3, 0.5, 0.7, 0.85)

models <- rq(formula = wemwbs ~ log(eqvinc) * income_group, tau = taus, data = subd)

# t-values
slope_t_values <- lapply(summary(models), function(x) x$coefficients[, "t value"])
slope_t_values_df <- do.call(rbind, slope_t_values)

# t-values for slope
# Create the logical indicator for "Above £50k"
subd$below_50k <- as.integer(subd$income_group != "Above £50k")
table(subd$below_50k)

models_b <- rq(wemwbs ~ log(eqvinc) * below_50k, tau = taus, data = subd)
slope_above_t_values <- lapply(summary(models_b), function(x) x$coefficients[, "t value"])
slope_above_t_values_df <- do.call(rbind, slope_above_t_values)

# We want the coefficients on log(eqvinc) and the interaction
metrics_low <- data.frame(
  intercept = coef(models)[1, ],
  slope_coef = coef(models)[2, ],
  slope_t_values = slope_t_values_df[, 2],
  income_group = "Below £50k"
)

metrics_high <- data.frame(
  intercept = coef(models)[1, ] + coef(models)[3, ],
  slope_coef = coef(models)[2, ] + coef(models)[4, ],
  slope_above_t_values = slope_above_t_values_df[, 2],
  income_group = "Above £50k"
)

print(metrics_low)

# Plotting
# Generate plot
offset_high <- c(-2, -1.5, 0, 2, 2.5)

p1 <- ggplot(NULL, aes(x = eqvinc, y = wemwbs, colour = income_group)) +
  
  # Plot points for income groups
  geom_point(data = subd, size = 0.5, shape = 16, color = "grey", alpha = 0.4) +
  
  # Add quantile regression lines for "Below £60K" income group
  geom_quantile(data = subset(subd, income_group == "Below £50k"), 
                color = "red", alpha = 0.8, quantiles = taus) +
  
  # Add quantile regression lines for "Above £60K" income group
  geom_quantile(data = subset(subd, income_group == "Above £50k"), 
                color = "darkturquoise", alpha = 0.8, quantiles = taus) +
  
  # Label for Income Threshold
  annotate("text", x = 50000, y = max(data_standardised$wemwbs_scaled)+2, label = "£50,000", color = "black", angle = 90, vjust = -0.5, size = 5) +
  
  # Annotations for Below
  annotate("text", x = 10000, y = metrics_low$intercept + metrics_low$slope_coef * log(10000), 
           label = paste0('atop(bold("Slope = ', round(metrics_low$slope_coef, 2), 
                          '"), "\nT-Score = ', round(metrics_low$slope_t_values, 2), '")'), 
           color = "black", size = 5, hjust = 0, parse = TRUE) +
  
  # Add labels for "Above £50K" coefficients
  annotate("text", x = 55000, y = metrics_high$intercept + metrics_high$slope_coef * log(55000) + offset_high,
           label = paste0('atop(bold("Slope = ', round(metrics_high$slope_coef, 2),
                          '"), "\nSlope t = ', round(metrics_high$slope_above_t_values, 2), '")'),
           color = "black", size = 4, hjust = 0, parse = TRUE) +
  # Labels for Percentiles 
  annotate("text", x = 600, y = metrics_low$intercept + metrics_low$slope_coef*log(1000), 
           label = paste0('atop(bold("Percentile: ', taus*100, '"))'), 
           color = "black", size = 5, hjust = 0, parse = TRUE) +
  
  # Add a dashed vertical line at £50,000
  geom_vline(xintercept = 50000, linetype = "dashed", color = "black", alpha = 0.5) +
  
  # Apply themes and labels
  theme_bw() +
  labs(x = "Income (£)", y = "WEMWBS Well-being Scale") + 
  scale_x_continuous(trans = "log10", breaks = scales::log_breaks(n = 8)) + 
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 20),
  )

p1


# Table
# For Slopes Below £50k, and Interaction Effects
taus0<-c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.5, 0.7, 0.85) 

abs1  <- rq(formula = wemwbs ~ log(eqvinc) * income_group,tau = taus0,
            data = subd)
summary(abs1)

# For Slopes for Above 60k
# Create the logical indicator for "Above £60k"
subd$below_50k <- as.integer(subd$income_group != "Above £50k")

abs2 <- rq(wemwbs ~ log(eqvinc) * below_50k, tau = taus0, data = subd)
summary(abs2)