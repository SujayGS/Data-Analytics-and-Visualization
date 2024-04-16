library(dplyr)

data <- read.csv("dataset1_K2201621-1.csv")
head(data)

# Descriptive statistics for each tenure category using dplyr
summary_stats <- data %>%
  group_by(tenure) %>%
  summarise(
    Age_Mean = mean(age),
    Income_Mean = mean(income),
    Education_Mode = names(sort(table(education), decreasing = TRUE)[1])
  )

summary_stats

# Install and load the ggplot2 package if not already installed
# install.packages("ggplot2")
library(ggplot2)

# Load the dataset
data <- read.csv("dataset1_K2201621-1.csv")

# Convert variables to factors with specific levels
data$tenure <- factor(data$tenure, levels = c(1, 2, 3, 4), labels = c("Family", "Owner", "Private Rent", "Social Rent"))
data$education <- factor(data$education, levels = c(2, 3, 1), labels = c("Primary", "Secondary", "Higher"))

# Create a clustered bar chart using ggplot2
ggplot(data, aes(x = tenure, fill = education)) +
  geom_bar(position = "dodge", stat = "count") +
  scale_fill_manual(values = c("blue", "red", "green")) +
  labs(x = "Tenure", y = "Count", fill = "Education") +
  theme_minimal()



# Sample data
set.seed(123)  # for reproducibility
data <- data.frame(
  vote = sample(c(0, 1), 100, replace = TRUE),
  tenure = factor(sample(1:4, 100, replace = TRUE), labels = c("Family", "Owner", "Private Rent", "Social Rent")),
  education = factor(sample(1:3, 100, replace = TRUE), labels = c("Higher", "Primary", "Secondary"))
)
---------------------------------------------------
data$tenure <- as.factor(data$tenure) 
data$education <- as.factor(data$education) 

legend_labels <- c("Primary", "Secondary", "Higher")

# Create a clustered bar chart
barplot(table(data$education, data$tenure),
        beside = TRUE,
        col = c("lightblue", "lightgreen", "lightcoral"),
        legend.text = TRUE,
        args.legend = list(x = "topleft", bty = "n", inset = c(0, -0.1)),
        main = "Clustered Bar Chart of Tenure and Education",
        xlab = "Tenure",
        ylab = "Count")

barplot(table(data$education, data$tenure),
        beside = TRUE,
        col = c("lightblue", "lightgreen", "lightcoral"),
        main = "Clustered Bar Chart of Tenure and Education",
        xlab = "Tenure",
        ylab = "Count")
legend("topleft", legend = legend_labels, fill = c("lightblue", "lightgreen", "lightcoral"), title = "Education")


----------------------------------------------------------------------------------------------------------

# Assuming your dataset is named <YOUR K-NUMBER>dataset1.csv
# Load the required libraries
library(ggplot2)

# Load the dataset

# Scatterplot with age on the x-axis and income on the y-axis
plot(x = data$age, y = data$income, xlab = "Age", ylab = "Income", main = ' Spline Curves for each Education Category')

# Spline curves for each tenure category
primary_spline <- smooth.spline(x = data$age[data$education == 'Primary'], y = data$income[data$education == 'Primary'], spar = 0.7)
secondary_spline <- smooth.spline(x = data$age[data$education == 'Secondary'], y = data$income[data$education == 'Secondary'], spar = 0.7)
higher_spline <- smooth.spline(x = data$age[data$education == 'Higher'], y = data$income[data$education == 'Higher'], spar = 0.7)

# Plot the spline curves
lines(primary_spline$x, primary_spline$y, col = 'purple', lwd = 2)
lines(secondary_spline$x, secondary_spline$y, col = 'orange', lwd = 2)
lines(higher_spline$x, higher_spline$y, col = 'green', lwd = 2)

# Save the plot as an image (adjust the filename and format as needed)
dev.copy(png, "scatterplot_with_splines.png")
dev.off()



# Add legend
legend("topleft", legend = c("Primary", "Secondary", "Higher"), 
       col = c("purple", "orange", "green"), lty = 1, lwd = 2, cex = 0.8,title = "Education")




  # Assuming your dataset is named <YOUR K-NUMBER>dataset1.csv
  # Load the required libraries
library(ggplot2)
library(MASS)  # Required for the 'glm' function
library(marginaleffects)
library(margins)

# Convert categorical variables to factors
data$tenure <- factor(data$tenure, levels = c("Own", "Fam", "Pri", "Soc"))
data$education <- factor(data$education, levels = c("Secondary", "Primary", "Higher"))

# Fit logistic regression model
model <- glm(vote ~ age + income + tenure + education, data = data, family = "binomial")
# Show coefficients and confidence intervals
summary(model)

model_1 <- glm(vote ~ income+ tenure + education, data = data, family = "binomial")
summary(model_1)

marginal_effects <- marginaleffects(model)
# Show marginal effects
summary(marginal_effects)

# Calculate marginal effects
marginal_effects <- marginaleffects(model_1)
# Show marginal effects
summary(marginal_effects)

marginal_effects <- margins(model)
summary(marginal_effects)

marginal_effects <- margins(model_1)
summary(marginal_effects)

conf_intervals <- confint(model_1)
print(conf_intervals)

coefficients_table <- coef(model_1)
confidence_intervals <- confint(model_1)

confidence_intervals <- confint(model)
confidence_intervals

marginal_effects <- margins(model_1, variables = "tenure")
marginal_effects

# Display the extracted tables
coefficients_table
confidence_intervals


# Calculate marginal effects
meffects <- marginal_effects(model_1)
# Display marginal effects and confidence intervals
summary(meffects)
plot(meffects)


log_odds <- predict(model_1, type = "response")

# Adding the predicted log-odds to the dataset
dataset$log_odds <- log_odds
print(dataset)

dataset3 <- read.csv("dataset2_K2201621-3.csv")

# Convert categorical variables to factors

# Predict log-odds for dataset2
log_odds <- predict(model_1, newdata = dataset3, type = "response")
dataset3$new_pred_prob <- log_odds
dataset3$difference <- dataset3$new_pred_prob - dataset3$predprob

# Transform log-odds to probabilities
predicted_probabilities <- plogis(log_odds)
predicted_probabilities

# Display the results
result <- data.frame(LogOdds = log_odds, Probability = predicted_probabilities)
print(result)

# Assuming 'your_data_frame' is the name of your data frame
write.csv(dataset3, file = "output_file.csv", row.names = TRUE)


summary(model_1)




#Task5

# Extract coefficients and standard errors
coef_table <- summary(model_1)$coef

# Extract log odds, odds ratios, and confidence intervals
log_odds <- coef_table[, "Estimate"]
odds_ratios <- exp(log_odds)
lower_ci_odds <- exp(log_odds - 1.96 * coef_table[, "Std. Error"])
upper_ci_odds <- exp(log_odds + 1.96 * coef_table[, "Std. Error"])

# Combine results into a data frame
result_df <- data.frame(
  Predictor = rownames(coef_table),
  LogOdds = log_odds,
  OddsRatio = odds_ratios,
  LowerCIOdds = lower_ci_odds,
  UpperCIOdds = upper_ci_odds
)

# Display results
print(result_df)
# Assuming 'model' is your logistic regression model
coefficients <- coef(model_1)
conf_intervals <- confint(model_1)

# Combine coefficients and confidence intervals into a data frame
result_df <- data.frame(coef = coefficients, conf_int = conf_intervals)

# Print the result_df
print(result_df)





















summary(model)

# Assuming 'model' is your logistic regression model
library(marginaleffects)

# Compute marginal effects
marginal_effects <- marginaleffects(model_1)

# Display marginal effects with confidence intervals
summary(marginal_effects)














