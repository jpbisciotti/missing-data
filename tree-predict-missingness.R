# ------------------------------------------------------------------------------
# Decision Tree Analysis: Build classification trees to predict missingness
# ------------------------------------------------------------------------------

# Parameter Preparation

airquality <- tibble::as_tibble(datasets::airquality)
data <- airquality

vars_with_missingness <- names(data)[colSums(is.na(data)) > 0]
vars_with_missingness

var_target <- "Ozone"
suffix <- "_missing"
var_outcome <- paste0(var_target, suffix)

# ------------------------------------------------------------------------------

# Data Preparation

# Prepare data for missing data analysis
# Create binary indicator for missingness of target variable
data_analysis <- data |>
  # Create the target missingness indicator
  # Create missingness indicators for other variables (potential predictors)
  dplyr::mutate(dplyr::across(tidyselect::where(~ any(is.na(.x))), ~ ifelse(is.na(.x), "missing", "not"), .names = "{.col}{suffix}"))

# ------------------------------------------------------------------------------

# Building the Classification Tree

# Get all variables except the target and its missing indicator
vars_predictors <- setdiff(colnames(data), c(var_target, var_outcome))
mod_formula <- as.formula(paste(var_outcome, "~", paste(vars_predictors, collapse = " + ")))

# Build the tree
mod_fit <- rpart::rpart(
  formula = mod_formula,
  data = data_analysis,
  method = "class",
  control = rpart::rpart.control(
    maxdepth = 4,
    minsplit = 20,
    minbucket = round(min_split/3),
    cp = 0.01  
  ))

# ------------------------------------------------------------------------------

# Interpreting the Tree Results

# Get variable importance
var_importance <- mod_fit$variable.importance
var_importance

# Get complexity parameter table
cp_table <- mod_fit$cptable
cp_table

# Get the rules
rules <- rpart.plot::rpart.rules(mod_fit, style = "wide")
rules

# Calculate overall accuracy
mod_pred <- predict(mod_fit, type = "class")
actual <- data_analysis[[var_outcome]]  # First column is the target
accuracy <- mean(mod_pred == actual)
accuracy

mod_interpretation <- list(
  importance = var_importance,
  rules = rules,
  accuracy = accuracy,
  cp_table = cp_table
)

# ------------------------------------------------------------------------------

# Additional Advanced Visualization

# Simple tree plot
rpart.plot::rpart.plot(
  mod_fit, 
  main = "Missing Data Pattern Tree - Simple View",
  type = 3,
  extra = 2,
  fallen.leaves = TRUE
)

# Detailed tree plot with percentages
rpart.plot::rpart.plot(
  mod_fit,
  main = "Missing Data Pattern Tree - Simple View", 
  type = 4,
  extra = 2,
  digits = 3,
  fallen.leaves = TRUE
)

# Plot with partykit for better visualization
plot(
  partykit::as.party(mod_fit), 
  main = "Missing Data Pattern Tree -  Party Plot",
  gp = grid::gpar(fontsize = 8),
  tp_args = list(text = "vertical")
)

# Variable importance plot
barplot(
  sort(mod_interpretation$importance, decreasing = TRUE),
  main = "Variable Importance",
  las = 2,
  cex.names = 0.8,
  col = "lightblue")

# ------------------------------------------------------------------------------
