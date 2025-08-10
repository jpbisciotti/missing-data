# ------------------------------------------------------------------------------
# Decision Tree Analysis: Build classification trees to predict missingness
# ------------------------------------------------------------------------------

# Parameter Preparation

data <- airquality

vars_with_missingness <- names(data)[colSums(is.na(data)) > 0]
vars_with_missingness

var_target <- "Ozone_cat"
suffix <- "_missing"
var_outcome <- paste0(var_target, suffix)

# ------------------------------------------------------------------------------

# Creating categorical variables with kmeans for sensitivity analysis later

tb_categorical_ozone <- tibble::tibble(
  Ozone = na.omit(data$Ozone), 
  Ozone_cat = kmeans(na.omit(data$Ozone), centers = 5, iter.max = 25)$cluster
) |> 
  dplyr::arrange(Ozone_cat, Ozone) |> 
  dplyr::distinct()

tb_categorical_solar <- tibble::tibble(
  Solar.R = na.omit(data$Solar.R), 
  solar_cat = kmeans(na.omit(data$Solar.R), centers = 5, iter.max = 25)$cluster
) |> 
  dplyr::arrange(solar_cat, Solar.R) |> 
  dplyr::distinct()

tb_categorical_wind <- tibble::tibble(
  Wind = na.omit(data$Wind), 
  wind_cat = kmeans(na.omit(data$Wind), centers = 5, iter.max = 25)$cluster
) |> 
  dplyr::arrange(wind_cat, Wind) |> 
  dplyr::distinct()

tb_categorical_temp <- tibble::tibble(
  Temp = na.omit(data$Temp), 
  temp_cat = kmeans(na.omit(data$Temp), centers = 5, iter.max = 25)$cluster
) |> 
  dplyr::arrange(temp_cat, Temp) |> 
  dplyr::distinct()

tbs_cat <- list(
  "Ozone" = tb_categorical_ozone,
  "Solar.R" = tb_categorical_solar,
  "Wind" = tb_categorical_wind,
  "Temp"= tb_categorical_temp
)

# ------------------------------------------------------------------------------

# Data Preparation
# Prepare data for missing data analysis
# Create binary indicator for missingness of target variable

# Join categorical features
data_analysis <- list(data) |>
  append(tbs_cat) |>
  purrr::reduce(dplyr::left_join) |>
  dplyr::select(-tidyselect::all_of(names(tbs_cat))) |>
  # Create the target missingness indicator
  # Create missingness indicators for other variables (potential predictors)
  dplyr::mutate(dplyr::across(tidyselect::where(~ any(is.na(.x))), ~ ifelse(is.na(.x), "missing", "not"), .names = "{.col}{suffix}"))

# ------------------------------------------------------------------------------

# Building the Classification Tree

# Get all variables except the target and its missing indicator
vars_predictors <- setdiff(colnames(data_analysis), c(var_target, var_outcome))
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
accuracy <- mean(predict(mod_fit, type = "class") == data_analysis[[var_outcome]])
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

# Extract insights from the tree

# Initialize list
insights <- list()

# Identify key predictors of missingness: Top predictors of missingness
top_predictors <- names(head(sort(mod_interpretation$importance, decreasing = TRUE), 3))
insights$key_predictors <- top_predictors
insights$key_predictors

# Analyze patterns for each key predictor
insights$patterns <- list()

insights$patterns <- purrr::map(
  top_predictors, 
  function(predictor) {
    data_analysis |>
      dplyr::group_by(.data[[predictor]], .data[[var_outcome]]) |>
      dplyr::summarize(count = dplyr::n(), .groups = "drop") |>
      dplyr::group_by(.data[[predictor]]) |>
      dplyr::mutate(percentage = round(count / sum(count) * 100, 1)) |>
      dplyr::filter(.data[[var_outcome]] == "missing")
  }) |>
  setNames(top_predictors)

# Pattern for top predictors
insights$patterns

# Calculate predictive performance metrics
# Confusion matrix
conf_matrix <- table(Predicted = predict(mod_fit, type = "class"), Actual = data_analysis[[var_outcome]])
insights$confusion_matrix <- conf_matrix

# Calculate metrics
tp <- conf_matrix["missing", "missing"]
tn <- conf_matrix["not", "not"] 
fp <- conf_matrix["missing", "not"]
fn <- conf_matrix["not", "missing"]

insights$metrics <- list(
  accuracy = (tp + tn) / sum(conf_matrix),
  precision = tp / (tp + fp),
  recall = tp / (tp + fn),
  specificity = tn / (tn + fp)
)

# Performance
tibble::enframe(unlist(insights$metrics), name = "metric")

# ------------------------------------------------------------------------------

# Assess missing data mechanism based on tree results

assessment <- list()

# Criterion 1: Predictability of missingness
accuracy <- insights$metrics$accuracy

if(accuracy < 0.6) {
  assessment$predictability <- "Low - suggests MCAR"
  assessment$mechanism_likely <- "MCAR"
} else if(accuracy < 0.8) {
  assessment$predictability <- "Moderate - suggests MAR"
  assessment$mechanism_likely <- "MAR"
} else {
  assessment$predictability <- "High - suggests strong MAR or potential MNAR"
  assessment$mechanism_likely <- "MAR/MNAR"
}

# Criterion 2: Number of important predictors
n_important <- length(insights$key_predictors)

if(n_important == 0) {
  assessment$complexity <- "No predictors - likely MCAR"
} else if(n_important <= 2) {
  assessment$complexity <- "Few predictors - likely simple MAR"
} else {
  assessment$complexity <- "Many predictors - complex MAR or MNAR"
}

# Criterion 3: Tree depth and complexity
tree_depth <- max(mod_fit$frame$var == "<leaf>")

if(tree_depth <= 2) {
  assessment$pattern_complexity <- "Simple patterns"
} else {
  assessment$pattern_complexity <- "Complex patterns"
}

# Overall assessment
if(assessment$mechanism_likely == "MCAR") {
  assessment$recommendation <- "Consider complete case analysis or simple imputation"
} else if(assessment$mechanism_likely == "MAR") {
  assessment$recommendation <- "Use multiple imputation including identified predictors"
} else {
  assessment$recommendation <- "Investigate further; consider sensitivity analysis"
}

cat(
  "Missing Data Mechanism Assessment:\n",
  "\n",
  "Predictability based on accuracy:", assessment$predictability, "\n",
  "Likely Mechanism based on accuracy:", assessment$mechanism_likely, "\n",
  "Recommendation based on accuracy:", assessment$recommendation, "\n",
  "\n",
  "Complexity based on number of important predictors:", assessment$complexity, "\n",
  "Pattern Complexity based on tree depth:", assessment$pattern_complexity, "\n"
)

# ------------------------------------------------------------------------------

# Sensitivity analysis
# Use tree insights to guide sensitivity analysis

# Analyze missingness patterns by top predictor
sensitivity_results <- purrr::map(
  top_predictors, 
  function(x) {
    data_analysis |>
      dplyr::group_by(.data[[x]]) |>
      dplyr::summarize(
        total_cases = dplyr::n(),
        missing_cases = sum(.data[[var_outcome]] == "missing"),
        missing_rate = round(missing_cases / total_cases * 100, 1),
        .groups = "drop"
      ) |>
      dplyr::mutate(max_diff = max(missing_rate) - min(missing_rate)) |>
      dplyr::mutate(large_diff = max_diff > 20)
  }) |> setNames(top_predictors)

# Sensitivity Analysis by Top Predictor
sensitivity_results

# Large
cat("\nLarge differences in missing rates detected (", max_diff, "% difference)\n",
    "Strong evidence for MAR mechanism\n",
    "Recommendation: Include", best_predvar, "in imputation model\n")

# Not Large
cat("\nModerate differences in missing rates (", max_diff, "% difference)\n",
    "Possible MAR mechanism\n",
    "Recommendation: Consider including", best_predvar, "in imputation model\n")

# ------------------------------------------------------------------------------
