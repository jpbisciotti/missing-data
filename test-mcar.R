# ------------------------------------------------------------------------------

# Testing for MCAR with Little's MCAR Test. The primary statistical test for
# MCAR is Little's test. This tests the null hypothesis that data are MCAR. It
# uses chi-square statistics to compare observed and expected missing data
# patterns. An important limitation is Little's test can only distinguish MCAR
# from non-MCAR, but cannot distinguish between MAR and MNAR.
# 
# If p-value > 0.05, you fail to reject MCAR (data may be MCAR)
# If p-value ≤ 0.05, reject MCAR (data are likely MAR or MNAR)

# ------------------------------------------------------------------------------

airquality <- tibble::as_tibble(datasets::airquality)
oceanbuoys <- tibble::as_tibble(naniar::oceanbuoys)

sum(is.na(airquality)) # 44
sum(is.na(airquality)) / prod(dim(airquality)) # 0.048

sum(is.na(oceanbuoys)) # 177
sum(is.na(oceanbuoys)) / prod(dim(oceanbuoys)) # 0.030

# p.value = 0.00142 -> reject MCAR (data are likely MAR or MNAR)
naniar::mcar_test(airquality) 

# p.value = 0 -> reject MCAR (data are likely MAR or MNAR)
naniar::mcar_test(oceanbuoys) 

# ------------------------------------------------------------------------------

# MCAR algo to replace with NA

iris <- tibble::as_tibble(datasets::iris)
iris_new <- iris
id_row <- NULL
id_col <- NULL
for (id_row in seq_len(nrow(iris_new))) {
  for (id_col in seq_len(ncol(iris_new))) {
    value_old <- iris_new[id_row, id_col]
    set.seed(id_row * id_col)
    value_new <- sample(x = c(NA, value_old), size = 1, replace = FALSE, prob = c(0.04, 0.96))
    iris_new[id_row, id_col] <- value_new
  }
}

iris_new_1 <- iris_new

sum(is.na(iris_new_1)) # 41
sum(is.na(iris_new_1)) / prod(dim(iris_new_1)) # 0.055

# p.value = 0.887 -> you fail to reject MCAR (data may be MCAR)
naniar::mcar_test(iris_new) 

# ------------------------------------------------------------------------------

# Non-MCAR algo to replace with NA

iris_new <- tibble::as_tibble(iris) |> 
  dplyr::arrange(Species, Petal.Length * Sepal.Length)
id_row <- NULL
id_col <- NULL
for (id_row in seq_len(nrow(iris_new))) {
  for (id_col in seq_len(ncol(iris_new))) {
    value_old <- iris_new[id_row, id_col]
    prob_missing <- pmin((4.5 / (id_row * id_col)), 1)
    set.seed(id_row)
    value_new <- sample(x = c(NA, value_old), size = 1, replace = FALSE, prob = c(prob_missing, 1 - prob_missing))
    iris_new[id_row, id_col] <- value_new
  }
}

iris_new_2 <- iris_new

# p.value = 0.00475 -> reject MCAR (data are likely MAR or MNAR)
naniar::mcar_test(iris_new_2) 

sum(is.na(iris_new_2)) # 41: same as above
sum(is.na(iris_new_2)) / prod(dim(iris_new_2)) # 0.16: same as above

# ------------------------------------------------------------------------------
