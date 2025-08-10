# ------------------------------------------------------------------------------

# Missing Data Matrix: heatmap of missing values across variables
# Upset Plots: show combinations of variables tend to be missing together
# Missing Data Patterns by Groups: examine missingness patterns by categories

airquality <- tibble::as_tibble(datasets::airquality)
oceanbuoys <- tibble::as_tibble(naniar::oceanbuoys)

# visualize missing patterns
naniar::vis_miss(airquality)
# Show patterns of co-occurring missingness
naniar::gg_miss_upset(airquality, nsets = 8)

# visualize missing patterns
naniar::vis_miss(oceanbuoys)
# Show patterns of co-occurring missingness
naniar::gg_miss_upset(oceanbuoys, nsets = 8)
# Examine missingness patterns by categories
oceanbuoys |> 
  dplyr::group_by(year) |> # categoryical_variable
  naniar::miss_var_summary() |> 
  dplyr::arrange(desc(pct_miss))

# ------------------------------------------------------------------------------
