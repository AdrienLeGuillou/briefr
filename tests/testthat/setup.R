# Setup the estimation object before any tests
n <- 100
excel_epoch <- lubridate::ymd("1900-01-01")
d_test <- data.frame(
  numeric_var = runif(n),
  quali_var = sample(c("a", "b", "c"), n, TRUE),
  quali2_var = sample(c("a", "b", "c"), n, TRUE),
  date_var = excel_epoch + as.integer(sample(n, n, replace = TRUE))
)

# debugonce(dflt_kable)
# brf_formatted_cat_lvl(d_test, quali_var, quali2_var)

# Code to run after all the tests
withr::defer(
  {
    rm(list = ls()) # placeholder, not useful
  },
  teardown_env()
)
