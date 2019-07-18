context("testing 05a_deterministic_analysis_functions.R")

library(dplyr)    # For data manipulation
library(darthpack)

l_params_all <- load_all_params()

#### Unit tests start ####
test_that("checking output", {
  ## generate the output from calculate_ce_out
  ce_table <- calculate_ce_out(l_params_all)
  
  # check the expected column names
  expect_identical(colnames(ce_table), c("Strategy", "Cost", "Effect", "NMB"))
  # check whether the strategies are either factor or character
  expect_true(is.factor(ce_table$Strategy) | is.character(ce_table$Strategy))
  # check whether Cost, Effect, and NMB columns are numeric
  expect_true(all(is.numeric(ce_table$Cost), is.numeric(ce_table$Effect), is.numeric(ce_table$NMB)))
  # check whether the strategies are identical to what we expect
  expect_identical(as.character(ce_table$Strategy), l_params_all$v_names_str)
})


