context("testing 05a_deterministic_analysis_functions.R")

library(dplyr)    # For data manipulation
library(darthpack)

l_params_all <- load_all_params()

test_that("checking output", {
  ce_table <- calculate_ce_out(l_params_all)
  
  expect_true(all(colnames(ce_table) == c("Strategy", "Cost", "Effect", "NMB")))
  expect_true(is.factor(ce_table$Strategy))
  expect_true(all(is.numeric(ce_table$Cost), is.numeric(ce_table$Effect), is.numeric(ce_table$NMB)))
  expect_true(all(ce_table$Strategy == l_params_all$v_names_str))
})


