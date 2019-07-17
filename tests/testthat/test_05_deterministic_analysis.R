context("testing 05a_deterministic_analysis.R")

library(reshape2)
library(dampack)
library(dplyr)
library(darthpack)

#### Generating relevant data and load functions ####
l_params_all <- load_all_params()
v_names_str <- l_params_all$v_names_str
n_str <- length(v_names_str)
l_params_basecase <- update_param_list(l_params_all, v_calib_post_map) 
df_out_ce <- calculate_ce_out(l_params_all = l_params_basecase, 
                              n_wtp = 150000)

test_that("check icer table", {
  # generate testing data
  df_cea_det <- calculate_icers(cost       = df_out_ce$Cost, 
                                effect     = df_out_ce$Effect, 
                                strategies = v_names_str)
  
  # check data formate
  expect_equal(nrow(df_cea_det), 2)
  expect_equal(df_cea_det$Strategy, v_names_str)
  expect_true(all(is.na(df_cea_det[1, c("Inc_Cost", "Inc_Effect", "ICER")])))
})


test_that("check one-way sensitivity output", {
  # generate testing data
  parms <- c("c_Trt", "p_HS1", "u_S1", "u_Trt")
  ranges <- list("c_Trt" = c(6000, 13000),
                 "p_HS1" = c(0.01, 0.50),
                 "u_S1"  = c(0.75, 0.95),
                 "u_Trt" = c(0.75, 0.95))
  nsamps <- 100
  n_wtp <- 150000
  
  owsa_nmb <- owsa_det(parms = parms, # parameter names
                       ranges = ranges,
                       nsamps = nsamps, # number of values  
                       FUN = calculate_ce_out, # Function to compute outputs 
                       params_basecase = l_params_basecase, # List with base-case parameters
                       outcome = "NMB",      # Output to do the OWSA on
                       strategies = v_names_str, # Names of strategies
                       n_wtp = n_wtp       # Extra argument to pass to FUN
  )
  
  # check samples 
  expect_equal(nrow(owsa_nmb), length(parms) * length(v_names_str) * nsamps)
  
  # check whether the output has all the parameters specified
  expect_true(all(unique(as.character(owsa_nmb$parameter)) %in% parms))
  expect_equal(length(unique(as.character(owsa_nmb$parameter))), 4)
  
  # check whether the ranges of the output parameters are the same as the range specified
  param_range <- owsa_nmb %>% 
    select(parameter, param_val) %>% 
    group_by(parameter) %>% 
    summarize(range_min = min(param_val, na.rm = T), 
              range_max = max(param_val, na.rm = T)) %>%
    ungroup() %>% 
    mutate(parameter = as.character(parameter))
  
  range_df <- do.call(rbind, ranges)
  colnames(range_df) <- c("min", "max")
  range_df <- as.data.frame(range_df)
  range_df <- range_df[match(rownames(range_df), param_range$parameter), ]
  
  expect_true(all(range_df$max == param_range$range_max))
  expect_true(all(range_df$min == param_range$range_min))
  
  # check owsa_nmb strategies
  stgy_set <- unique(as.character(owsa_nmb$strategy))
  expect_true(all(stgy_set %in% v_names_str))
  
  # check outcome_val
  expect_true(all(is.numeric(owsa_nmb$outcome_val)))
  expect_false(any(is.na(owsa_nmb$outcome_val)))
})


test_that("check two-way sensitivity output", {
  # generate testing data
  parm1 <- "u_S1"
  parm2 <- "u_Trt"
  ranges <- list("u_S1"  = c(0.70, 0.80),
                 "u_Trt" = c(0.90, 1.00))
  nsamps <- 40
  twsa_nmb <- twsa_det(parm1 = parm1,  # parameter 1 name
                       parm2 = parm2, # parameter 2 name
                       ranges = ranges,
                       nsamps = nsamps, # number of values  
                       FUN = calculate_ce_out, # Function to compute outputs 
                       params_basecase = l_params_basecase, # Vector with base-case parameters
                       outcome = "NMB",      # Output to do the OWSA on
                       strategies = v_names_str, # Names of strategies
                       n_wtp = 150000        # Extra argument to pass to FUN
  )
    
  # check samples
  expect_equal(nrow(twsa_nmb), nsamps * nsamps * length(v_names_str))
  
  # check ranges
  param_range <- data.frame(parms = c(parm1, parm2), 
                            min = c(min(twsa_nmb$u_S1), min(twsa_nmb$u_Trt)), 
                            max = c(max(twsa_nmb$u_S1), max(twsa_nmb$u_Trt)))
  range_df <- do.call(rbind, ranges)
  expect_equal(sum(range_df[, 1] - param_range[, "min"]), 0)
  expect_equal(sum(range_df[, 2] - param_range[, "max"]), 0)
  
  # check strategies
  expect_true(all(unique(as.character(twsa_nmb$strategy)) %in% v_names_str))
  
  # check outcome_val
  expect_true(all(is.numeric(twsa_nmb$outcome_val)))
  expect_false(any(is.na(twsa_nmb$outcome_val)))
})




