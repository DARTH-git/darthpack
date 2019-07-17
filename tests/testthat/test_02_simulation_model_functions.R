context("testing 02_simulation_model_functions.R")

library(dplyr)    # For data manipulation
library(darthpack)

# load inputs
l_params_all <- load_all_params()

#### Unit tests start ####
test_that("invalid number of cycles", {
  l_params_all$n_t <- 90
  
  expect_error(decision_model(l_params_all, verbose = F), 
               "Not all the age in the age range have a corresponding mortality rate")
})

test_that("invalid initial states", {
  l_params_all$v_s_init <- c(H = -1, S1 = 0, S2 = 0, D = 0)
  
  expect_error(decision_model(l_params_all, verbose = F), 
               "vector of initial states \\(v_s_init\\) is not valid")
})

test_that("reproducing error message invalid transition probabiliies", {
  # generate testing data
  for (i in 1:length(l_params_all)) {
    assign(names(l_params_all)[[i]], l_params_all[[i]])
  }
  
  # Mortality for healthy individuals
  p_HDage  <- 1 - exp(-v_r_mort_by_age[(n_age_init + 1) + 0:(n_t - 1)])        
  # Mortality for sick individuals
  p_S1Dage <- 1 - exp(-v_r_mort_by_age[(n_age_init + 1) + 0:(n_t - 1)] * hr_S1)
  # Mortality for sicker individuals
  p_S2Dage <- 1 - exp(-v_r_mort_by_age[(n_age_init + 1) + 0:(n_t - 1)] * hr_S2)
  
  a_P <- array(0, dim = c(n_states, n_states, n_t),
               dimnames = list(v_n, v_n, 0:(n_t-1)))
  # Fill in array
  # From H
  a_P["H", "H", ]  <- (1-p_HDage) * (1 - p_HS1)
  a_P["H", "S1", ] <- (1-p_HDage) * p_HS1
  a_P["H", "D", ]  <- p_HDage
  # From S1
  a_P["S1", "H", ]  <- (1-p_S1Dage) * p_S1H
  a_P["S1", "S1", ] <- (1-p_S1Dage) * (1 - (p_S1S2 + p_S1H))
  a_P["S1", "S2", ] <- (1-p_S1Dage) * p_S1S2
  a_P["S1", "D", ]  <- p_S1Dage
  # From S2
  a_P["S2", "S2", ] <- 1 - p_S2Dage
  a_P["S2", "D", ]  <- p_S2Dage
  # From D
  a_P["D", "D", ] <- 1
  
  # check if the correct input might produce unintended message
  expect_silent(check_transition_probability(a_P, err_stop = F, verbose = F))
  expect_silent(check_transition_probability(a_P, err_stop = F, verbose = T))
  expect_silent(check_transition_probability(a_P, err_stop = T, verbose = F))
  expect_silent(check_transition_probability(a_P, err_stop = T, verbose = T))
  
  # check error messages of "check_transition_probability"
  a_P2 <- a_P
  a_P2["S2", "S2", ] <- -0.03
  expect_warning(check_transition_probability(a_P2, err_stop = F, verbose = T))
  expect_error(check_transition_probability(a_P2, err_stop = T, verbose = F))
  expect_error(check_transition_probability(a_P2, err_stop = T, verbose = T))
  
  # check error messages of "check_sum_of_transition_array"
  a_P2 <- a_P
  a_P2["S2", "S2", ] <- -0.03
  expect_warning(check_sum_of_transition_array(a_P2, n_states, n_t, err_stop = F, verbose = T))  
  expect_error(check_sum_of_transition_array(a_P2, n_states, n_t, err_stop = T, verbose = T)) 
  
  # check the decision_model function
  l_params_all2 <- l_params_all
  l_params_all2$p_S1S2 <- -0.105
  expect_silent(decision_model(l_params_all2, err_stop = F, verbose = F))
  expect_error(decision_model(l_params_all2, err_stop = T, verbose = F))
})

test_that("correct outputs", {
  output <- decision_model(l_params_all, err_stop = F, verbose = F)
  
  # checking overall outputs
  expect_equal(length(output), 2)
  expect_true(all(unlist(lapply(output, is.array))))
  expect_identical(names(output), c("a_P", "m_M"))
  
  # checking output 1
  expect_equal(dim(output[[1]]), 
               c(l_params_all$n_states, l_params_all$n_states, l_params_all$n_t))
  expect_true(all(output[[1]] >= 0) | all(output[[1]] <= 1))
  expect_true(all(round(apply(output[[1]], c(1, 3), function(x) sum(x)) * 100) / 100 == 1))
  
  # checking output 2
  expect_equal(dim(output[[2]]), 
               c(l_params_all$n_t + 1, l_params_all$n_states))
  expect_true(all(round(rowSums(output[[2]]) * 100) / 100 == 1))
  expect_true(all(output[[2]] >= 0))
  
  output <- decision_model(l_params_all, err_stop = T, verbose = F)
})
