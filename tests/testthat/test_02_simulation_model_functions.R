context("testing 02_simulation_model_functions.R")

library(dplyr)    # For data manipulation
library(darthpack)

# load inputs
l_params_all <- load_all_params()

#### Unit tests start ####
test_that("invalid inputs", {

  # We use an inaccurate input to raise a specific error message 
  # "Not all the age in the age range have a corresponding mortality rate" 
  l_params_all$n_t <- 90
  expect_error(decision_model(l_params_all), 
               "Not all the age in the age range have a corresponding mortality rate")
  
  
  # We use an inaccurate input to raise a specific error message 
  # "vector of initial states (v_s_init) is not valid" 
  l_params_all$n_t <- 75
  l_params_all$v_s_init <- c(H = -1, S1 = 0, S2 = 0, D = 0)
  expect_error(decision_model(l_params_all), 
               "vector of initial states \\(v_s_init\\) is not valid")
})

test_that("reproducing error message invalid transition probabiliies", {
  ## generate testing data
  for (i in 1:length(l_params_all)) {
    assign(names(l_params_all)[[i]], l_params_all[[i]])
  }
  
  # mortality for healthy individuals
  p_HDage  <- 1 - exp(-v_r_mort_by_age[(n_age_init + 1) + 0:(n_t - 1)])        
  p_S1Dage <- 1 - exp(-v_r_mort_by_age[(n_age_init + 1) + 0:(n_t - 1)] * hr_S1)
  p_S2Dage <- 1 - exp(-v_r_mort_by_age[(n_age_init + 1) + 0:(n_t - 1)] * hr_S2)
  
  # creating transition probability array and filling out the values
  a_P <- array(0, dim = c(n_states, n_states, n_t),
               dimnames = list(v_n, v_n, 0:(n_t-1)))
  a_P["H", "H", ]  <- (1-p_HDage) * (1 - p_HS1)
  a_P["H", "S1", ] <- (1-p_HDage) * p_HS1
  a_P["H", "D", ]  <- p_HDage
  a_P["S1", "H", ]  <- (1-p_S1Dage) * p_S1H
  a_P["S1", "S1", ] <- (1-p_S1Dage) * (1 - (p_S1S2 + p_S1H))
  a_P["S1", "S2", ] <- (1-p_S1Dage) * p_S1S2
  a_P["S1", "D", ]  <- p_S1Dage
  a_P["S2", "S2", ] <- 1 - p_S2Dage
  a_P["S2", "D", ]  <- p_S2Dage
  a_P["D", "D", ] <- 1
  
  ## check if the correct input might produce unintended message
  expect_silent(check_transition_probability(a_P, err_stop = F, verbose = F))
  expect_silent(check_transition_probability(a_P, err_stop = F, verbose = T))
  expect_silent(check_transition_probability(a_P, err_stop = T, verbose = F))
  expect_silent(check_transition_probability(a_P, err_stop = T, verbose = T))
  
  ## check error messages of "check_transition_probability"
  a_P2 <- a_P
  # use an invalid value that would cause warning or error 
  a_P2["S2", "S2", ] <- -0.03
  
  # we expect there is an warning message
  expect_warning(check_transition_probability(a_P2, err_stop = F, verbose = T))
  # we expect there is an error message
  expect_error(check_transition_probability(a_P2, err_stop = T, verbose = F))
  # we expect there is an error message instead of a warning message
  expect_error(check_transition_probability(a_P2, err_stop = T, verbose = T))
  
  ## check error messages of "check_sum_of_transition_array"
  # we expect there is an warning message
  expect_warning(check_sum_of_transition_array(a_P2, n_states, n_t, err_stop = F, verbose = T))  
  # we expect there is an error message
  expect_error(check_sum_of_transition_array(a_P2, n_states, n_t, err_stop = T, verbose = F))
  # we expect there is an error message instead of a warning message
  expect_error(check_sum_of_transition_array(a_P2, n_states, n_t, err_stop = T, verbose = T)) 
  
  ## testing whether the "check_" functions work properly in the decision_model function
  l_params_all2 <- l_params_all
  l_params_all2$p_S1S2 <- -0.105
  
  expect_silent(decision_model(l_params_all2, err_stop = F, verbose = F))
  expect_error(decision_model(l_params_all2, err_stop = T, verbose = F))
})

test_that("correct outputs", {
  ## generate output data from decision_model
  output <- decision_model(l_params_all, err_stop = F, verbose = F)
  
  ## checking overall outputs
  # check the number of elements in the output
  expect_equal(length(output), 2)
  # check whether both the outputs are array type
  expect_true(all(unlist(lapply(output, is.array))))
  # check whether the output names are identical as expected 
  expect_identical(names(output), c("a_P", "m_M"))
  
  ## checking output 1: a_P (the transition probability array)
  # check whether the dimension of a_P is as expected
  expect_equal(dim(output[[1]]), 
               c(l_params_all$n_states, l_params_all$n_states, l_params_all$n_t))
  # check whether all the transition probability is between 0 and 1
  expect_true(all(output[[1]] >= 0) | all(output[[1]] <= 1))
  # check whether all rows of each transition matrix sum up to 1 (sum_to_1)
  # because the sums are not numerically equal to 1, we made some adjustment (correct_small_digits)
  sum_to_1 <- apply(output[[1]], c(1, 3), function(x) sum(x)) 
  correct_small_digits <- round(sum_to_1 * 100) / 100
  expect_true(all(correct_small_digits == 1))
  
  ## checking output 2: m_M (trace matrix)
  # check whether the dimension of m_M is as expected
  expect_equal(dim(output[[2]]), 
               c(l_params_all$n_t + 1, l_params_all$n_states))
  # check whether each row in m_M sums up to 1 (sum_to_1)
  # because the sumes are not numerically equal to 1, we made some adjustment (correct_small_digits)
  sum_to_1 <- rowSums(output[[2]])
  correct_small_digits <- round(sum_to_1 * 100) / 100
  expect_true(all(correct_small_digits == 1))
  expect_true(all(output[[2]] >= 0 & output[[2]] <= 1))
})
