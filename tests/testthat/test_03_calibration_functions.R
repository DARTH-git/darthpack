context("testing 03_calibration_functions.R")

library(darthpack)

l_params_all   <- load_all_params()
v_param_names  <- c("p_S1S2", "hr_S1", "hr_S2")
v_lb <- c(p_S1S2 = 0.01, hr_S1 = 1.0, hr_S2 = 5)
v_ub <- c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15)
v_params_calib <- c(p_S1S2 = 0.105, hr_S1 = 3, hr_S2 = 10)

#### Unit tests start ####
test_that("calibration_out returns one output per target and per target time", {
  l_out <- calibration_out(v_params_calib = v_params_calib,
                           l_params_all   = l_params_all)

  # the output names must match the names of the calibration targets, because
  # log_lik() matches model outputs to targets by name
  expect_identical(names(l_out), names(SickSicker_targets))

  # one model output per target time, labelled with the model cycle
  for (target in names(SickSicker_targets)) {
    expect_equal(length(l_out[[target]]), nrow(SickSicker_targets[[target]]))
    expect_identical(names(l_out[[target]]),
                     as.character(SickSicker_targets[[target]]$Time))
    # all three targets are proportions
    expect_true(all(l_out[[target]] >= 0 & l_out[[target]] <= 1))
  }
})

test_that("calibration_out honours v_target_times and rejects invalid ones", {
  l_out <- calibration_out(v_params_calib = v_params_calib,
                           l_params_all   = l_params_all,
                           v_target_times = c(5, 15))
  expect_identical(names(l_out$Surv), c("5", "15"))

  # survival must be non-increasing over time
  l_out_all <- calibration_out(v_params_calib = v_params_calib,
                               l_params_all   = l_params_all,
                               v_target_times = 0:l_params_all$n_t)
  expect_true(all(diff(l_out_all$Surv) <= 0))
  expect_equal(unname(l_out_all$Surv[1]), 1)

  # a target time beyond the time horizon used to return NA silently
  expect_error(calibration_out(v_params_calib = v_params_calib,
                               l_params_all   = l_params_all,
                               v_target_times = c(10, 20, 30, 999)),
               "Not valid: 999")
  expect_error(calibration_out(v_params_calib = v_params_calib,
                               l_params_all   = l_params_all,
                               v_target_times = 10.5),
               "whole model cycles")
})

test_that("sample_prior draws within the prior bounds", {
  set.seed(1)
  m_samp <- sample_prior(n_samp = 50)

  expect_equal(dim(m_samp), c(50L, length(v_param_names)))
  expect_identical(colnames(m_samp), v_param_names)
  for (i in seq_along(v_param_names)) {
    expect_true(all(m_samp[, i] >= v_lb[i] & m_samp[, i] <= v_ub[i]))
  }

  # sample.prior is kept as an alias for IMIS and old user code
  set.seed(1)
  expect_identical(sample.prior(n_samp = 50), m_samp)
})

test_that("sample_prior and log_prior validate their bounds", {
  expect_error(sample_prior(n_samp = 2, v_lb = c(0.01, 1)),
               "one element per calibrated")
  expect_error(sample_prior(n_samp = 2,
                            v_lb = c(p_S1S2 = 0.60, hr_S1 = 1.0, hr_S2 = 5),
                            v_ub = c(p_S1S2 = 0.50, hr_S1 = 4.5, hr_S2 = 15)),
               "smaller than the corresponding")
  expect_error(log_prior(v_params = c(p_S1S2 = 0.1, hr_S1 = 3)),
               "3 calibrated parameter")
})

test_that("log_prior is flat inside the bounds and -Inf outside", {
  set.seed(2)
  m_samp <- sample_prior(n_samp = 10)
  v_lprior <- log_prior(m_samp)

  expect_equal(length(v_lprior), nrow(m_samp))
  # uniform priors => constant density inside the support
  expect_equal(v_lprior, rep(-sum(log(v_ub - v_lb)), nrow(m_samp)))
  expect_equal(prior(m_samp), exp(v_lprior))

  # outside the support
  m_out <- m_samp
  m_out[3, "hr_S2"] <- 100
  expect_true(is.infinite(log_prior(m_out)[3]))
  expect_equal(prior(m_out)[3], 0)
  expect_true(all(is.finite(log_prior(m_out)[-3])))

  # a single parameter set given as a vector works too
  expect_equal(length(log_prior(v_params_calib)), 1L)
})

test_that("log_lik returns one value per parameter set", {
  set.seed(3)
  m_samp <- sample_prior(n_samp = 4)
  v_llik <- log_lik(m_samp, l_params_all = l_params_all)

  expect_equal(length(v_llik), nrow(m_samp))
  expect_true(all(is.finite(v_llik)))
  expect_equal(likelihood(m_samp, l_params_all = l_params_all), exp(v_llik))
  expect_equal(length(log_lik(v_params_calib, l_params_all = l_params_all)), 1L)
})

test_that("a failing parameter set does not corrupt the other log-likelihoods", {
  set.seed(4)
  m_samp <- sample_prior(n_samp = 5)
  v_llik_ref <- log_lik(m_samp, l_params_all = l_params_all)

  # NA makes the decision model fail for this parameter set only
  m_bad <- m_samp
  m_bad[3, "hr_S1"] <- NA_real_
  v_llik_bad <- log_lik(m_bad, l_params_all = l_params_all)

  expect_equal(length(v_llik_bad), nrow(m_bad))
  expect_true(is.infinite(v_llik_bad[3]) && v_llik_bad[3] < 0)
  # the remaining parameter sets are untouched
  expect_equal(v_llik_bad[-3], v_llik_ref[-3])
  expect_false(any(is.na(v_llik_bad)))
})

test_that("log_lik target weights are validated and applied", {
  set.seed(5)
  m_samp <- sample_prior(n_samp = 3)

  expect_error(log_lik(m_samp, l_params_all = l_params_all,
                       v_weights = c(1, 1)),
               "one weight per calibration target")
  # zero weights on every target give a log-likelihood of zero
  expect_equal(log_lik(m_samp, l_params_all = l_params_all,
                       v_weights = rep(0, 3)),
               rep(0, 3))
  # doubling every weight doubles the overall log-likelihood
  expect_equal(log_lik(m_samp, l_params_all = l_params_all,
                       v_weights = rep(2, 3)),
               2 * log_lik(m_samp, l_params_all = l_params_all))
})

test_that("log_post equals log_prior + log_lik inside the prior support", {
  set.seed(6)
  m_samp <- sample_prior(n_samp = 5)

  expect_equal(log_post(m_samp, l_params_all = l_params_all),
               log_prior(m_samp) + log_lik(m_samp, l_params_all = l_params_all))
  expect_equal(posterior(m_samp, l_params_all = l_params_all),
               exp(log_post(m_samp, l_params_all = l_params_all)))

  # outside the prior support the log-posterior is -Inf and the decision model
  # is never evaluated at the infeasible parameter set
  m_out <- m_samp
  m_out[2, "p_S1S2"] <- 5
  v_lpost <- log_post(m_out, l_params_all = l_params_all)
  expect_equal(length(v_lpost), nrow(m_out))
  expect_true(is.infinite(v_lpost[2]) && v_lpost[2] < 0)
  expect_equal(v_lpost[-2], log_post(m_samp, l_params_all = l_params_all)[-2])
  expect_equal(posterior(m_out, l_params_all = l_params_all)[2], 0)
})

test_that("the posterior favours the calibrated parameters over the prior mean", {
  # the MAP estimate shipped with the package must have a higher posterior
  # density than an arbitrary draw from the prior
  set.seed(7)
  v_prior_draw <- sample_prior(n_samp = 1)[1, ]
  expect_gt(log_post(v_calib_post_map, l_params_all = l_params_all),
            log_post(v_prior_draw, l_params_all = l_params_all))
})
