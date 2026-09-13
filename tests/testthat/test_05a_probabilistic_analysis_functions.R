context("testing 05a_probabilistic_analysis_functions.R")

library(darthpack)

v_calib_names <- colnames(m_calib_post)
v_psa_names <- c(v_calib_names,
                 "p_HS1", "p_S1H",
                 "c_H", "c_S1", "c_S2", "c_Trt", "c_D",
                 "u_H", "u_S1", "u_S2", "u_D", "u_Trt")

#### Unit tests start ####
test_that("generate_psa_params honours n_sim", {
  # n_sim used to be overwritten with nrow(m_calib_post), so every request
  # returned 1000 rows whatever the user asked for
  expect_equal(nrow(generate_psa_params(n_sim = 10)), 10)
  expect_equal(nrow(generate_psa_params(n_sim = 1)), 1)
  expect_equal(nrow(generate_psa_params()), nrow(m_calib_post))

  df_psa <- generate_psa_params(n_sim = 10)
  expect_identical(colnames(df_psa), v_psa_names)
  expect_equal(ncol(df_psa), 15)
})

test_that("generate_psa_params is reproducible for a given seed", {
  # the seed argument was assigned to an unused variable, so repeated calls
  # returned different datasets
  expect_equal(generate_psa_params(n_sim = 20, seed = 1),
               generate_psa_params(n_sim = 20, seed = 1))
  expect_false(isTRUE(all.equal(generate_psa_params(n_sim = 20, seed = 1),
                                generate_psa_params(n_sim = 20, seed = 2))))

  # seed = NULL leaves the random number generator to the caller
  set.seed(42)
  df_a <- generate_psa_params(n_sim = 20, seed = NULL)
  set.seed(42)
  df_b <- generate_psa_params(n_sim = 20, seed = NULL)
  expect_equal(df_a, df_b)
})

test_that("generate_psa_params draws the calibrated parameters from the posterior", {
  # every calibrated parameter set must be one of the posterior draws, so that
  # the correlation between the calibrated parameters is preserved
  df_psa <- generate_psa_params(n_sim = 50, seed = 3)
  m_drawn <- as.matrix(df_psa[, v_calib_names])
  v_post_keys <- apply(m_calib_post, 1, paste, collapse = "|")
  v_drawn_keys <- apply(m_drawn, 1, paste, collapse = "|")
  expect_true(all(v_drawn_keys %in% v_post_keys))

  # asking for the full posterior sample uses each draw exactly once
  df_full <- generate_psa_params(n_sim = nrow(m_calib_post), seed = 3)
  expect_equal(as.matrix(df_full[, v_calib_names]),
               matrix(m_calib_post, ncol = length(v_calib_names),
                      dimnames = list(NULL, v_calib_names)))
})

test_that("generate_psa_params warns when asked for more sets than the posterior has", {
  expect_warning(df_psa <- generate_psa_params(n_sim = nrow(m_calib_post) + 5,
                                               seed = 4),
                 "resampled with replacement")
  expect_equal(nrow(df_psa), nrow(m_calib_post) + 5)
})

test_that("generate_psa_params rejects an invalid n_sim", {
  expect_error(generate_psa_params(n_sim = 0), "at least 1")
  expect_error(generate_psa_params(n_sim = -5), "at least 1")
  expect_error(generate_psa_params(n_sim = NA), "at least 1")
  expect_error(generate_psa_params(n_sim = c(10, 20)), "single number")
  expect_error(generate_psa_params(n_sim = "1000"), "single number")
})

test_that("the sampled PSA parameters respect their distributional bounds", {
  df_psa <- generate_psa_params(n_sim = 200, seed = 5)

  # probabilities
  expect_true(all(df_psa$p_HS1 > 0 & df_psa$p_HS1 < 1))
  expect_true(all(df_psa$p_S1H > 0 & df_psa$p_S1H < 1))
  # costs are non-negative, and the dead state is costless
  for (cost in c("c_H", "c_S1", "c_S2", "c_Trt")) {
    expect_true(all(df_psa[[cost]] > 0))
  }
  expect_true(all(df_psa$c_D == 0))
  # utilities are capped at 1 by the truncated normals, and death is worth 0
  for (util in c("u_H", "u_S1", "u_S2", "u_Trt")) {
    expect_true(all(df_psa[[util]] <= 1))
  }
  expect_true(all(df_psa$u_D == 0))
})

test_that("every PSA parameter set can be run through the decision model", {
  l_params_all <- load_all_params()
  df_psa <- generate_psa_params(n_sim = 25, seed = 6)

  for (i in 1:nrow(df_psa)) {
    l_psa_input <- update_param_list(l_params_all, df_psa[i, ])
    df_out <- calculate_ce_out(l_psa_input, n_wtp = 150000)
    expect_true(all(is.finite(df_out$Cost)))
    expect_true(all(is.finite(df_out$Effect)))
    expect_true(all(df_out$Cost >= 0))
    expect_true(all(df_out$Effect >= 0))
  }
})
