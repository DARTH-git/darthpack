context("testing 05b_deterministic_analysis_functions.R")

library(darthpack)

l_params_all <- load_all_params()

#### Unit tests start ####
test_that("calculate_ce_out discounts costs with d_c and effects with d_e", {
  # Undiscounted totals, used as the reference
  l_nodisc <- update_param_list(l_params_all, c(d_c = 0, d_e = 0))
  df_nodisc <- calculate_ce_out(l_nodisc)

  # Discounting costs only must lower costs and leave effects untouched
  l_disc_c <- update_param_list(l_params_all, c(d_c = 0.03, d_e = 0))
  df_disc_c <- calculate_ce_out(l_disc_c)
  expect_true(all(df_disc_c$Cost < df_nodisc$Cost))
  expect_equal(df_disc_c$Effect, df_nodisc$Effect)

  # Discounting effects only must lower effects and leave costs untouched
  l_disc_e <- update_param_list(l_params_all, c(d_c = 0, d_e = 0.03))
  df_disc_e <- calculate_ce_out(l_disc_e)
  expect_true(all(df_disc_e$Effect < df_nodisc$Effect))
  expect_equal(df_disc_e$Cost, df_nodisc$Cost)
})

test_that("calculate_ce_out returns the expected shape and consistent NMB", {
  df_ce <- calculate_ce_out(l_params_all, n_wtp = 150000)

  expect_identical(colnames(df_ce), c("Strategy", "Cost", "Effect", "NMB"))
  expect_equal(nrow(df_ce), length(l_params_all$v_names_str))
  # the strategy column must be usable as a label by dampack
  expect_true(is.factor(df_ce$Strategy) | is.character(df_ce$Strategy))
  expect_identical(as.character(df_ce$Strategy), l_params_all$v_names_str)
  expect_true(all(is.numeric(df_ce$Cost), is.numeric(df_ce$Effect),
                  is.numeric(df_ce$NMB)))
  expect_equal(df_ce$NMB, df_ce$Effect * 150000 - df_ce$Cost)
  # treatment adds cost and utility in the Sick-Sicker model
  expect_true(df_ce$Cost[2]   > df_ce$Cost[1])
  expect_true(df_ce$Effect[2] > df_ce$Effect[1])
})

test_that("owsa_det varies each parameter over its own range", {
  df_owsa <- owsa_det(parms  = c("c_Trt", "u_S1"),
                      ranges = list("c_Trt" = c(6000, 13000),
                                    "u_S1"  = c(0.75, 0.95)),
                      nsamps = 5,
                      params_basecase = l_params_all,
                      FUN     = calculate_ce_out,
                      outcome = "NMB",
                      n_wtp   = 150000)

  expect_s3_class(df_owsa, "owsa")
  expect_identical(colnames(df_owsa),
                   c("parameter", "param_val", "strategy", "outcome_val"))
  expect_equal(nrow(df_owsa), 2 * 5 * l_params_all$n_str)
  expect_equal(range(df_owsa$param_val[df_owsa$parameter == "c_Trt"]),
               c(6000, 13000))
  expect_equal(range(df_owsa$param_val[df_owsa$parameter == "u_S1"]),
               c(0.75, 0.95))
  expect_true(all(is.finite(df_owsa$outcome_val)))

  # a ranges list given in another order must give the same answer, not swap
  # the ranges between the two parameters
  df_owsa_rev <- owsa_det(parms  = c("c_Trt", "u_S1"),
                          ranges = list("u_S1"  = c(0.75, 0.95),
                                        "c_Trt" = c(6000, 13000)),
                          nsamps = 5,
                          params_basecase = l_params_all,
                          FUN     = calculate_ce_out,
                          outcome = "NMB",
                          n_wtp   = 150000)
  expect_equal(df_owsa_rev, df_owsa)
})

test_that("owsa_det reproduces the base case at the base-case value", {
  # varying a parameter over a degenerate range must return the base-case NMB
  df_ce <- calculate_ce_out(l_params_all, n_wtp = 150000)
  df_owsa <- owsa_det(parms  = "c_Trt",
                      ranges = list("c_Trt" = c(l_params_all$c_Trt,
                                                l_params_all$c_Trt)),
                      nsamps = 2,
                      params_basecase = l_params_all,
                      FUN     = calculate_ce_out,
                      outcome = "NMB",
                      n_wtp   = 150000)
  expect_equal(unique(df_owsa$outcome_val), df_ce$NMB)
})

test_that("owsa_det rejects invalid inputs with informative errors", {
  # replace(), not modifyList(), because the latter would merge the ranges
  # lists instead of replacing them
  call_owsa <- function(...) {
    args <- list(parms = "c_Trt", ranges = list("c_Trt" = c(6000, 13000)),
                 nsamps = 3, params_basecase = l_params_all,
                 FUN = calculate_ce_out, outcome = "NMB", n_wtp = 150000)
    do.call(owsa_det, replace(args, names(list(...)), list(...)))
  }

  # a misspelled parameter name
  expect_error(call_owsa(parms = "c_Trtt", ranges = list("c_Trtt" = c(1, 2))),
               "not in 'params_basecase'")
  # a range given for the wrong parameter
  expect_error(call_owsa(ranges = list("u_S1" = c(0, 1))),
               "No range was given for: c_Trt")
  # an unnamed ranges list
  expect_error(call_owsa(ranges = list(c(6000, 13000))),
               "must be a named list")
  # a range that is not a pair of numbers
  expect_error(call_owsa(ranges = list("c_Trt" = c(6000, 9000, 13000))),
               "numeric vector of length two")
  expect_error(call_owsa(ranges = list("c_Trt" = c(13000, 6000))),
               "numeric vector of length two")
  # a mismatched number of parameters and ranges
  expect_error(call_owsa(parms = c("c_Trt", "u_S1")),
               "is not the same as the number of ranges")
  # ranges that is not a list
  expect_error(call_owsa(ranges = c(6000, 13000)),
               "should be a list")
  # an outcome FUN does not produce
  expect_error(call_owsa(outcome = "ICER"), "is not part of FUN outcomes")
  # nsamps that cannot produce a range of values
  expect_error(call_owsa(nsamps = 1), "at least 2")
  expect_error(call_owsa(nsamps = "100"), "must be a single number")
  # a FUN that fails at the base case: the error used to escape this check
  expect_error(call_owsa(FUN = function(x, ...) stop("boom")),
               "FUN is not well defined")
  # a FUN that does not return a data frame of strategies and outcomes
  expect_error(call_owsa(FUN = function(x, ...) 1:3),
               "must return a data frame")
})

test_that("twsa_det evaluates the full grid of both parameters", {
  df_twsa <- twsa_det(parm1  = "u_S1",
                      parm2  = "u_Trt",
                      ranges = list("u_S1"  = c(0.70, 0.80),
                                    "u_Trt" = c(0.90, 1.00)),
                      nsamps = 4,
                      params_basecase = l_params_all,
                      FUN     = calculate_ce_out,
                      outcome = "NMB",
                      progress = FALSE,
                      n_wtp   = 150000)

  expect_s3_class(df_twsa, "twsa")
  expect_identical(colnames(df_twsa),
                   c("u_S1", "u_Trt", "strategy", "outcome_val"))
  expect_equal(nrow(df_twsa), 4 * 4 * l_params_all$n_str)
  expect_equal(range(df_twsa$u_S1),  c(0.70, 0.80))
  expect_equal(range(df_twsa$u_Trt), c(0.90, 1.00))
  expect_true(all(is.finite(df_twsa$outcome_val)))

  # order of the ranges list must not matter
  df_twsa_rev <- twsa_det(parm1  = "u_S1",
                          parm2  = "u_Trt",
                          ranges = list("u_Trt" = c(0.90, 1.00),
                                        "u_S1"  = c(0.70, 0.80)),
                          nsamps = 4,
                          params_basecase = l_params_all,
                          FUN     = calculate_ce_out,
                          outcome = "NMB",
                          progress = FALSE,
                          n_wtp   = 150000)
  expect_equal(df_twsa_rev, df_twsa)
})

test_that("twsa_det rejects invalid inputs with informative errors", {
  call_twsa <- function(...) {
    args <- list(parm1 = "u_S1", parm2 = "u_Trt",
                 ranges = list("u_S1" = c(0.70, 0.80),
                               "u_Trt" = c(0.90, 1.00)),
                 nsamps = 3, params_basecase = l_params_all,
                 FUN = calculate_ce_out, outcome = "NMB",
                 progress = FALSE, n_wtp = 150000)
    do.call(twsa_det, replace(args, names(list(...)), list(...)))
  }

  expect_error(call_twsa(parm2 = "u_Trtt"), "not in 'params_basecase'")
  expect_error(call_twsa(ranges = list("u_S1" = c(0.70, 0.80))),
               "is not the same as the number of ranges")
  expect_error(call_twsa(ranges = list("u_S1"  = c(0.70, 0.80),
                                       "u_S2"  = c(0.40, 0.60))),
               "No range was given for: u_Trt")
  expect_error(call_twsa(FUN = function(x, ...) stop("boom")),
               "FUN is not well defined")
  expect_error(call_twsa(outcome = "ICER"), "is not part of FUN outcomes")
})
