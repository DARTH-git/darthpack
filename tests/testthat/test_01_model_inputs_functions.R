context("testing 01_model_inputs_functions.R")

library(darthpack)

# The data-raw directory is listed in .Rbuildignore, so the .csv files are
# present when the repository is used as a coding template but not when the
# tests run from an installed package under R CMD check
path_init <- "../../data-raw/01_init_params.csv"
path_mort <- "../../data-raw/01_all_cause_mortality.csv"
has_csv <- file.exists(path_init) && file.exists(path_mort)

#### Unit tests start ####
test_that("load_mort_data returns the mortality rates of the total population", {
  v_r_mort_by_age <- load_mort_data()

  expect_equal(nrow(v_r_mort_by_age), nrow(all_cause_mortality))
  expect_equal(ncol(v_r_mort_by_age), 1)
  expect_equal(as.vector(v_r_mort_by_age), all_cause_mortality$Total)
  expect_true(all(v_r_mort_by_age >= 0))
  # mortality is read from a .csv file identically
  skip_if_not(has_csv, "data-raw/*.csv not available")
  expect_equal(load_mort_data(path_mort), v_r_mort_by_age)
})

test_that("load_mort_data reports a missing file or a missing Total column", {
  expect_error(load_mort_data("no/such/file.csv"), "does not exist")

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(data.frame(Age = 0:2, Rate = c(0.1, 0.2, 0.3)), tmp,
                   row.names = FALSE)
  expect_error(load_mort_data(tmp), "must have a 'Total' column")
})

test_that("load_all_params returns every parameter the model needs", {
  l_params_all <- load_all_params()

  # the result must be visible, so that calling it at the console prints it
  expect_true(withVisible(load_all_params())$visible)

  expect_type(l_params_all, "list")
  expect_true(all(c("v_names_str", "n_str", "n_age_init", "n_t", "v_age_names",
                    "v_n", "n_states", "v_s_init", "v_r_mort_by_age",
                    names(df_params_init)) %in% names(l_params_all)))
  expect_equal(l_params_all$n_states, 4)
  expect_equal(l_params_all$n_str, 2)
  expect_identical(l_params_all$v_n, c("H", "S1", "S2", "D"))
  expect_equal(sum(l_params_all$v_s_init), 1)

  # one age label per row of the cohort trace, which spans cycles 0 to n_t
  expect_equal(length(l_params_all$v_age_names), l_params_all$n_t + 1)
  expect_equal(length(l_params_all$v_age_names),
               nrow(decision_model(l_params_all)$m_M))

  # reading the shipped .csv files gives the same parameters as the package data
  skip_if_not(has_csv, "data-raw/*.csv not available")
  expect_equal(load_all_params(file.init = path_init, file.mort = path_mort),
               l_params_all)
})

test_that("load_all_params reports missing files and incomplete parameters", {
  expect_error(load_all_params(file.init = "no/such/file.csv"),
               "does not exist")

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  # an incomplete parameter set used to fail much later with the obscure
  # "object 'n_age_init' not found"
  utils::write.csv(data.frame(c_H = 2000, u_H = 1), tmp, row.names = FALSE)
  expect_error(load_all_params(file.init = tmp), "is missing: ")
  expect_error(load_all_params(file.init = tmp), "n_age_init")

  # more than one row of parameters
  df_two <- rbind(df_params_init, df_params_init)
  utils::write.csv(df_two, tmp, row.names = FALSE)
  expect_error(load_all_params(file.init = tmp), "exactly one row")
})

test_that("update_param_list updates the named parameters", {
  l_params_all <- load_all_params()

  # a named vector
  l_upd <- update_param_list(l_params_all, c(p_S1S2 = 0.2, hr_S1 = 4))
  expect_equal(l_upd$p_S1S2, 0.2)
  expect_equal(l_upd$hr_S1, 4)
  # everything else is untouched
  expect_equal(l_upd[setdiff(names(l_upd), c("p_S1S2", "hr_S1"))],
               l_params_all[setdiff(names(l_params_all), c("p_S1S2", "hr_S1"))])

  # a list
  expect_equal(update_param_list(l_params_all, list(p_S1S2 = 0.2))$p_S1S2, 0.2)
  # a single-row data frame, as used by the PSA loop
  df_one <- generate_psa_params(n_sim = 1, seed = 1)
  l_psa_input <- update_param_list(l_params_all, df_one)
  expect_equal(l_psa_input$c_Trt, df_one$c_Trt)
})

test_that("update_param_list rejects names that are not model parameters", {
  l_params_all <- load_all_params()

  # a misspelled name used to be appended silently while the parameter the user
  # meant to change kept its old value
  expect_error(update_param_list(l_params_all, c(p_S1S2_typo = 0.2)),
               "not in 'l_params_all'")
  expect_error(update_param_list(l_params_all, c(p_S1S2_typo = 0.2)),
               "p_S1S2_typo")
  expect_error(update_param_list(l_params_all, c(p_S1S2 = 0.2, hr_1 = 4)),
               "hr_1")
  expect_error(update_param_list(l_params_all, c(0.2)),
               "must be named")

  # allow_new = TRUE restores the previous behaviour
  l_upd <- update_param_list(l_params_all, c(p_new = 0.2), allow_new = TRUE)
  expect_equal(l_upd$p_new, 0.2)
  expect_equal(l_upd$p_S1S2, l_params_all$p_S1S2)
})
