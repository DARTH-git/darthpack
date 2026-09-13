# darthpack 0.2.0

This is a maintenance release. It fixes a number of defects in the framework
functions, most of which produced wrong results silently rather than failing,
and adds input validation and unit tests so that the same mistakes are caught
in future.

**The calibrated parameters are unchanged.** Re-running the IMIS calibration
reproduces the published posterior bit-for-bit: `m_calib_post` and
`v_calib_post_map` are identical to version 0.1.0, and
`tables/03_summary_posterior.csv` agrees to 14 significant digits. The framework
functions also return the same values as 0.1.0 at the parameters shipped with the
package (log-posterior at the MAP = 19.00374). The cost-effectiveness results
**do** change; see *Regenerated results* below.

## Bug fixes

### Calibration (`R/03_calibration_functions.R`)

* `log_lik()` assigned `-Inf` to the whole `v_llik_overall` vector instead of to
  element `j` when the decision model could not be evaluated at a parameter set.
  A single failing parameter set collapsed the vector to length 1 and then
  re-grew it, so the log-likelihoods already computed for **valid** parameter
  sets were replaced by `-Inf` and `NA`. Any Bayesian calibration that
  encountered one infeasible draw returned a corrupted likelihood surface with
  no warning. A failing parameter set now gets `-Inf` on its own and the others
  are untouched.

* `log_lik()` read the third calibration target as `SickSicker_targets$PropSick`
  and `l_model_res$PropSick`, which only resolved because `$` partial-matches
  `PropSicker` on lists. Any additional target whose name began with `PropSick`
  would have silently changed which target was used. All target names are now
  exact.

* `calibration_out()` took model outputs from the hard-coded cohort-trace rows
  `c(11, 21, 31)`. With a time horizon shorter than 30 cycles it returned `NA`
  outputs and the calibration proceeded on them. Target times are now the
  `v_target_times` argument (default `c(10, 20, 30)`), matched against the named
  rows of the trace and validated against `n_t`.

* `prior()`, `likelihood()`, `log_post()` and `posterior()` accepted only
  `v_params`, so prior bounds, target weights and `l_params_all` passed to them
  were silently dropped and the defaults were used instead. They now take and
  forward the same arguments as `log_prior()` and `log_lik()`.

* `analysis/03_calibration.R` computed the MAP estimate as
  `which.max(exp(log_post(...)))`. `exp()` underflows to zero for every draw once
  the log-posterior is strongly negative, in which case `which.max()` silently
  returns the first draw. The MAP is now taken on the log scale.

### Cost-effectiveness analysis (`R/05b_deterministic_analysis_functions.R`)

* `calculate_ce_out()` discounted costs at `d_e` and QALYs at `d_c` — the two
  rates were crossed over. The repository ships `d_c = d_e = 0.03`, so the
  base case and the published results are unaffected, but any user who set
  different rates for costs and effects got each stream discounted at the other's
  rate.

* `owsa_det()` and `twsa_det()` validated `FUN` with a test
  (`is.na(sum(is.na(jj)))`) that is `FALSE` both when `FUN` works and when it
  fails, so the guard could never fire. A broken `FUN` failed later with an
  unrelated message. The check now works and reports the underlying error.

* `owsa_det()` and `twsa_det()` consumed `ranges` positionally while checking its
  names with `sum(parms == names(ranges))`, which errors on a length mismatch and
  gives a misleading message when the list is unnamed. `ranges` is now matched to
  the parameter names, so a list written in a different order than `parms` no
  longer varies a parameter over another parameter's range.

* `twsa_det()`'s progress report compared floating point numbers with `==`.

### Probabilistic analysis (`R/05a_probabilistic_analysis_functions.R`)

* `generate_psa_params()` overwrote its own `n_sim` argument with
  `nrow(m_calib_post)` on the first line of the body, so it always returned 1000
  parameter sets. `analysis/05a_probabilistic_analysis.R` sized its output
  matrices from its own `n_sim`, so raising `n_sim` above 1000 there left the PSA
  loop reading past the end of the input data frame and feeding rows of `NA`
  parameters to the model.

* `generate_psa_params()` assigned its `seed` argument to an unused local
  (`set_seed <- seed`) instead of calling `set.seed()`, so the PSA dataset was
  not reproducible and two identical calls returned different data.

* `analysis/05a_probabilistic_analysis.R` called `data("l_psa")` immediately
  after building the PSA object, silently replacing the object just simulated
  with the archived one, so every figure and table below came from the packaged
  data rather than from the run. It is now commented out.

### Decision model checks (`R/02_decision_model_functions.R`)

* `check_sum_of_transition_array()` did not check what it documented. It summed
  **all** the row sums of a cycle and compared that total with the number of
  states, so row sums that were wrong in opposite directions cancelled out: an
  array whose rows summed to 1.25 and 0.75 passed. It also used exact `==`
  equality on floating point numbers, so a valid array could be rejected for
  rounding alone. Every row of every cycle is now checked against one within a
  `tolerance`, and the failing rows, cycles and sums are reported.

* `check_transition_probability()` selected invalid entries with
  `which(a_P < 0 | a_P > 1)`. Comparing `NA` or `NaN` with `<` or `>` gives `NA`,
  which `which()` drops, so an array with undefined transition probabilities
  passed and the model went on to produce a cohort trace full of `NA`.

### Model inputs (`R/01_model_inputs_functions.R`)

* `update_param_list()` appended misspelled parameter names to `l_params_all`
  and left the parameter the user meant to change at its old value. The model
  then ran to completion and returned base-case results with no warning. Unknown
  names now raise an error listing them; `allow_new = TRUE` restores the previous
  behaviour.

* `load_all_params()` built its parameter list with the self-assignment
  `df_params_init <- df_params_init` and then appended `df_params_init` at the
  end, which was always the **package** data. A user's `file.init` was therefore
  partly ignored.

* `load_all_params()` ended on an assignment, so it returned invisibly and
  printed nothing at the console.

* `v_age_names` was one label shorter than the `n_t + 1` rows of the cohort trace
  it labels.

* `dplyr::select(df, .data$Total)` is deprecated in tidyselect >= 1.2.0 and
  warned on every call to `load_all_params()`.

### Package infrastructure

* `R CMD build` failed: `knitr::opts_knit$set(root.dir = '..')` inside the figure
  chunks of two vignettes pointed the *following* chunk at the package's parent
  directory, where `../figs/*.png` does not resolve. No source tarball with
  vignettes could be produced, so `R CMD check` could not run to completion.
  `R CMD check` now reports `Status: OK`.

* `open_guide()` was broken for anyone using darthpack as an installed package.
  The guide lives in the top-level `report` directory, which is not installed, so
  `system.file()` returned `""` and the function opened the root of the
  filesystem. It now falls back to the online guide.

* `SickSicker_targets.Rd` and `df_posterior_summ.Rd` were malformed and lost
  their `\format` and `\description` sections: under `markdown = TRUE`, the `\%`
  in the roxygen block became `\\%` in the Rd file, where the `%` starts a
  comment that swallowed the rest of the line.

## New features

* `sample_prior()` follows the DARTH snake_case naming convention.
  `sample.prior()` is retained as an exported alias, because `IMIS::IMIS()`
  resolves the sampling function by that exact name.

* `log_lik()`, `log_post()` and `posterior()` gain a `v_weights` argument for
  the per-target weights that the comments previously told users to set by
  editing the function body.

* `log_post()` no longer runs the decision model for parameter sets outside the
  support of the prior, where the log-posterior is `-Inf` whatever the likelihood
  is. This avoids evaluating the model at infeasible parameter values and speeds
  up calibration.

* `calibration_out()` gains `v_target_times`; `check_sum_of_transition_array()`
  gains `tolerance` and defaults `n_states` and `n_t` to the dimensions of the
  array; `twsa_det()` gains `progress`; `update_param_list()` gains `allow_new`;
  `generate_psa_params()` accepts `seed = NULL`; `open_guide()` gains `online`.

* The two `check_` functions return their report invisibly, so it can be
  inspected without parsing warning text.

## Tests

The test suite grows from 27 to 294 assertions and now covers components 01, 02,
03, 05a and 05b. Each of the defects above has a test that fails against version
0.1.0. One pre-existing assertion read `all(a_P >= 0) | all(a_P <= 1)`, which
passed whenever either bound held on its own, and is now `&`.
`tests/testthat/test_05a_deterministic_analysis_function.R` was renamed to
`test_05b_deterministic_analysis_functions.R`: it tested a component 05b function
under an 05a name.

## Dependencies

* `plotrix` moved from Imports to Suggests; no package function uses it.
* Added the packages the framework loads but never declared: `bookdown`,
  `matrixStats`, `psych`, `scatterplot3d`, `shiny`, `rstudioapi` and `usethis`.
  Users who followed the README and installed everything in DESCRIPTION were
  missing them.
* Removed `BiocManager` and `rlang`, which nothing uses.
* `IMIS` has been archived on CRAN; `analysis/03_calibration.R` now documents
  installing it from the archive with `devtools::install_version()`.

## Regenerated results

All the artefacts in `data/`, `output/`, `figs/` and `tables/` were regenerated
by running `analysis/_master.R` end to end against the fixed code, so the
repository is now internally consistent: every committed figure and table is
what the code in the repository actually produces.

* **The calibration is unchanged.** The IMIS run reproduces the published
  posterior exactly, so `m_calib_post`, `v_calib_post_map` and the posterior
  summary carry the same values as before (the summary table differs only in the
  last one or two of 15 significant digits).

* **The deterministic CEA table changed substantially, because it had been
  stale.** `tables/05b_deterministic_cea_results.csv` previously held
  No Treatment 159232.1 / 22.08072 and Treatment 293747.6 / 22.80879, giving an
  ICER of 184755.6. Those numbers were produced by an earlier version of
  `calculate_ce_out()` and were never regenerated, so they did not match what the
  code in the repository produced even before this release. The table now holds
  115244.8 / 20.0233 and 214165.8 / 20.7226, with an ICER of 141457.4. The
  discrepancy was not caused by the discounting fix, which cannot change these
  numbers while the repository sets `d_c = d_e`.

  A useful cross-check: the deterministic and probabilistic ICERs now agree
  (141457 and 141799). Previously they differed by 29% (184756 against 143201)
  for the same model, which is what flagged the stale table.

* **The PSA results moved slightly, because the sampling is now reproducible.**
  `generate_psa_params()` now sets the seed it is given, so the draws differ from
  the unseeded ones behind the old `l_psa`. The probabilistic ICER moves from
  143201.5 to 141798.7, within Monte Carlo error for 1000 samples. Re-running
  `analysis/05a_probabilistic_analysis.R` now reproduces `l_psa` exactly.

* `tables/05a_probabilistic_cea_results.csv` no longer has the unnamed row-number
  column that `write.csv()` added without `row.names = FALSE`.

* All 21 figures in `figs/` were regenerated.

To keep the package data objects in step with the analysis output, `_master.R`
now also sources `data-raw/03_imis_output.R` and `data-raw/05a_psa_dataset.R`,
which refresh `m_calib_post`, `v_calib_post_map` and `l_psa`. Running only the
`analysis/` scripts, as the previous `_master.R` did, left those `.rda` objects
holding the *old* calibration while `output/` held the new one — and because
`generate_psa_params()` reads `m_calib_post` from the package data, the
probabilistic analysis would then have run on the stale posterior. The
`data-raw/` scripts now pass `overwrite = TRUE` so they can be re-run at all.

Note that `_master.R` re-runs the IMIS calibration, which takes a few minutes.
The previous version said the calibration and validation components were
commented out, but the `source()` calls were live.

# darthpack 0.1.0

* Archived the release that accompanies the published article in zenodo: https://zenodo.org/record/3445451.

* Version included in following manuscript:
    - Alarid-Escudero F, Krijkamp E, Pechlivanoglou P, Jalal H, Kao SY, Yang A, Enns EA. "A need for change! A coding framework for improving transparency in decision modeling". PharmacoEconomics 2019. http://dx.doi.org/10.1007/s40273-019-00837-x (In press)
    
* Added a `NEWS.md` file to track changes to the package.
