################################################################################ 
# This script runs the cost-effectiveness analysis of a hypothetical treatment #
# for the simulated cohort of the Sick-Sicker state-transition model (STM)     #
#                                                                              #                                                                          # 
# Authors:                                                                     #
#     - Fernando Alarid-Escudero, PhD, <fernando.alarid@cide.edu>              # 
#     - Eline Krijkamp, MS                                                     #
#     - Petros Pechlivanoglou, PhD                                             #
#     - Hawre Jalal, MD, PhD                                                   #
#     - Eva A. Enns, PhD                                                       # 
################################################################################
# The structure of this code is according to the DARTH framework               #
# https://github.com/DARTH-git/Decision-Modeling-Framework                     #
################################################################################

# rm(list = ls()) # to clean the workspace

#### 05b.1 Load packages and functions ####
#### 05b.1.1 Load packages ####
# devtools::install_github("DARTH-git/dampack") # Uncomment if dampack not installed
library(dampack) 

#### 05b.1.2 Load inputs ####
l_params_all <- load_all_params(file.init = "data-raw/01_init_params.csv",
                                file.mort = "data-raw/01_all_cause_mortality.csv") # function in darthpack

#### 05b.1.3 Load functions ####
# no required functions

#### 05b.1.4 Load calibrated parameters ####
### Load MAP as best calibration parameter set for deterministic analysis
data("v_calib_post_map")

#### 05b.2 Cost-effectiveness analysis parameters ####
### Strategy names
v_names_str <- l_params_all$v_names_str
### Number of strategies
n_str <- length(v_names_str)
### Parameters for base-case CEA
## Update base-case parameters with calibrated values at MAP
l_params_basecase <- update_param_list(l_params_all, v_calib_post_map) 

#### 05b.3 Compute cost-effectiveness outcomes ####
df_out_ce <- calculate_ce_out(l_params_all = l_params_basecase, 
                              n_wtp = 150000)
df_out_ce

#### 05b.4 Conduct CEA with deterministic output ####
### Calculate incremental cost-effectiveness ratios (ICERs)
df_cea_det <- calculate_icers(cost       = df_out_ce$Cost, 
                              effect     = df_out_ce$Effect, 
                              strategies = v_names_str)
df_cea_det
### Save CEA table with ICERs
## As .RData
save(df_cea_det, 
          file = "tables/05b_deterministic_cea_results.RData")
## As .csv
write.csv(df_cea_det, 
          file = "tables/05b_deterministic_cea_results.csv")

#### 05b.5 Plot cost-effectiveness frontier ####
plot(df_cea_det)
ggsave("figs/05b_cea_frontier.png", width = 8, height = 6)

#### 05b.6 Deterministic sensitivity analysis (DSA) ####
#### 05b.6.1 One-way sensitivity analysis (OWSA) ####
owsa_nmb <- darthpack::owsa_det(parms = c("c_Trt", "p_HS1", "u_S1", "u_Trt"), # parameter names
                     ranges = list("c_Trt" = c(6000, 13000),
                                   "p_HS1" = c(0.01, 0.50),
                                   "u_S1"  = c(0.75, 0.95),
                                   "u_Trt" = c(0.75, 0.95)),
                     nsamps = 100, # number of values  
                     FUN = calculate_ce_out, # Function to compute outputs 
                     params_basecase = l_params_basecase, # List with base-case parameters
                     outcome = "NMB",      # Output to do the OWSA on
                     strategies = v_names_str, # Names of strategies
                     n_wtp = 150000        # Extra argument to pass to FUN
                     )
plot(owsa_nmb, txtsize = 16, n_x_ticks = 5, 
     facet_scales = "free") +
     theme(legend.position = "bottom")
ggsave("figs/05b_owsa_nmb.png", width = 10, height = 6)

#### 05b.6.1 Optimal strategy with OWSA ####
owsa_opt_strat(owsa = owsa_nmb)
ggsave("figs/05b_optimal_owsa_nmb.png", width = 8, height = 6)

#### 05b.6.3 Tornado plot ####
owsa_tornado(owsa = owsa_nmb)
ggsave("figs/05b_tornado_Treatment_nmb.png", width = 8, height = 6)

#### 05b.6.2 Two-way sensitivity analysis (TWSA) ####
twsa_nmb <- darthpack::twsa_det(parm1 = "u_S1",  # parameter 1 name
                     parm2 = "u_Trt", # parameter 2 name
                     ranges = list("u_S1"  = c(0.70, 0.80),
                                   "u_Trt" = c(0.90, 1.00)),
                     nsamps = 40, # number of values  
                     FUN = calculate_ce_out, # Function to compute outputs 
                     params_basecase = l_params_basecase, # Vector with base-case parameters
                     outcome = "NMB",      # Output to do the OWSA on
                     strategies = v_names_str, # Names of strategies
                     n_wtp = 150000        # Extra argument to pass to FUN
)
plot(twsa_nmb)
ggsave("figs/05b_twsa_uS1_uTrt_nmb.png", width = 8, height = 6)
