################################################################################ 
# This script conducts the probabilistic sensitivity analysis (PSA) of the     #
# cost-effectiveness analysis of a hypothetical treatment for the simulated    #
# cohort of the Sick-Sicker state-transition model (STM) to create PSA dataset #
#                                                                              # 
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

rm(list = ls()) # to clean the workspace

#### 05a.1 Load packages and functions ####
#### 05a.1.1 Load packages ####
# PSA functionality
library(dampack)   # decision-analytic modeling visualization tool

#### 05a.1.2 Load inputs ####
l_params_all <- load_all_params(file.init = "data-raw/01_init_params.csv",
                                file.mort = "data-raw/01_all_cause_mortality.csv") # function in darthpack

#### 05a.1.3 Load functions ####
# no required functions

#### 05a.2 Cost-effectiveness analysis parameters ####
### Strategy names
v_names_str <- l_params_all$v_names_str
### Number of strategies
n_str <- length(v_names_str)

#### 05a.3 Setup probabilistic analysis ####
### Number of simulations
n_sim <- 1000

### Generate PSA input dataset
df_psa_input <- generate_psa_params(n_sim = n_sim)

### Initialize matrices for PSA output 
## Matrix of costs
df_c <- as.data.frame(matrix(0, 
                             nrow = n_sim,
                             ncol = n_str))
colnames(df_c) <- v_names_str
## Matrix of effectiveness
df_e <- as.data.frame(matrix(0, 
                             nrow = n_sim,
                             ncol = n_str))
colnames(df_e) <- v_names_str

#### 05a.4 Conduct probabilistic sensitivity analysis ####
### Run decision model on each parameter set of PSA input dataset to produce
### PSA outputs for cost and effects
for(i in 1:n_sim){ # i <- 1
  l_psa_input <- update_param_list(l_params_all, df_psa_input[i,])
  df_out_temp <- calculate_ce_out(l_psa_input)
  df_c[i, ] <- df_out_temp$Cost
  df_e[i, ] <- df_out_temp$Effect
  # Display simulation progress
  if(i/(n_sim/10) == round(i/(n_sim/10),0)) {
    cat('\r', paste(i/n_sim * 100, "% done", sep = " "))
  }
}

### Create PSA object for dampack
l_psa <- make_psa_obj(cost = df_c, 
                      effectiveness = df_e, 
                      parameters = df_psa_input, 
                      strategies = v_names_str)

#### 05a.5 Save PSA objects ####
save(df_psa_input, df_c, df_e, v_names_str, n_str,
     l_psa,
     file = "output/05a_psa_dataset.RData")

#### 05a.6 Create probabilistic analysis graphs ####
data("l_psa") # stored as data object in 'darthpack'

### Vector with willingness-to-pay (WTP) thresholds
v_wtp <- seq(0, 200000, by = 10000)

#### 05a.6.1 Cost-effectiveness scatter plot ####
plot(l_psa)
ggsave("figs/05a_cea_plane_scatter.png", width = 8, height = 6)

#### 05a.6.2 Conduct CEA with probabilistic output ####
### Compute expected costs and effects for each strategy from the PSA
df_out_ce_psa <- summary(l_psa)
### Calculate incremental cost-effectiveness ratios (ICERs)
df_cea_psa <- calculate_icers(cost = df_out_ce_psa$meanCost, 
                              effect = df_out_ce_psa$meanEffect,
                              strategies = df_out_ce_psa$Strategy)
df_cea_psa
### Save CEA table with ICERs
## As .RData
# save(df_cea_psa, 
#      file = "tables/05a_probabilistic_cea_results.RData")
save(df_cea_psa, 
     file = "data/05a_probabilistic_cea_results.RData")
## As .csv
write.csv(df_cea_psa, 
          file = "tables/05a_probabilistic_cea_results.csv")

#### 05a.6.3 Plot cost-effectiveness frontier ####
plot(df_cea_psa)
ggsave("figs/05a_cea_frontier_psa.png", width = 8, height = 6)

#### 05a.6.4 Cost-effectiveness acceptability curves (CEACs) and frontier (CEAF) ####
ceac_obj <- ceac(wtp = v_wtp, psa = l_psa)
### Regions of highest probability of cost-effectiveness for each strategy
summary(ceac_obj)
### CEAC & CEAF plot
plot(ceac_obj)
ggsave("figs/05a_ceac_ceaf.png", width = 8, height = 6)

#### 05a.6.3 Expected Loss Curves (ELCs) ####
elc_obj <- calc_exp_loss(wtp = v_wtp, psa = l_psa)
elc_obj
plot(elc_obj, log_y = FALSE)
ggsave("figs/05a_elc.png", width = 8, height = 6)

#### 05a.7 Create linear regression metamodeling sensitivity analysis graphs ####
#### 05a.7.1 One-way sensitivity analysis (OWSA) ####
owsa_lrm_nmb <- owsa(l_psa, parms = c("c_Trt", "p_HS1", "u_S1", "u_Trt"),
                     outcome = "nmb", wtp = 150000)
plot(owsa_lrm_nmb, txtsize = 16, n_x_ticks = 5, 
     facet_scales = "free") +
  theme(legend.position = "bottom")
ggsave("figs/05a_owsa_lrm_nmb.png", width = 10, height = 6)  

#### 05a.7.2 Optimal strategy with OWSA ####
owsa_opt_strat(owsa = owsa_lrm_nmb)
ggsave("figs/05a_optimal_owsa_lrm_nmb.png", width = 8, height = 6)

#### 05a.7.3 Tornado plot ####
owsa_tornado(owsa = owsa_lrm_nmb, strategy = "Treatment")
ggsave("figs/05a_tornado_lrm_Treatment_nmb.png", width = 8, height = 6)

#### 05a.7.4 Two-way sensitivity analysis (TWSA) ####
twsa_lrm_nmb <- twsa(l_psa, parm1 = "u_S1", parm2 = "u_Trt",
                     outcome = "nmb", wtp = 150000)
plot(twsa_lrm_nmb)
ggsave("figs/05a_twsa_lrm_uS1_uTrt_nmb.png", width = 8, height = 6)  
