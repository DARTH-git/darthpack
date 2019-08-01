################################################################################ 
# This script runs the cohort implementation of the Sick-Sicker                #
# state-transition model (STM)                                                 #
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

#### 02.1 Load packages and functions ####
#### 02.1.1 Load packages and functions ####
library(dplyr)    # For data manipulation
library(survival) # For plotting state-transition diagram

#### 02.1.2 Load inputs ####
l_params_all <- load_all_params(file.init = "data-raw/01_init_params.csv",
                                file.mort = "data-raw/01_all_cause_mortality.csv") # function in darthpack

#### 02.1.3 Load functions ####
# no functions required

#### 02.2 Run STM ####
### Create list of model output
l_out_stm <- decision_model(l_params_all = l_params_all)

### Plot Markov cohort trace
png("figs/02_trace_plot.png")
  matplot(l_out_stm$m_M,
          xlab = "Cycle", ylab = "Proportion")
  legend("right", legend = l_params_all$v_n, 
         pch = as.character(1:4), col = 1:4)
dev.off()

### Plot state-transition diagram
png("figs/02_model_diagram.png")
  connect <- (l_out_stm$a_P[,,1] > 0)
  survival::statefig(layout = c(2, 2), connect = connect )
dev.off()
