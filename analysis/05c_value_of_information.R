################################################################################ 
# This script conducts the value of information (VOI) analysis of the          #
# cost-effectiveness analysis (CEA) of a hypothetical treatment for the        #  
# simulated cohort of the Sick-Sicker state-transition model (STM)             #
# (PSA) dataset                                                                #
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

#### 05c.1 Load packages, functions and data ####
#### 05c.1.1 Load packages ####
library(dampack)   # decision-analytic modeling visualization tool

#### 05c.1.2 Load functions ####
#### 05c.1.3 Load PSA dataset ####
data("l_psa")

#### 05c.2 Define VOI inputs ####
### Vector with willingness-to-pay (WTP) thresholds
v_wtp <- seq(0, 200000, by = 10000)

#### 05c.3 Expected value of perfect information (EVPI) ####
evpi <- calc_evpi(wtp = v_wtp, psa = l_psa)
plot(evpi, effect_units = "QALY")
ggsave("figs/05c_evpi.png", width = 8, height = 6)

#### 05c.4 Expected value of partial perfect information (EVPPI) ####

#### 05c.5 Expected value of sample information (EVSI) ####
