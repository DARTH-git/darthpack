################################################################################ 
# This script runs all the components of the DARTH framework using the         #
# Sick-Sicker model as testbed. It computes the cost-effectiveness analysis of #
# a hypothetical treatment for the simulated cohort of the Sick-Sicker         #
# state-transition model (STM)                                                 #
#                                                                              # 
# Authors:                                                                     #
#     - Fernando Alarid-Escudero, PhD, <fernando.alarid@cide.edu>              # 
#     - Eline Krijkamp, MSc                                                    #
#     - Petros Pechlivanoglou, PhD                                             #
#     - Hawre Jalal, MD, PhD                                                   #
#     - Eva A. Enns, PhD                                                       #
################################################################################
# The structure of this code is according to the DARTH framework               #
# https://github.com/DARTH-git/Decision-Modeling-Framework                     #
################################################################################
# rm(list = ls()) # to clean the workspace

#### 00 Install and load packages ####
### Uncomment if you don't have all required packages installed
# source("analysis/app0_package_setup.R", echo = TRUE) 

#### 01 Load inputs ####
source("analysis/01_model_inputs.R", echo = TRUE)

#### 02 Load simulation model and test it ####
source("analysis/02_decision_model.R", echo = TRUE)

#### 03 Calibrate simulation model ####
### Uncomment if you want to rerun the calibration component 
source("analysis/03_calibration.R", echo = TRUE)

#### 04 Validate simulation model ####
### Uncomment if you want to rerun the validation component
source("analysis/04_validation.R", echo = TRUE)

#### 05a Conduct probabilistic analysis ####
source("analysis/05a_probabilistic_analysis.R", echo = TRUE)

#### 05b Conduct deterministic analysis ####
source("analysis/05b_deterministic_analysis.R", echo = TRUE)

#### 05c Conduct value of information analysis ####
source("analysis/05c_value_of_information.R", echo = TRUE)
