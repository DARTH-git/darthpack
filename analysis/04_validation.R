################################################################################ 
# This script conducts an internal validation of the Sick-Sicker               # 
# state-transition model (STM) by comparing the model-predicted outputs        #
# evaluated at the calibrated parameters vs the calibration targets. This      #
# script could be modified by adding an external validation exercise.          #
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

#### 04.1 Load packages and functions ####
#### 04.1.1 Load packages ####
# Dependencies have been loaded with 'darthpack'

#### 04.1.2 Load inputs ####
l_params_all <- load_all_params(file.init = "data-raw/01_init_params.csv",
                                file.mort = "data-raw/01_all_cause_mortality.csv") # function in darthpack

#### 04.1.3 Load functions ####
# no required functions

#### 04.1.4 Load targets and calibrated parameters ####
data("03_calibration_targets")
data("m_calib_post")
data("v_calib_post_map")

#### 04.2 Compute model-predicted outputs ####
#### 04.2.1 Compute model-predicted outputs for each sample of posterior distribution ####
### Number of posterior samples
n_samp <- nrow(m_calib_post)

### Define matrices to store model outputs
m_out_surv <- matrix(NA, nrow = n_samp, ncol = nrow(SickSicker_targets$Surv))
colnames(m_out_surv) <- SickSicker_targets$Surv$Time
m_out_prev <- matrix(NA, nrow = n_samp, ncol = nrow(SickSicker_targets$Prev))
colnames(m_out_prev) <- SickSicker_targets$Prev$Time
m_out_prop <- matrix(NA, nrow = n_samp, ncol = nrow(SickSicker_targets$PropSicker))
colnames(m_out_prop) <- SickSicker_targets$PropSicker$Time

### Evaluate model at each posterior sample and store results
for(i in 1:n_samp){ # i = 1
  l_out_post <- calibration_out(v_params_calib = m_calib_post[i, ], 
                                  l_params_all = l_params_all)
  m_out_surv[i, ] <- l_out_post$Surv
  m_out_prev[i, ] <- l_out_post$Prev
  m_out_prop[i, ] <- l_out_post$PropSicker
  cat('\r', paste(round(i/n_samp * 100), "% done", sep = " ")) # display progress
}

### Create data frames with model predicted outputs
df_out_surv <- data.frame(Type = "Model", 
                          Target = "Survival",
                          m_out_surv, 
                          check.names = FALSE)
df_out_prev <- data.frame(Type = "Model", 
                          Target = "Prevalence",
                          m_out_prev, 
                          check.names = FALSE)
df_out_prop <- data.frame(Type = "Model", 
                          Target = "Proportion of Sicker",
                          m_out_prop, 
                          check.names = FALSE)

### Transform data frames to long format
df_out_surv_lng <- reshape2::melt(df_out_surv, 
                         id.vars = c("Type", "Target"), 
                         variable.name = "Time")
df_out_prev_lng <- reshape2::melt(df_out_prev, 
                         id.vars = c("Type", "Target"), 
                         variable.name = "Time")
df_out_prop_lng <- reshape2::melt(df_out_prop, 
                         id.vars = c("Type", "Target"), 
                         variable.name = "Time")

### Compute posterior model-predicted 95% CI
df_out_surv_sum <- data_summary(df_out_surv_lng, varname = "value",
                                groupnames = c("Type", "Target", "Time"))
df_out_prev_sum <- data_summary(df_out_prev_lng, varname = "value",
                                groupnames = c("Type", "Target", "Time"))
df_out_prop_sum <- data_summary(df_out_prop_lng, varname = "value",
                                groupnames = c("Type", "Target", "Time"))

#### 04.3.2 Compute model-predicted outputs at MAP estimate ####
l_out_calib_map <- calibration_out(v_params_calib = v_calib_post_map, 
                                   l_params_all = l_params_all)

#### 04.4 Internal validation: Model-predicted outputs vs. targets ####
### TARGET 1: Survival ("Surv")
png("figs/04_posterior_vs_targets_survival.png", 
    width = 8, height = 6, units = 'in', res = 300)
plotrix::plotCI(x = SickSicker_targets$Surv$Time, y = SickSicker_targets$Surv$value, 
                ui = SickSicker_targets$Surv$ub,
                li = SickSicker_targets$Surv$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Alive)")
lines(x = SickSicker_targets$Surv$Time,
      y = df_out_surv_sum$lb, col = "red", lty = 2)
lines(x = SickSicker_targets$Surv$Time,
      y = df_out_surv_sum$ub, col = "red", lty = 2)
points(x = SickSicker_targets$Surv$Time, 
       y = l_out_calib_map$Surv, 
       pch = 8, col = "red")
legend("bottomright", 
       legend = c("Target", 
                  "Model-predicted 95% CrI",
                  "Model-predicted output at MAP"),
       col = c("black", "red", "red"), 
       pch = c(1, NA, 8),
       lty = c(NA, 2, NA))
dev.off()

### TARGET 2: Prevalence ("Prev")
png("figs/04_posterior_vs_targets_prevalence.png", 
    width = 8, height = 6, units = 'in', res = 300)
plotrix::plotCI(x = SickSicker_targets$Prev$Time, y = SickSicker_targets$Prev$value, 
                ui = SickSicker_targets$Prev$ub,
                li = SickSicker_targets$Prev$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Sick+Sicker)")
lines(x = SickSicker_targets$Prev$Time,
      y = df_out_prev_sum$lb, col = "red", lty = 2)
lines(x = SickSicker_targets$Prev$Time,
      y = df_out_prev_sum$ub, col = "red", lty = 2)
points(x = SickSicker_targets$Prev$Time, 
       y = l_out_calib_map$Prev, 
       pch = 8, col = "red")
legend("bottomright", 
       legend = c("Target", 
                  "Model-predicted 95% CrI",
                  "Model-predicted output at MAP"),
       col = c("black", "red", "red"), 
       pch = c(1, NA, 8),
       lty = c(NA, 2, NA))
dev.off()

### TARGET 3: Proportion who are Sicker ("PropSicker"), among all those afflicted (Sick+Sicker)
png("figs/04_posterior_vs_targets_proportion_sicker.png", 
    width = 8, height = 6, units = 'in', res = 300)
plotrix::plotCI(x = SickSicker_targets$PropSick$Time, y = SickSicker_targets$PropSick$value, 
                ui = SickSicker_targets$PropSick$ub,
                li = SickSicker_targets$PropSick$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Sicker | Sick+Sicker)")
lines(x = SickSicker_targets$PropSicker$Time,
      y = df_out_prop_sum$lb, col = "red", lty = 2)
lines(x = SickSicker_targets$PropSicker$Time,
      y = df_out_prop_sum$ub, col = "red", lty = 2)
points(x = SickSicker_targets$PropSicker$Time, 
       y = l_out_calib_map$PropSicker, 
       pch = 8, col = "red")
legend("bottomright", 
       legend = c("Target", 
                  "Model-predicted 95% CrI",
                  "Model-predicted output at MAP"),
       col = c("black", "red", "red"), 
       pch = c(1, NA, 8),
       lty = c(NA, 2, NA))
dev.off()