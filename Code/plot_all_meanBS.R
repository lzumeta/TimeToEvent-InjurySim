
source("Code/plot_Brier_Curves.R")


## Plot 1 -----------------
## load very small sample size, scenario 2
dir <- "Results/Scenario2/"
name <- "Nsim_100_Nobs_60_cens_0.75_xvars_50_verysmallsize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "pecBoot632plus_", name, ".rds"))

p1 <-plot_Brier_Curves(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
                       pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                       splitMethod = "Boot632plusErr")

## load very small sample size, scenario 3
dir <- "Results/Scenario3/"
name <- "Nsim_100_Nobs_60_cens_0.75_xvars_50_verysmallsize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "pecBoot632plus_", name, ".rds"))

p2 <-plot_Brier_Curves(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
                       pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                       splitMethod = "Boot632plusErr")

p3 <- grid.arrange(p1, p2, ncol = 2)
ggsave(plot = p3, filename = "Figures/meanBrierCurves_verysmallsize.pdf", width = 35, height = 12)



## Plot 2 -----------------
## load small sample size, scenario 2
dir <- "Results/Scenario2/"
name <- "Nsim_100_Nobs_191_cens_0.75_xvars_50_smallsize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "pecBoot632plus_", name, ".rds"))

p1 <-plot_Brier_Curves(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
                       pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                       splitMethod = "Boot632plusErr")

## load small sample size, scenario 3
dir <- "Results/Scenario3/"
name <- "Nsim_100_Nobs_191_cens_0.75_xvars_50_smallsize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "pecBoot632plus_", name, ".rds"))

p2 <-plot_Brier_Curves(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
                       pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                       splitMethod = "Boot632plusErr")

p3 <- grid.arrange(p1, p2, ncol = 2)
ggsave(plot = p3, filename = "Figures/meanBrierCurves_smallsize.pdf", width = 35, height = 12)



## Plot 3 -----------------
## load large sample size, scenario 2
dir <- "Results/Scenario2/"
name <- "Nsim_100_Nobs_391_cens_0.75_xvars_50_largesize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "pecBoot632plus_", name, ".rds"))

p1 <-plot_Brier_Curves(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
                       pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                       splitMethod = "Boot632plusErr")

## load large sample size, scenario 2
dir <- "Results/Scenario3/"
name <- "Nsim_100_Nobs_391_cens_0.75_xvars_50_largesize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "pecBoot632plus_", name, ".rds"))

p2 <-plot_Brier_Curves(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
                       pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                       splitMethod = "Boot632plusErr")

p3 <- grid.arrange(p1, p2, ncol = 2)
ggsave(plot = p3, filename = "Figures/meanBrierCurves_largesize.pdf", width = 35, height = 12)



## Plot 4 -----------------
## load large sample size, scenario 2
dir <- "Results/Scenario2/"
name <- "Nsim_100_Nobs_670_cens_0.75_xvars_50_verylargesize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "pecBoot632plus_", name, ".rds"))

p1 <-plot_Brier_Curves(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
                       pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                       splitMethod = "Boot632plusErr")

## load very small sample size, scenario 2
dir <- "Results/Scenario3/"
name <- "Nsim_100_Nobs_670_cens_0.75_xvars_50_verylargesize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "pecBoot632plus_", name, ".rds"))

p2 <-plot_Brier_Curves(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
                       pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                       splitMethod = "Boot632plusErr")

p3 <- grid.arrange(p1, p2, ncol = 2)
ggsave(plot = p3, filename = "Figures/meanBrierCurves_verylargesize.pdf", width = 35, height = 12)


