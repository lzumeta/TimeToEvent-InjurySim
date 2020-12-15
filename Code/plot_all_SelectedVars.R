library(gridExtra)

source("Code/plot_selectedVars.R")

ci <- TRUE
CaseStudy <- FALSE

## Plot 1 -----------------
## load very small sample size, scenario 2
dir <- "Results/Scenario2/"
name <- "Nsim_100_Nobs_60_cens_0.75_xvars_50_verysmallsize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "VarsBeSS_", name, ".rds")); load(paste0(dir, "VarsLasso_", name, ".rds"))
load(paste0(dir, "VarsEnet_", name, ".rds")); load(paste0(dir, "VarsRidge_", name, ".rds"))
load(paste0(dir, "VarsGroupLasso_", name, ".rds")); load(paste0(dir, "VarsCoxBoost_", name, ".rds"))

p1 <- plot_selectedVars(ci = ci, CaseStudy = CaseStudy)

## load very small sample size, scenario 3
dir <- "Results/Scenario3/"
name <- "Nsim_100_Nobs_60_cens_0.75_xvars_50_verysmallsize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "VarsBeSS_", name, ".rds")); load(paste0(dir, "VarsLasso_", name, ".rds"))
load(paste0(dir, "VarsEnet_", name, ".rds")); load(paste0(dir, "VarsRidge_", name, ".rds"))
load(paste0(dir, "VarsGroupLasso_", name, ".rds")); load(paste0(dir, "VarsCoxBoost_", name, ".rds"))

p2 <- plot_selectedVars(ci = ci, CaseStudy = CaseStudy)

p3 <- grid.arrange(p1, p2, ncol = 2)
ggsave(plot = p3, filename = "Figures/SelectedVars_verysmallsize.pdf", width = 35, height = 12)



## Plot 2 -----------------
## load small sample size, scenario 2
dir <- "Results/Scenario2/"
name <- "Nsim_100_Nobs_191_cens_0.75_xvars_50_smallsize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "VarsBeSS_", name, ".rds")); load(paste0(dir, "VarsLasso_", name, ".rds"))
load(paste0(dir, "VarsEnet_", name, ".rds")); load(paste0(dir, "VarsRidge_", name, ".rds"))
load(paste0(dir, "VarsGroupLasso_", name, ".rds")); load(paste0(dir, "VarsCoxBoost_", name, ".rds"))

p1 <- plot_selectedVars(ci = ci, CaseStudy = CaseStudy)

## load small sample size, scenario 3
dir <- "Results/Scenario3/"
name <- "Nsim_100_Nobs_191_cens_0.75_xvars_50_smallsize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "VarsBeSS_", name, ".rds")); load(paste0(dir, "VarsLasso_", name, ".rds"))
load(paste0(dir, "VarsEnet_", name, ".rds")); load(paste0(dir, "VarsRidge_", name, ".rds"))
load(paste0(dir, "VarsGroupLasso_", name, ".rds")); load(paste0(dir, "VarsCoxBoost_", name, ".rds"))

p2 <- plot_selectedVars(ci = ci, CaseStudy = CaseStudy)

p3 <- grid.arrange(p1, p2, ncol = 2)
ggsave(plot = p3, filename = "Figures/SelectedVars_smallsize.pdf", width = 35, height = 12)



## Plot 3 -----------------
## load large sample size, scenario 2
dir <- "Results/Scenario2/"
name <- "Nsim_100_Nobs_391_cens_0.75_xvars_50_largesize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "VarsBeSS_", name, ".rds")); load(paste0(dir, "VarsLasso_", name, ".rds"))
load(paste0(dir, "VarsEnet_", name, ".rds")); load(paste0(dir, "VarsRidge_", name, ".rds"))
load(paste0(dir, "VarsGroupLasso_", name, ".rds")); load(paste0(dir, "VarsCoxBoost_", name, ".rds"))

p1 <- plot_selectedVars(ci = ci, CaseStudy = CaseStudy)

## load large sample size, scenario 2
dir <- "Results/Scenario3/"
name <- "Nsim_100_Nobs_391_cens_0.75_xvars_50_largesize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "VarsBeSS_", name, ".rds")); load(paste0(dir, "VarsLasso_", name, ".rds"))
load(paste0(dir, "VarsEnet_", name, ".rds")); load(paste0(dir, "VarsRidge_", name, ".rds"))
load(paste0(dir, "VarsGroupLasso_", name, ".rds")); load(paste0(dir, "VarsCoxBoost_", name, ".rds"))

p2 <- plot_selectedVars(ci = ci, CaseStudy = CaseStudy)

p3 <- grid.arrange(p1, p2, ncol = 2)
ggsave(plot = p3, filename = "Figures/SelectedVars_largesize.pdf", width = 35, height = 12)



## Plot 4 -----------------
## load large sample size, scenario 2
dir <- "Results/Scenario2/"
name <- "Nsim_100_Nobs_670_cens_0.75_xvars_50_verylargesize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "VarsBeSS_", name, ".rds")); load(paste0(dir, "VarsLasso_", name, ".rds"))
load(paste0(dir, "VarsEnet_", name, ".rds")); load(paste0(dir, "VarsRidge_", name, ".rds"))
load(paste0(dir, "VarsGroupLasso_", name, ".rds")); load(paste0(dir, "VarsCoxBoost_", name, ".rds"))

p1 <- plot_selectedVars(ci = ci, CaseStudy = CaseStudy)

## load very small sample size, scenario 2
dir <- "Results/Scenario3/"
name <- "Nsim_100_Nobs_670_cens_0.75_xvars_50_verylargesize"
dir <- paste0(dir, name, "/")
load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "VarsBeSS_", name, ".rds")); load(paste0(dir, "VarsLasso_", name, ".rds"))
load(paste0(dir, "VarsEnet_", name, ".rds")); load(paste0(dir, "VarsRidge_", name, ".rds"))
load(paste0(dir, "VarsGroupLasso_", name, ".rds")); load(paste0(dir, "VarsCoxBoost_", name, ".rds"))

p2 <- plot_selectedVars(ci = ci, CaseStudy = CaseStudy)

p3 <- grid.arrange(p1, p2, ncol = 2)
ggsave(plot = p3, filename = "Figures/SelectedVars_verylargesize.pdf", width = 35, height = 12)



