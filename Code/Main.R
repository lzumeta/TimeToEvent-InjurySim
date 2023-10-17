######################## Main.R #################################################
## This file runs all analyses within the simulation study.
##
## 1) Generation of the design matrix
## 2) Generation of Nsim data sets, splitting each Nsim data set in training 
##    and testing sets. (saving)
## 3) Variable  selection per each data sets. It runs all proposed variable 
##    selection technique (saving, after each method)
## 4) Calculation of predictive measures (saving)
## 5) Plot the results
################################################################################


## Packages ---------------------------------------------------------------------
library(MASS)
library(tidyverse)
library(furrr)


## Functions --------------------------------------------------------------------
## Sourcing of needed functions:
source("Code/design_matrix.R")
source("Code/mysim_survdata.R")


## 1) Design matrix -------------------------------------------------------------
cat("Generating design matix X... \n\n")
if (scenario == "Scenario 1") { 
  if (var_method == "RPART") {   ## NOT USED  (....for Scenario 4, true model: rpart)
    X <- design_matrix(scenario = scenario, Nobs = Nobs, xvars = xvars, noise = noise,
                       cluster_sizes = cluster_sizes, data = data, 
                       augment_rows = augment_rows, w = w)
    
    # X$X53 <- ifelse(X[,21] < c1, 1, 0)
    # X$X54 <- ifelse(X[,21]>=c1 & X[,27]>=c2, 1, 0)
    # X$X55 <- ifelse(X[,21]>=c1 & X[,27]<c2 & X[,45]==0 & X[,31]<c3, 1, 0)
    # X$X56 <- ifelse(X[,21]>=c1 & X[,27]<c2 & X[,45]==0 & X[,31]>=c3, 1, 0)
    # X$X57 <- ifelse(X[,21]>=c1 & X[,27]<c2 & X[,45]==1 & X[,16]<c4, 1, 0)
    # X$X58 <- ifelse(X[,21]>=c1 & X[,27]<c2 & X[,45]==1 & X[,16]>=c4, 1, 0)
    
    X$X53 <- ifelse(X[,19] < c1, 1, 0)
    X$X54 <- ifelse(X[,19]>=c1 & X[,50]>=c2, 1, 0)
    X$X55 <- ifelse(X[,19]>=c1 & X[,50]<c2, 1, 0)
    
  } else {
    X <- design_matrix(scenario = scenario, Nobs = Nobs, xvars = xvars, noise = noise,
                       cluster_sizes = cluster_sizes, data = data, 
                       augment_rows = augment_rows, w = w)
  }
  w <- X$w
  X$w <- NULL
  
} else {
  X <- design_matrix(scenario = scenario, Nobs = Nobs, xvars = xvars, noise = noise,
                     cluster_sizes = cluster_sizes, w = w)
  w <- X$w
  X$w <- NULL
}


## 2) Simulate data ---------------------------------------------------------
cat("Generating Nsim, ", Nsim, "data sets... \n\n")
dfs <- mysim_survdata(Nobs, Tmax, Nsim, xvars, true_beta, censorship, knots, X,
                      frailty = frailty, cluster.num = cluster_num, cluster.sizes = cluster_sizes,
                      w = w)

save(Nsim, Tmax, censorship, knots, xvars, Nobs, X, true_beta, dfs,
     cluster_sizes, cluster_num,
     file = paste0(dir, "Data_", name, ".rds"))
cat("SAVED!: ", dir, "Data_", name, ".rds\n\n")
rm(X, w, vs, idx, baseline.build, censor.x, 
   design_matrix, generate.lm, make.margeffect, mysim_survdata,
   NK_clusters, sim.survdata, train_test, user.baseline)


## 3) Variable selection ---------------------------------------------------
cat("Starting variable selection... \n\n")
source("Code/Main_variable_selection.R")


## 4) Predictive measures --------------------------------------------------
cat("Starting calculation of predictive measures... \n\n")
print(system.time({source("Code/Main_predictive_capacity.R")}))
save(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
     pecBoot632plus_grouplasso, pecBoot632plus_coxboost,
     file = paste0(dir, "pecBoot632plus_", name, ".rds"))
cat("SAVED!: ", dir, "pecBoot632plus_", name, ".rds\n\n")

rm(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
   pecBoot632plus_grouplasso, pecBoot632plus_coxboost)


## 5) Plot the results -----
source("Code/plot_selectedVars.R")
source("Code/plot_Brier_Curves.R")
source("Code/plot_IBS_boxplots.R")

## Create figure directory
sce <- ifelse(scenario == "Scenario 2", "Scenario2/", "Scenario3/")
fig.dir <- paste0("Figures/", sce, name)
if (!dir.exists(fig.dir)) dir.create(fig.dir)

## SelectedVars Plot
pdf(file = paste0(fig.dir, "SelectedVars.pdf"), width = 18, height = 10)
load(paste0(dir, "VarsBeSS_", name, ".rds")); load(paste0(dir, "VarsLasso_", name, ".rds"))
load(paste0(dir, "VarsEnet_", name, ".rds")); load(paste0(dir, "VarsRidge_", name, ".rds"))
load(paste0(dir, "VarsGroupLasso_", name, ".rds")); load(paste0(dir, "VarsCoxBoost_", name, ".rds"))
plot_selectedVars(ci = ci, CaseStudy = CaseStudy)
dev.off()
rm(est.bess, coefs.bess, vars.bess,
   coefs.lasso, vars.lasso, ci95s.lasso, vars.lasso2,
   coefs.enet05, vars.enet05, ci95s.enet05, vars.enet052, 
   coefs.ridge, vars.ridge, ci95s.ridge, vars.ridge2,
   coefs.grouplasso, vars.grouplasso,
   coefs.coxboost, vars.coxboost)

## Briers Curve Plot
pdf(file = paste0(fig.dir, "Brier_Curves.pdf"), width = 12, height = 8)
load(paste0(dir, "pecBoot632plus_", name, ".rds"))
plot_Brier_Curves(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
                  pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                  splitMethod = "Boot632plusErr")
plot_Brier_Curves(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
                  pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                  splitMethod = "AppErr")
dev.off()
## Integrated Brier Score Boxplots
pdf(file = paste0(fig.dir, "IBS_boxplots.pdf"), width = 6, height = 4)
plot_IBS_boxplots(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
                  pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                  splitMethod = "Boot632plusErr")
plot_IBS_boxplots(pecBoot632plus_bess, pecBoot632plus_lasso, pecBoot632plus_enet, pecBoot632plus_ridge, 
                  pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                  splitMethod = "AppErr")
dev.off()
##



