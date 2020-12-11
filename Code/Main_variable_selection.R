######################## Main_variable_selection.R ##############################
## This file computes the proposed variables selection methods for all training 
## sets (e.g. Nsim = 100 sets) produced, in the previous step. Methods of
## penalized Cox regression, including best subset selection, lasso, elastic net, 
## ridge regression and the tree-based rpart method.
##
## Functions used: doBeSS, doRegularization, doRpart, 
##                in myFunctions: myMeans, myBias, myCoverage (now not used)
################################################################################

## Functions -------------------------------------------------------------------
## sourcing of needed functions
source("Codes/Simulations/doBeSS.R")
source("Codes/Simulations/doRegularization.R")
source("Codes/Simulations/doGroupLasso.R")
source("Codes/Simulations/doCoxBoost.R")
source("Codes/Simulations/myFunctions.R")


plan(multisession)

## BeSS -------------------------------------------------------------------------
cat("BeSS selection... \n\n")

print(system.time({
  est.bess <- doBeSS(dfs, vars = xvars)
}))
coefs.bess <- est.bess$coefs
# est.bess.low <- est.bess$low
# est.bess.up <- est.bess$up
# coefs.bess.bias <- myBias(coefs.bess, true_beta); 
# bess.bias <- myMeans(coefs.bess.bias)
# coefs.bess.coverage <- myCoverage(est.bess.low, est.bess.up, true_beta);
# bess.coverage <- myMeans(coefs.bess.coverage)

## Variables 
vars.bess <- apply(coefs.bess, 1, function(x) names(which(x != 0)))
## just in case, to return a list (in apply when the elements are of the same length it returns a matrix)
if (class(vars.bess) == 'matrix') {
  vars.bess <- lapply(1:ncol(vars.bess), function(i) vars.bess[,i])
} 

## Save
save(est.bess, coefs.bess, vars.bess,
     file = paste0(dir, "VarsBeSS_", name, ".rds"))
rm(est.bess, coefs.bess, vars.bess)
cat("SAVED!: ", dir, "VarsBeSS_", name, ".rds\n\n")
##


## Lasso -----------------------------------------------------------------------
cat("Lasso selection... \n\n")

print(system.time({
  coefs.lasso <- doRegularization(dfs, alpha = 1, vars = xvars)
}))
lambdas.lasso <- coefs.lasso["lambdas"][[1]]
coefs.lasso <- coefs.lasso["coefs"][[1]]
# coefs.lasso.bias <- myBias(coefs.lasso, true_beta)
# lasso.bias <- myMeans(coefs.lasso.bias)

## Confidence intervals of the estimated coefficients (calcultated via bootstrap)
cat("Lasso 95% ci... \n\n")
print(system.time({
  ci95s.lasso <- future_map2(dfs, lambdas.lasso, function(df, lambda)
    bootstrap_regularization(df, B = 100, alph  = 1, vars = xvars, lambda = lambda), .progress = TRUE,
    .options = future_options(seed = 123456L))
}))

## Variables 
vars.lasso <- apply(coefs.lasso, 1, function(x) names(which(x != 0)))

## Variables (2): whether coefficient estimate is really relevant or not.
## evaluate whether conf interval contain 0 or not, in each of the simulation replicas
vars.lasso2 <- map(ci95s.lasso, function(ci95) which(ci95[1,] > 0 | 0 > ci95[2,])) ## Selected, differente from 0
vars.lasso2 <- map(vars.lasso2, function(elem) names(dfs[[1]])[elem])

## Save
save(coefs.lasso, vars.lasso, ci95s.lasso, vars.lasso2, 
     file = paste0(dir, "VarsLasso_", name, ".rds"))
rm(lambdas.lasso, coefs.lasso, vars.lasso, ci95s.lasso, vars.lasso2)
cat("SAVED!: ", dir, "VarsLasso_", name, ".rds\n\n")
##


## Ridge -----------------------------------------------------------------------
cat("Ridge selection... \n\n")

print(system.time({
  coefs.ridge <- doRegularization(dfs, alpha = 0, vars = xvars)
}))
lambdas.ridge <- coefs.ridge["lambdas"][[1]]
coefs.ridge <- coefs.ridge["coefs"][[1]]
# coefs.ridge.bias <- myBias(coefs.ridge, true_beta)
# ridge.bias <- myMeans(coefs.ridge.bias)

## Confidence intervals of the estimated coefficients (calcultated via bootstrap)
cat("Ridge 95% ci... \n\n")
plan(multiprocess)
print(system.time({
  ci95s.ridge <- future_map2(dfs, lambdas.ridge, function(df, lambda.ridge) 
    bootstrap_regularization(df, B = 100, alph  = 0, vars = xvars, lambda = lambda.ridge), .progress = TRUE,
    .options = future_options(seed = 123456L))
}))

## Variables
vars.ridge <- apply(coefs.ridge, 1, function(x) names(which(x != 0)))
if (class(vars.ridge) == "matrix") {
  vars.ridge <- rep(list(paste0("X", 1:xvars)), Nsim)
}

## Variables (2): whether coefficient estimate is really relevant or not.
## evaluate whether conf interval contain 0 or not, in each of the simulation replicas
vars.ridge2 <- map(ci95s.ridge, function(ci95) which(ci95[1,] > 0 | 0 > ci95[2,])) ## Selected, differente from 0   
vars.ridge2 <- map(vars.ridge2, function(elem) names(dfs[[1]])[elem])

## Save
save(coefs.ridge, ci95s.ridge, vars.ridge, vars.ridge2,
     file = paste0(dir, "VarsRidge_", name, ".rds"))
rm(lambdas.ridge, coefs.ridge, vars.ridge, ci95s.ridge, vars.ridge2)
cat("SAVED!: ", dir, "VarsRidge_", name, ".rds\n\n")
##


## Elastic net -----------------------------------------------------------------
cat("Elastic net selection... \n\n")

## Variables05: fixing alpha = 0.5
print(system.time({
  coefs.enet05 <- doRegularization(dfs, alpha = 0.5, vars = xvars)
}))
lambdas.enet05 <- coefs.enet05["lambdas"][[1]]
coefs.enet05 <- coefs.enet05["coefs"][[1]]

vars.enet05 <- apply(coefs.enet05, 1, function(x) names(which(x != 0)))

cat("Elastic net, alpha = 0.5 95% ci... \n\n")
print(system.time({
  ci95s.enet05  <- future_map2(dfs, lambdas.enet05, function(df, lambda)
    bootstrap_regularization(df, B = 100, alph  = 0.5, vars = xvars, lambda = lambda), .progress = TRUE,
    .options = future_options(seed = 123456L))
}))
# Variables05 (2)
vars.enet052 <- map(ci95s.enet05, function(ci95) which(ci95[1,] > 0 | 0 > ci95[2,])) ## Selected, different from 0
vars.enet052 <- map(vars.enet052, function(elem) names(dfs[[1]])[elem])

## Save 
save(coefs.enet05, vars.enet05, ci95s.enet05, vars.enet052,
     file = paste0(dir, "VarsEnet_", name, ".rds"))
rm(lambdas.enet05, coefs.enet, ci95s.enet, vars.enet, vars.enet2, 
   coefs.enet05, ci95s.enet05, vars.enet05, vars.enet052)
cat("SAVED!: ", dir, "VarsEnet_", name, ".rds\n\n")
##


## GroupLasso --------------------------------------------------------------
cat("GroupLasso selection... \n\n")

print(system.time({
  coefs.grouplasso <- doGroupLasso(dfs, vars = xvars, group = group)
}))
lambdas.grouplasso <- coefs.grouplasso["lambdas"][[1]]
coefs.grouplasso <- coefs.grouplasso["coefs"][[1]]

## Variables 
vars.grouplasso <- apply(coefs.grouplasso, 1, function(x) names(which(x != 0)))

## Save
save(coefs.grouplasso, vars.grouplasso,  
     file = paste0(dir, "VarsGroupLasso_", name, ".rds"))
rm(lambdas.grouplasso, coefs.grouplasso, vars.grouplasso)
cat("SAVED!: ", dir, "VarsGroupLasso_", name, ".rds\n\n")
##


## CoxBoost ----------------------------------------------------------------
cat("CoxBoost selection... \n\n")

print(system.time({
  coefs.coxboost <- doCoxBoost(dfs, vars = xvars)
}))
stepnos.coxboost <- coefs.coxboost["step_nos"][[1]]
coefs.coxboost <- coefs.coxboost["coefs"][[1]]

## Variables 
vars.coxboost <- apply(coefs.coxboost, 1, function(x) names(which(x != 0)))

## Save
save(coefs.coxboost, vars.coxboost, 
     file = paste0(dir, "VarsCoxBoost_", name, ".rds"))
rm(stepnos.coxboost, coefs.coxboost, vars.coxboost)
cat("SAVED!: ", dir, "VarsCoxBoost_", name, ".rds\n\n")
##

rm(doBeSS, doRegularization, doGroupLasso, doCoxBoost, 
   bootstrap_regularization, get.enet.coef, 
   myBias, myMeans, myCoverage)

