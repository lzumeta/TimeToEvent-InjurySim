######################## Main_predictive_capacity.R #############################
## It computes time prediction error curves (pec-s) or also known as, 
## time-dependent Brier Score-s, for each frailty model fitted.
## We use Boot632plus method for validating this predictive capacity. This is a
## linear combination of AppErr and BootCv using weights dependent on how the
## models perform in permuted data
################################################################################


## Packages --------------------------------------------------------------------
library(survival)
library(pec)


## Functions -------------------------------------------------------------------
source("Code/predictSurvProb.R") ## to predict coxph.penal models 
source("Code/BootstrapCrossValidation.R")

## vector of times: every model is evaluated at the same time points
## final follow-up times are not considered, since Brier Score is not stable at these points
times <- round(seq(10, Tmax - 200, length.out = 1000))


## BeSS ------------------------------------------------------------------------
cat("Calculating prediction error curves for BeSS.... \n\n")
load(paste0(dir, "VarsBeSS_", name, ".rds"))

## coxph + frailty(id, distr = 'gaussian') with BeSS selected variables 
fgaus_models_bess <- future_map2(vars.bess, dfs, function(var.bess, df) {
  if (length(var.bess) == 0) var.bess <- 1
  form <- paste0("Surv(time, status)~", paste0(var.bess, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  m_bess_fgaus <- coxph(as.formula(form), 
                        data = df, y = TRUE, x = TRUE,
                        control = coxph.control(eps = 1e-11, iter.max = 500)
  )
  
  return(m_bess_fgaus)
}, .options = future_options(seed = 123456L)) 


# plan(multisession) ## (not working)
# pecBoot632plus_bess <- future_pmap(list(dfs, fgaus_models_bess, vars.bess), function(df, model.bess, var.bess) {
#   if(length(var) == 0) var.bess <- 1
#   form <- paste0("Surv(time, status)~", paste0(var.bess, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
#   model.bess[["formula"]] <- as.formula(form)
#   pec(object=model.bess,
#       formula=Surv(time,status)~1,
#       data=df,
#       times = times, exact =  F,
#       #exact=TRUE, maxtime = 2000,
#       cens.model="marginal",
#       splitMethod="Boot632plus", B = 100,
#       verbose = F
#   )
# })


pecBoot632plus_bess <- vector("list", Nsim)
for (i in 1:Nsim) {
  cat("i = ", i, "\n\n")
  df <- dfs[[i]]
  model <- fgaus_models_bess[[i]]
  var <- vars.bess[[i]]
  if(length(var) == 0) var <- 1
  form <- paste0("Surv(time, status)~", paste0(var, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  model[["formula"]] <- as.formula(form)
  pecBoot632plus_bess[[i]] <- try(my_pec(object=model,
                                         formula=Surv(time,status)~1,
                                         data=df,
                                         times = times, exact =  F,
                                         #exact=TRUE, maxtime = 2000,
                                         cens.model="marginal",
                                         splitMethod="Boot632plus", B = 100,
                                         verbose = F))
}


rm(est.bess, coefs.bess, vars.bess,
   fgaus_models_bess)


## Lasso ------------------------------------------------------------------------
cat("Calculating prediction error curves for Lasso.... \n\n")
load(paste0(dir, "VarsLasso_", name, ".rds"))

## coxph + frailty(id, distr = 'gaussian') with Lasso selected variables 
fgaus_models_lasso <- future_map2(vars.lasso, dfs, function(var.lasso, df) {
  if (length(var.lasso) == 0) var.lasso <- 1
  form <- paste0("Surv(time, status)~", paste0(var.lasso, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  m_lasso_fgaus <- coxph(as.formula(form), 
                         data = df, y = TRUE, x = TRUE,
                         control = coxph.control(eps = 1e-11, iter.max = 500))
  
  return(m_lasso_fgaus)
}, .options = future_options(seed = 123456L)) 


pecBoot632plus_lasso <- vector("list", Nsim)
for (i in 1:Nsim) {
  cat("i = ", i, "\n\n")
  df <- dfs[[i]]
  model <- fgaus_models_lasso[[i]]
  var <- vars.lasso[[i]]
  if(length(var) == 0) var <- 1
  form <- paste0("Surv(time, status)~", paste0(var, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  model[["formula"]] <- as.formula(form)
  pecBoot632plus_lasso[[i]] <- try(my_pec(object=model,
                                          formula=Surv(time,status)~1,
                                          data=df,
                                          times = times, exact =  F,
                                          #exact=TRUE, maxtime = 2000,
                                          cens.model="marginal",
                                          splitMethod="Boot632plus", B = 100, # M = Nobs,
                                          verbose= F))
}

#lapply(pecBoot632plus_lasso, function(x) class(x) == "try-error") %>% unlist() %>% sum()
rm(coefs.lasso, vars.lasso, ci95s.lasso, vars.lasso2,
   fgaus_models_lasso)


## Enet ------------------------------------------------------------------------
cat("Calculating prediction error curves for Enet.... \n\n")
load(paste0(dir, "VarsEnet_", name, ".rds"))

## coxph + frailty(id, distr = 'gaussian') with Enet selected variables 
fgaus_models_enet <- future_map2(vars.enet05, dfs, function(var.enet, df) {
  if (length(var.enet) == 0) var.enet <- 1
  form <- paste0("Surv(time, status)~", paste0(var.enet, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  m_enet_fgaus <- coxph(as.formula(form), 
                        data = df, y = TRUE, x = TRUE,
                        control = coxph.control(eps = 1e-11, iter.max = 500))
  return(m_enet_fgaus)
}, .options = future_options(seed = 123456L)) 


pecBoot632plus_enet <- vector("list", Nsim)
for (i in 1:Nsim) {
  cat("i = ", i, "\n\n")
  df <- dfs[[i]]
  model <- fgaus_models_enet[[i]]
  var <- vars.enet05[[i]]
  if(length(var) == 0) var <- 1
  form <- paste0("Surv(time, status)~", paste0(var, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  model[["formula"]] <- as.formula(form)
  pecBoot632plus_enet[[i]] <- try(my_pec(object=model,
                                      formula=Surv(time,status)~1,
                                      data=df,
                                      times = times, exact =  F,
                                      #exact=TRUE, maxtime = 2000,
                                      cens.model="marginal",
                                      splitMethod="Boot632plus", B = 100,
                                      verbose = F))
}

rm(coefs.enet05, vars.enet05, ci95s.enet05, vars.enet052,
   fgaus_models_enet)


## Ridge ------------------------------------------------------------------------
cat("Calculating prediction error curves for Ridge.... \n\n")
load(paste0(dir, "VarsRidge_", name, ".rds"))

## coxph + frailty(id, distr = 'gaussian') with Ridge selected variables 
fgaus_models_ridge <- future_map2(vars.ridge2, dfs, function(var.ridge, df) {
  if (length(var.ridge) == 0) var.ridge <- 1
  form <- paste0("Surv(time, status)~", paste0(var.ridge, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  m_ridge_fgaus <- coxph(as.formula(form), 
                         data = df, y = TRUE, x = TRUE,
                         control = coxph.control(eps = 1e-11, iter.max = 500))
  return(m_ridge_fgaus)
}, .options = future_options(seed = 123456L)) 


pecBoot632plus_ridge <- vector("list", Nsim)
for (i in 1:Nsim) {
  cat("i = ", i, "\n\n")
  df <- dfs[[i]]
  model <- fgaus_models_ridge[[i]]
  var <- vars.ridge2[[i]]
  if(length(var) == 0) var <- 1
  form <- paste0("Surv(time, status)~", paste0(var, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  model[["formula"]] <- as.formula(form)
  pecBoot632plus_ridge[[i]] <- try(my_pec(object=model,
                                       formula=Surv(time,status)~1,
                                       data=df,
                                       times = times, exact =  F,
                                       #exact=TRUE, maxtime = 2000,
                                       cens.model="marginal",
                                       splitMethod="Boot632plus", B = 100,
                                       verbose = F))
}

rm(coefs.ridge, ci95s.ridge, vars.ridge, vars.ridge2,
   fgaus_models_ridge)


## GroupLasso ------------------------------------------------------------------------
cat("Calculating prediction error curves for GroupLasso.... \n\n")
load(paste0(dir, "VarsGroupLasso_", name, ".rds"))

## coxph + frailty(id, distr = 'gaussian') with Ridge selected variables 
fgaus_models_grouplasso <- future_map2(vars.grouplasso, dfs, function(var.grouplasso, df) {
  if (length(var.grouplasso) == 0) var.grouplasso <- 1
  form <- paste0("Surv(time, status)~", paste0(var.grouplasso, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  m_grouplasso_fgaus <- coxph(as.formula(form), 
                              data = df, y = TRUE, x = TRUE, 
                              control = coxph.control(eps = 1e-11, iter.max = 500))
  return(m_grouplasso_fgaus)
}, .options = future_options(seed = 123456L)) 


pecBoot632plus_grouplasso <- vector("list", Nsim)
for (i in 1:Nsim) {
  cat("i = ", i, "\n\n")
  df <- dfs[[i]]
  model <- fgaus_models_grouplasso[[i]]
  var <- vars.grouplasso[[i]]
  if(length(var) == 0) var <- 1
  form <- paste0("Surv(time, status)~", paste0(var, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  model[["formula"]] <- as.formula(form)
  pecBoot632plus_grouplasso[[i]] <- try(my_pec(object=model,
                                            formula=Surv(time,status)~1,
                                            data=df,
                                            times = times, exact =  F,
                                            #exact=TRUE, maxtime = 2000,
                                            cens.model="marginal",
                                            splitMethod="Boot632plus", B = 100,
                                            verbose = F))
}

rm(coefs.grouplasso, vars.grouplasso,
   fgaus_models_grouplasso)


## CoxBoost ------------------------------------------------------------------------
cat("Calculating prediction error curves for CoxBoost.... \n\n")
load(paste0(dir, "VarsCoxBoost_", name, ".rds"))

## coxph + frailty(id, distr = 'gaussian') with Ridge selected variables 
fgaus_models_coxboost <- future_map2(vars.coxboost, dfs, function(var.coxboost, df) {
  if (length(var.coxboost) == 0) var.coxboost <- 1
  form <- paste0("Surv(time, status)~", paste0(var.coxboost, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  m_coxboost_fgaus <- coxph(as.formula(form), 
                            data = df, y = TRUE, x = TRUE,
                            control = coxph.control(eps = 1e-11, iter.max = 500))
  return(m_coxboost_fgaus)
}, .options = future_options(seed = 123456L)) 


pecBoot632plus_coxboost <- vector("list", Nsim)
for (i in 1:Nsim) {
  cat("i = ", i, "\n\n")
  df <- dfs[[i]]
  model <- fgaus_models_coxboost[[i]]
  var <- vars.coxboost[[i]]
  if(length(var) ==  0) var <- 1
  form <- paste0("Surv(time, status)~", paste0(var, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  model[["formula"]] <- as.formula(form)
  pecBoot632plus_coxboost[[i]] <- try(my_pec(object=model,
                                          formula=Surv(time,status)~1,
                                          data=df,
                                          times = times, exact = F,
                                          #exact=TRUE, maxtime = 2000,
                                          cens.model="marginal",
                                          splitMethod="Boot632plus", B = 100,
                                          verbose= F))
}

rm(coefs.coxboost, vars.coxboost,
   fgaus_models_coxboost)



