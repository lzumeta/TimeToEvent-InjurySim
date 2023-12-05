dir <- "Results/Scenario3/"
name <- "Nsim_100_Nobs_670_cens_0.75_xvars_50_verylargesize"
dir <- paste0(dir, name, "/")

load(paste0(dir, "Data_", name, ".rds"))
load(paste0(dir, "VarsBeSS_", name, ".rds")); load(paste0(dir, "VarsLasso_", name, ".rds"))
load(paste0(dir, "VarsEnet_", name, ".rds")); load(paste0(dir, "VarsRidge_", name, ".rds"))
load(paste0(dir, "VarsGroupLasso_", name, ".rds")); load(paste0(dir, "VarsCoxBoost_", name, ".rds"))

true_beta
which(true_beta!=0)
true_vars <- paste0("X", which(true_beta!=0))
tot_vars <- paste0("X", 1:xvars)
zero_vars <- tot_vars[!tot_vars %in% true_vars]

## Success rate
map(vars.bess, function(x) length(x) == length(true_vars) && x %in% true_vars) %>% unlist() %>% sum()
map(vars.lasso, function(x) length(x) == length(true_vars) && x %in% true_vars) %>% unlist() %>% sum()
map(vars.enet05, function(x) length(x) == length(true_vars) && x %in% true_vars) %>% unlist() %>% sum()
map(vars.ridge2, function(x) length(x) == length(true_vars) && x %in% true_vars) %>% unlist() %>% sum()
map(vars.grouplasso, function(x) length(x) == length(true_vars) && x %in% true_vars) %>% unlist() %>% sum()
map(vars.coxboost, function(x) length(x) == length(true_vars) && x %in% true_vars) %>% unlist() %>% sum()


## Average model size
sapply(vars.bess, length) %>% mean()
sapply(vars.lasso, length) %>% mean()
sapply(vars.enet05, length) %>% mean()
sapply(vars.ridge2, length) %>% mean()
sapply(vars.grouplasso, length) %>% mean()
sapply(vars.coxboost, length) %>% mean()

## Average number of falsely selected covariates 
map(vars.bess, function(x) sum(x %in% zero_vars)) %>% unlist() %>% mean() 
map(vars.lasso, function(x) sum(x %in% zero_vars)) %>% unlist() %>% mean() 
map(vars.enet05, function(x) sum(x %in% zero_vars)) %>% unlist() %>% mean() 
map(vars.ridge2, function(x) sum(x %in% zero_vars)) %>% unlist() %>% mean() 
map(vars.grouplasso, function(x) sum(x %in% zero_vars)) %>% unlist() %>% mean() 
map(vars.coxboost, function(x) sum(x %in% zero_vars)) %>% unlist() %>% mean() 

## Average number of correct 0 coefficients (26 in the case of scenario 1 bess)
map(vars.bess, function(x) sum(!zero_vars %in% x)) %>% unlist() %>% mean()
map(vars.lasso, function(x) sum(!zero_vars %in% x)) %>% unlist() %>% mean()
map(vars.enet05, function(x) sum(!zero_vars %in% x)) %>% unlist() %>% mean()
map(vars.ridge2, function(x) sum(!zero_vars %in% x)) %>% unlist() %>% mean()
map(vars.grouplasso, function(x) sum(!zero_vars %in% x)) %>% unlist() %>% mean()
map(vars.coxboost, function(x) sum(!zero_vars %in% x)) %>% unlist() %>% mean()

## Average number of incorrect 0 coefficients 
map(vars.bess, function(x) sum(!true_vars %in% x)) %>% unlist() %>% mean()
map(vars.lasso, function(x) sum(!true_vars %in% x)) %>% unlist() %>% mean()
map(vars.enet05, function(x) sum(!true_vars %in% x)) %>% unlist() %>% mean()
map(vars.ridge2, function(x) sum(!true_vars %in% x)) %>% unlist() %>% mean()
map(vars.grouplasso, function(x) sum(!true_vars %in% x)) %>% unlist() %>% mean()
map(vars.coxboost, function(x) sum(!true_vars %in% x)) %>% unlist() %>% mean()

## MSE -------------------
## Run models
## BeSS ------------------------------------------------------------------------
## coxph + frailty(id, distr = 'gaussian') with BeSS selected variables 
fgaus_models_bess <- future_map2(vars.bess, dfs, function(var.bess, df) {
  if (length(var.bess) == 0) var.bess <- 1
  form <- paste0("Surv(time, status)~", paste0(var.bess, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  m_bess_fgaus <- coxph(as.formula(form), 
                        data = df, y = TRUE, x = TRUE,
                        control = coxph.control(eps = 1e-11, iter.max = 500)
  )
  
  return(m_bess_fgaus)
}, .options = furrr_options(seed = 123456L)) 


## Lasso ------------------------------------------------------------------------
## coxph + frailty(id, distr = 'gaussian') with Lasso selected variables 
fgaus_models_lasso <- future_map2(vars.lasso, dfs, function(var.lasso, df) {
  if (length(var.lasso) == 0) var.lasso <- 1
  form <- paste0("Surv(time, status)~", paste0(var.lasso, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  m_lasso_fgaus <- coxph(as.formula(form), 
                         data = df, y = TRUE, x = TRUE,
                         control = coxph.control(eps = 1e-11, iter.max = 500))
  
  return(m_lasso_fgaus)
}, .options = furrr_options(seed = 123456L)) 


## Enet ------------------------------------------------------------------------
## coxph + frailty(id, distr = 'gaussian') with Enet selected variables 
fgaus_models_enet <- future_map2(vars.enet05, dfs, function(var.enet, df) {
  if (length(var.enet) == 0) var.enet <- 1
  form <- paste0("Surv(time, status)~", paste0(var.enet, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  m_enet_fgaus <- coxph(as.formula(form), 
                        data = df, y = TRUE, x = TRUE,
                        control = coxph.control(eps = 1e-11, iter.max = 500)
  )
  return(m_enet_fgaus)
}, .options = furrr_options(seed = 123456L)) 


## Ridge ------------------------------------------------------------------------
## coxph + frailty(id, distr = 'gaussian') with Ridge selected variables 
fgaus_models_ridge <- future_map2(vars.ridge2, dfs, function(var.ridge, df) {
  if (length(var.ridge) == 0) var.ridge <- 1
  form <- paste0("Surv(time, status)~", paste0(var.ridge, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  m_ridge_fgaus <- coxph(as.formula(form), 
                         data = df, y = TRUE, x = TRUE,
                         control = coxph.control(eps = 1e-11, iter.max = 500)
  )
  return(m_ridge_fgaus)
}, .options = furrr_options(seed = 123456L)) 


## GroupLasso ------------------------------------------------------------------------
## coxph + frailty(id, distr = 'gaussian') with Ridge selected variables 
fgaus_models_grouplasso <- future_map2(vars.grouplasso, dfs, function(var.grouplasso, df) {
  if (length(var.grouplasso) == 0) var.grouplasso <- 1
  form <- paste0("Surv(time, status)~", paste0(var.grouplasso, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  m_grouplasso_fgaus <- coxph(as.formula(form), 
                              data = df, y = TRUE, x = TRUE, 
                              control = coxph.control(eps = 1e-11, iter.max = 500)
  )
  return(m_grouplasso_fgaus)
}, .options = furrr_options(seed = 123456L)) 


## CoxBoost ------------------------------------------------------------------------
## coxph + frailty(id, distr = 'gaussian') with Ridge selected variables 
fgaus_models_coxboost <- future_map2(vars.coxboost, dfs, function(var.coxboost, df) {
  if (length(var.coxboost) == 0) var.coxboost <- 1
  form <- paste0("Surv(time, status)~", paste0(var.coxboost, collapse = "+"), "+ frailty(id, distribution = 'gaussian')")
  m_coxboost_fgaus <- coxph(as.formula(form), 
                            data = df, y = TRUE, x = TRUE,
                            control = coxph.control(eps = 1e-11, iter.max = 500)
  )
  return(m_coxboost_fgaus)
}, .options = furrr_options(seed = 123456L)) 


## MEASURES -------------------------
# bess --------------------------------------------------------------------
all_coefs <- lapply(1:100, function(i) as.vector(rep(0,xvars)))
all_coefs <- map(all_coefs, function(cof) {names(cof) <- paste0("X", 1:xvars); return(cof)})
coefs_selected <- map(fgaus_models_bess, coef)
coefs2 <- map2(all_coefs, coefs_selected, function(x,y) {
  x <- x[!(names(x) %in% names(y))]
  res <- c(x,y)
  res <- res[order(names(res))]
  return(res)
})

mse1 <- map(coefs2, function(hatbeta) (hatbeta-true_beta)^2)

## make a function
all_coefs <- lapply(1:100, function(i) as.vector(rep(0,xvars)))
all_coefs <- map(all_coefs, function(cof) {names(cof) <- paste0("X", 1:xvars); return(cof)})

coefs_estimated <- function(model, all_coefs) {
  coefs_est <- coef(model)
  coefs_est2 <- all_coefs[!(names(all_coefs) %in% names(coefs_est))]
  res <- c(coefs_est2, coefs_est)
  res <- res[order(names(res))]
  return(res)
} 
mse <- function(hatbeta) sum((hatbeta-true_beta)^2)
bias <- function(hatbeta) sum(hatbeta - true_beta)
empSE <- function(hatbeta) sum((hatbeta - mean(hatbeta))^2)

coefs_est_bess <- map2(fgaus_models_bess, all_coefs, function(x,y) coefs_estimated(x,y))
mse_bess <- map(coefs_est_bess, mse)  
bias_bess <- map(coefs_est_bess, bias)
empSE_bess <- map(coefs_est_bess, empSE)
# lasso 
coefs_est_lasso <- map2(fgaus_models_lasso, all_coefs, function(x,y) coefs_estimated(x,y))
mse_lasso <- map(coefs_est_lasso, mse)  
bias_lasso <- map(coefs_est_lasso, bias)
empSE_lasso <- map(coefs_est_lasso, empSE)
## enet
coefs_est_enet <- map2(fgaus_models_enet, all_coefs, function(x,y) coefs_estimated(x,y))
mse_enet <- map(coefs_est_enet, mse)
bias_enet <- map(coefs_est_enet, bias)
empSE_enet <- map(coefs_est_enet, empSE)
## ridge
coefs_est_ridge <- map2(fgaus_models_ridge, all_coefs, function(x,y) coefs_estimated(x,y))
mse_ridge <- map(coefs_est_ridge, mse)  
bias_ridge <- map(coefs_est_ridge, bias)
empSE_ridge <- map(coefs_est_ridge, empSE)
## grouplasso
coefs_est_grouplasso <- map2(fgaus_models_grouplasso, all_coefs, function(x,y) coefs_estimated(x,y))
mse_grouplasso <- map(coefs_est_grouplasso, mse)  
bias_grouplasso <- map(coefs_est_grouplasso, bias)
empSE_grouplasso <- map(coefs_est_grouplasso, empSE)
## coxboost
coefs_est_coxboost <- map2(fgaus_models_coxboost, all_coefs, function(x,y) coefs_estimated(x,y))
mse_coxboost <- map(coefs_est_coxboost, mse)  
bias_coxboost <- map(coefs_est_coxboost, bias)
empSE_coxboost <- map(coefs_est_coxboost, empSE)

## MSE
mse_bess %>% reduce(rbind) %>% mean() %>% round(2)
mse_lasso %>% reduce(rbind) %>% mean() %>% round(2)
mse_enet %>% reduce(rbind) %>% mean() %>% round(2)
mse_ridge %>% reduce(rbind) %>% mean() %>% round(2)
mse_grouplasso %>% reduce(rbind) %>% mean() %>% round(2)
mse_coxboost %>% reduce(rbind) %>% mean() %>% round(2)

## BIAS
bias_bess %>% reduce(rbind) %>% mean() %>% round(2)
bias_lasso %>% reduce(rbind) %>% mean() %>% round(2)
bias_enet %>% reduce(rbind) %>% mean() %>% round(2)
bias_ridge %>% reduce(rbind) %>% mean() %>% round(2)
bias_grouplasso %>% reduce(rbind) %>% mean() %>% round(2)
bias_coxboost %>% reduce(rbind) %>% mean() %>% round(2)

## empSE
empSE_bess %>% reduce(rbind) %>% (\(x) sum(x)/(100-1))() %>% sqrt() %>% round(2)
empSE_lasso %>% reduce(rbind) %>% (\(x) sum(x)/(100-1))() %>% sqrt() %>% round(2)
empSE_enet %>% reduce(rbind) %>% (\(x) sum(x)/(100-1))() %>% sqrt() %>%round(2)
empSE_ridge %>% reduce(rbind) %>% (\(x) sum(x)/(100-1))() %>% sqrt() %>%round(2)
empSE_grouplasso %>% reduce(rbind) %>% (\(x) sum(x)/(100-1))() %>% sqrt() %>% round(2)
empSE_coxboost %>% reduce(rbind) %>% (\(x) sum(x)/(100-1))() %>% sqrt() %>% round(2)


## IBS ------------------------
## Functions to assign the worst value, when errors if any
## if there are errors

## Numero de fallos al calcular errores de prediccioin
load(paste0(dir, "pecBoot632plus_", name, ".rds"))
lapply(pecBoot632plus_bess, function(x) is.na(ibs(x)[2, "Boot632plusErr"])) %>% unlist() %>% sum()
lapply(pecBoot632plus_lasso, function(x) is.na(ibs(x)[2, "Boot632plusErr"])) %>% unlist() %>% sum()
lapply(pecBoot632plus_enet, function(x) is.na(ibs(x)[2, "Boot632plusErr"])) %>% unlist() %>% sum()
lapply(pecBoot632plus_ridge, function(x) is.na(ibs(x)[2, "Boot632plusErr"])) %>% unlist() %>% sum()
lapply(pecBoot632plus_grouplasso, function(x) is.na(ibs(x)[2, "Boot632plusErr"])) %>% unlist() %>% sum()
lapply(pecBoot632plus_coxboost, function(x) is.na(ibs(x)[2, "Boot632plusErr"])) %>% unlist() %>% sum()

any_try_error <- function(pec) {
  idx <- lapply(pec, function(x) class(x) == "try-error") %>% unlist() %>% which
  if (length(idx)) pec <-  pec[-idx] 
  return(pec)
}
imput_worst_ibs <- function(ibs) {
  if (any(is.na(ibs))) ibs[is.na(ibs)] <- 0.25
  return(ibs)
}
pecBoot632plus_bess <- any_try_error(pecBoot632plus_bess)
pecBoot632plus_lasso <- any_try_error(pecBoot632plus_lasso)
pecBoot632plus_enet <- any_try_error(pecBoot632plus_enet)
pecBoot632plus_ridge <- any_try_error(pecBoot632plus_ridge)
pecBoot632plus_grouplasso <- any_try_error(pecBoot632plus_grouplasso)
pecBoot632plus_coxboost <- any_try_error(pecBoot632plus_coxboost)

## Median ibs [0,1000]
ibs_bess_1000 <- map(pecBoot632plus_bess, function(x) ibs(x, times = 1000, what = "Boot632plusErr")[2,]) %>% unlist()  
ibs_lasso_1000 <- map(pecBoot632plus_lasso, function(x) ibs(x, times = 1000, what = "Boot632plusErr")[2,]) %>% unlist() 
ibs_enet_1000 <- map(pecBoot632plus_enet, function(x) ibs(x, times = 1000, what = "Boot632plusErr")[2,]) %>% unlist() 
ibs_ridge_1000 <- map(pecBoot632plus_ridge, function(x) ibs(x, times = 1000, what = "Boot632plusErr")[2,]) %>% unlist() 
ibs_grouplasso_1000 <- map(pecBoot632plus_grouplasso, function(x) ibs(x, times = 1000, what = "Boot632plusErr")[2,]) %>% unlist()
ibs_coxboost_1000 <- map(pecBoot632plus_coxboost, function(x) ibs(x, times = 1000, what = "Boot632plusErr")[2,]) %>% unlist() 

add_try_error <- function(ibs) {
  n <- length(ibs)
  if (n < 100) {
    idx <- 100 - n
    ibs <- c(ibs, rep(0.25, idx))
  }
  return(ibs)
}

ibs_bess_1000 <-  add_try_error(ibs_bess_1000)
ibs_lasso_1000 <-  add_try_error(ibs_lasso_1000)
ibs_enet_1000 <-  add_try_error(ibs_enet_1000)
ibs_ridge_1000 <-  add_try_error(ibs_ridge_1000)
ibs_grouplasso_1000 <- add_try_error(ibs_grouplasso_1000)
ibs_coxboost_1000 <-  add_try_error(ibs_coxboost_1000)

ibs_bess_1000 <- imput_worst_ibs(ibs_bess_1000)
ibs_lasso_1000 <- imput_worst_ibs(ibs_lasso_1000)
ibs_enet_1000 <- imput_worst_ibs(ibs_enet_1000)
ibs_ridge_1000 <- imput_worst_ibs(ibs_ridge_1000)
ibs_grouplasso_1000 <- imput_worst_ibs(ibs_grouplasso_1000)
ibs_coxboost_1000 <- imput_worst_ibs(ibs_coxboost_1000)

round(median(ibs_bess_1000), 3)
round(median(ibs_lasso_1000), 3)
round(median(ibs_enet_1000), 3)
round(median(ibs_ridge_1000), 3)
round(median(ibs_grouplasso_1000), 3)
round(median(ibs_coxboost_1000), 3)

## Median ibs [0, 3500]
ibs_bess_3500 <- map(pecBoot632plus_bess, function(x) ibs(x, times = 3500, what = "Boot632plusErr")[2,]) %>% unlist() 
ibs_lasso_3500 <- map(pecBoot632plus_lasso, function(x) ibs(x, times = 3500, what = "Boot632plusErr")[2,]) %>% unlist() 
ibs_enet_3500 <- map(pecBoot632plus_enet, function(x) ibs(x, times = 3500, what = "Boot632plusErr")[2,]) %>% unlist() 
ibs_ridge_3500 <- map(pecBoot632plus_ridge, function(x) ibs(x, times = 3500, what = "Boot632plusErr")[2,]) %>% unlist() 
ibs_grouplasso_3500 <- map(pecBoot632plus_grouplasso, function(x) ibs(x, times = 3500, what = "Boot632plusErr")[2,]) %>% unlist() 
ibs_coxboost_3500 <- map(pecBoot632plus_coxboost, function(x) ibs(x, times = 3500, what = "Boot632plusErr")[2,]) %>% unlist()

ibs_bess_3500 <-  add_try_error(ibs_bess_3500)
ibs_lasso_3500 <-  add_try_error(ibs_lasso_3500)
ibs_enet_3500 <-  add_try_error(ibs_enet_3500)
ibs_ridge_3500 <-  add_try_error(ibs_ridge_3500)
ibs_grouplasso_3500 <- add_try_error(ibs_grouplasso_3500)
ibs_coxboost_3500 <-  add_try_error(ibs_coxboost_3500)

ibs_bess_3500 <-  imput_worst_ibs(ibs_bess_3500)
ibs_lasso_3500 <-  imput_worst_ibs(ibs_lasso_3500)
ibs_enet_3500 <-  imput_worst_ibs(ibs_enet_3500)
ibs_ridge_3500 <-  imput_worst_ibs(ibs_ridge_3500)
ibs_grouplasso_3500 <- imput_worst_ibs(ibs_grouplasso_3500)
ibs_coxboost_3500 <-  imput_worst_ibs(ibs_coxboost_3500)

round(median(ibs_bess_3500), 3)
round(median(ibs_lasso_3500), 3)
round(median(ibs_enet_3500), 3)
round(median(ibs_ridge_3500), 3)
round(median(ibs_grouplasso_3500), 3)
round(median(ibs_coxboost_3500), 3)

rm(list=ls())
##





