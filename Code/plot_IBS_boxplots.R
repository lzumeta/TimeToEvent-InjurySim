library(tidyverse)
library(purrr)
library(magrittr)
library(pec)


plot_IBS_boxplots <- function(pec_bess, pec_lasso, pec_enet, pec_ridge, 
                              pec_grouplasso, pec_coxboost, splitMethod) {
  
  times <- pec_bess[[1]]$time
  tmax <- max(times)
  
  ## check any convergence problem
  any_try_error <- function(pec) {
    idx <- lapply(pec, function(x) class(x) == "try-error") %>% unlist() %>% which
    if (length(idx)) pec <-  pec[-idx] 
    return(pec)
  }
  pec_bess <- any_try_error(pec_bess)
  pec_lasso <- any_try_error(pec_lasso)
  pec_enet <- any_try_error(pec_enet)
  pec_ridge <- any_try_error(pec_ridge)
  pec_grouplasso <- any_try_error(pec_grouplasso)
  pec_coxboost <- any_try_error(pec_coxboost)
  
  
  briers_ref   <- map(pec_bess, function(elem) elem[[splitMethod]][["Reference"]])
  briers_bess  <- map(pec_bess, function(elem) elem[[splitMethod]][[2]]) ## 2 = coxph.penal or coxph
  briers_lasso <- map(pec_lasso, function(elem) elem[[splitMethod]][[2]])
  briers_enet  <- map(pec_enet, function(elem) elem[[splitMethod]][[2]])
  briers_ridge <- map(pec_ridge, function(elem) elem[[splitMethod]][[2]])
  briers_grouplasso <- map(pec_grouplasso, function(elem) elem[[splitMethod]][[2]])
  briers_coxboost <- map(pec_coxboost, function(elem) elem[[splitMethod]][[2]])
  
  
  ## Extract integrated brier scores
  ibs_bess <- map(pec_bess, function(elem) ibs(elem)) %>% reduce(rbind)
  ibs_bess_ref <- ibs_bess[seq(1,nrow(ibs_bess), by = 2),]
  ibs_bess <- ibs_bess[seq(2,nrow(ibs_bess), by = 2),]
  
  ibs_lasso <- map(pec_lasso, function(elem) ibs(elem)) %>% reduce(rbind)
  ibs_lasso_ref <- ibs_lasso[seq(1,nrow(ibs_lasso), by = 2),]
  ibs_lasso <- ibs_lasso[seq(2,nrow(ibs_lasso), by = 2),]
  
  ibs_enet <- map(pec_enet, function(elem) ibs(elem)) %>% reduce(rbind)
  ibs_enet_ref <- ibs_enet[seq(1,nrow(ibs_enet), by = 2),]
  ibs_enet <- ibs_enet[seq(2,nrow(ibs_enet), by = 2),]
  
  ibs_ridge <- map(pec_ridge, function(elem) ibs(elem)) %>% reduce(rbind)
  ibs_ridge_ref <- ibs_ridge[seq(1,nrow(ibs_ridge), by = 2),]
  ibs_ridge <- ibs_ridge[seq(2,nrow(ibs_ridge), by = 2),]
  
  ibs_grouplasso <- map(pec_grouplasso, function(elem) ibs(elem)) %>% reduce(rbind)
  ibs_grouplasso_ref <- ibs_grouplasso[seq(1,nrow(ibs_grouplasso), by = 2),]
  ibs_grouplasso <- ibs_grouplasso[seq(2,nrow(ibs_grouplasso), by = 2),]
  
  ibs_coxboost <- map(pec_coxboost, function(elem) ibs(elem)) %>% reduce(rbind)
  ibs_coxboost_ref <- ibs_coxboost[seq(1,nrow(ibs_coxboost), by = 2),]
  ibs_coxboost <- ibs_coxboost[seq(2,nrow(ibs_coxboost), by = 2),]
  
  
  ## assign worst IBS value to models that couldn't be fitted
  imput_worst_ibs <- function(ibs) {
    if (any(is.na(ibs))) ibs[is.na(ibs)] <- 0.25
    return(ibs)
  }
  
  ibs_bess <- imput_worst_ibs(ibs_bess)
  ibs_lasso <- imput_worst_ibs(ibs_lasso)
  ibs_enet <- imput_worst_ibs(ibs_enet)
  ibs_ridge <- imput_worst_ibs(ibs_ridge)
  ibs_grouplasso <- imput_worst_ibs(ibs_grouplasso)
  ibs_coxboost <- imput_worst_ibs(ibs_coxboost)
  
  add_try_error <- function(ibs) {
    n <- nrow(ibs)
    if (n < 100) {
      idx <- 100 - n
      for (i in seq_along(idx)) {
        ibs <- rbind(ibs, data.frame(AppErr = 0.25, 
                                     BootCvErr = 0.25, NoInfErr = 0.25,
                                     Boot632plusErr = 0.25))
      }
    }
    return(ibs)
  }
  
  ibs_bess <-  add_try_error(ibs_bess)
  ibs_lasso <-  add_try_error(ibs_lasso)
  ibs_enet <-  add_try_error(ibs_enet)
  ibs_ridge <-  add_try_error(ibs_ridge)
  ibs_grouplasso <- add_try_error(ibs_grouplasso)
  ibs_coxboost <-  add_try_error(ibs_coxboost)
  ##
  
  ## Create data
  ibs_data <- data.frame(BeSS = ibs_bess[, splitMethod], Lasso = ibs_lasso[, splitMethod], Enet = ibs_enet[, splitMethod],
                         Ridge = ibs_ridge[,splitMethod], GroupLasso = ibs_grouplasso[, splitMethod],
                         CoxBoost = ibs_coxboost[, splitMethod])
  
  ## Plot
  p <- ibs_data %>%
    gather(key = Model, value = ibs) %>%
    mutate(Model = factor(Model, levels = c("BeSS", "Lasso", "Enet", "Ridge", "GroupLasso", "CoxBoost"))) %>% 
    ggplot(aes(x = Model, y = ibs, fill = Model)) + geom_boxplot() +
    ylab("") + xlab("") + ggtitle(paste0(splitMethod, " Integrated Brier Scores (IBS) in t = [0, ", tmax, ")")) +
    scale_fill_manual(values = c("BeSS" = "#1B9E77", "Lasso" = "#D95F02", "Enet" = "#7570B3", 
                                 "Ridge" = "#E7298A", "GroupLasso" = "#66A61E", "CoxBoost" = "#E6AB02"),
                      labels = c("BeSS", "Lasso", "Elastic Net", "Ridge", "Group Lasso", "Boosting")) +
    theme_bw() + theme(legend.position = "none")
  
  print(p)
  
}
