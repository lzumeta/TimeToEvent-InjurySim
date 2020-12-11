dir <- "Codes/Simulations/Results/"

for (i in 1:8){
  cat("i = ", i, "\n\n")
  
  j <- i %% 4
  if (j == 0) j <- 4
  scenario <- ifelse(i<=4, "Scenario2/", "Scenario3/")
  direct <- paste0(dir,scenario)
  setting <- dir(direct)[[j]]
  
  load(paste0(direct, setting, "/Data_", setting, ".rds"))
  load(paste0(direct, setting, "/pecBoot632plus_", setting, ".rds"))
  
  name <- ifelse(j==1, "3 teams", 
                 ifelse(j==2, "6 teams", ifelse(j==3, "1 team", "10 teams")))
  
  pec_bess <- pecBoot632plus_bess
  pec_lasso <- pecBoot632plus_lasso
  pec_enet <- pecBoot632plus_enet
  pec_ridge <- pecBoot632plus_ridge
  pec_grouplasso <- pecBoot632plus_grouplasso
  pec_coxboost <- pecBoot632plus_coxboost
  splitMethod <- "Boot632plusErr"
  
  
  ## if there are errors
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
  
  # briers_rpart <- map(pec_rpart, function(elem) elem[[splitMethod]][[2]])
  # idx <- map(briers_rpart, function(x) any(is.na(x))) %>% unlist() %>% which()
  # briers_rpart[idx] <- map(pec_rpart[idx], function(elem) elem[["AppErr"]][[2]])
  
  
  
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
  
  # ibs_rpart <- map(pec_rpart, function(elem) ibs(elem)) %>% reduce(rbind)
  # ibs_rpart_ref <- ibs_rpart[seq(1,nrow(ibs_rpart), by = 2),]
  # ibs_rpart <- ibs_rpart[seq(2,nrow(ibs_rpart), by = 2),]
  
  ibs_grouplasso <- map(pec_grouplasso, function(elem) ibs(elem)) %>% reduce(rbind)
  ibs_grouplasso_ref <- ibs_grouplasso[seq(1,nrow(ibs_grouplasso), by = 2),]
  ibs_grouplasso <- ibs_grouplasso[seq(2,nrow(ibs_grouplasso), by = 2),]
  
  ibs_coxboost <- map(pec_coxboost, function(elem) ibs(elem)) %>% reduce(rbind)
  ibs_coxboost_ref <- ibs_coxboost[seq(1,nrow(ibs_coxboost), by = 2),]
  ibs_coxboost <- ibs_coxboost[seq(2,nrow(ibs_coxboost), by = 2),]
  
  
  ## if there are NAs assign random guess
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
  
  cat(scenario, name, "\n")
  
  ibs_bess[,"Boot632plusErr"] %>% summary() %>% round(3) %>% print()
  ibs_lasso[,"Boot632plusErr"] %>% summary() %>% round(3) %>% print()
  ibs_enet[,"Boot632plusErr"] %>% summary() %>% round(3) %>% print()
  ibs_ridge[,"Boot632plusErr"] %>% summary() %>% round(3) %>% print()
  ibs_grouplasso[,"Boot632plusErr"] %>% summary() %>% round(3) %>% print()
  ibs_coxboost[,"Boot632plusErr"] %>% summary() %>% round(3) %>% print()
}

## Error reduction
## Decrease Formula: range of the IBS (10 teams)/range of the IBS (1 team)

## Scenario 2 BeSS (1 team vs 10 teams)
(0.137-0.095)/(0.198-0.067)
100-round((0.137-0.095)/(0.198-0.067)*100,4)

## Scenario 2 Group Lasso (1 team vs 10 teams)
(0.148-0.099)/(0.250-0.074)
100-round((0.148-0.099)/(0.250-0.074)*100,4)

## Scenario 3 BeSS (1 team vs 10 teams)
(0.125-0.089)/(0.171-0.059)
100-round((0.125-0.089)/(0.171-0.059)*100,4)

## Scenario 3 Group Lasso (1 team vs 10 teams)
(0.134-0.094)/(0.255-0.072)
100-round((0.134-0.094)/(0.255-0.072)*100,4)

