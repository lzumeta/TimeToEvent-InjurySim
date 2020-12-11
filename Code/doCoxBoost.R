## Function for CoxBoost ---------------------------------------
doCoxBoost <- function(dfs, vars) {
  models <- map(dfs, function(df) {
    X <- as.matrix(df[,1:vars])
    y <- Surv(df[,"time"], df[,"status"])
    
    set.seed(16)
    flds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE) ## to always obtain the same results
    
    ## 10-fold cross-validation
    cv.CoxBoost(time=df[["time"]], status=df[["status"]], x=X, maxstepno=500, type="verweij", penalty=100,
                K=10, folds = flds)$optimal.step
  })
  
  coefs <- map2(dfs, models, function(df, step) {
    X <- as.matrix(df[,1:vars])
    CoxBoost(time=df[["time"]], status=df[["status"]], x=X, stepno=step, penalty=100) %>% 
      coef()
  }) %>% 
    map(~t(as.matrix(.x))) %>% 
    purrr::reduce(rbind) %>% 
    as.data.frame(row.names=F)
  
  return(list(coefs = coefs, step_nos = models))
}
