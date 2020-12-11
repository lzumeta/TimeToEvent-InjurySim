## Function for GroupLasso ---------------------------------------
doGroupLasso <- function(dfs, vars, group) {
  models <- future_map(dfs, function(df) {
    X <- as.matrix(df[,1:vars])
    y <- Surv(df[,"time"], df[,"status"])
    
    set.seed(16)
    flds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE) ## to always obtain the same results
    foldids = rep(1,length(y))
    foldids[flds$Fold02] = 2; foldids[flds$Fold03] = 3; foldids[flds$Fold04] = 4; foldids[flds$Fold05] = 5; foldids[flds$Fold06] = 6; 
    foldids[flds$Fold07] = 7; foldids[flds$Fold08] = 8; foldids[flds$Fold09] = 9; foldids[flds$Fol10] = 10
    
    cv.grpsurv(X, y, group = group, penalty = "grLasso", alpha = 1, 
               nfolds = 10, fold = foldids)
    
  }, .options = future_options(seed = 123456L))
  
  lambda_mins <- future_map(models, function(model) model[["lambda.min"]])
  
  coefs <- future_map(models, function(m) coefficients(m, s = m[["lambda.min"]])) %>% 
    map(~t(as.matrix(.x))) %>% 
    purrr::reduce(rbind) %>% 
    as.data.frame(row.names=F)
  
  return(list(coefs = coefs, lambdas = lambda_mins))
}