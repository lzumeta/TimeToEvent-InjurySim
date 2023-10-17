## Function doRegularization ----------------------------------------------

## function for elastic net -----------------------------------------------
set.alpha <- c(0, 0.2, 0.4, 0.6, 0.7, 0.8, 0.9, 1)
res.lambda <- rep(NA, length(set.alpha))
res.min.pld <- rep(NA, length(set.alpha))
res.coef.opt <- list(NA, length(set.alpha))

get.enet.coef <- function(X, y, set.alpha = c(0.2, 0.4, 0.6, 0.7, 0.8, 0.9), 
                          res.lambda, res.min.pld, res.coef.opt, foldid) {
  
  for (i in 1:length(set.alpha)) {
    ## cross validation to find the optimal lambda, for each specific alpha
    enet.cv = cv.glmnet(X, y,
                        alpha = set.alpha[i],
                        family="cox", nfolds = 10, foldid = foldid,
                        standardize=T, parallel=T)
    ## optimum lambda and minimum partial likelihood (pld)
    res.lambda[i] = enet.cv$lambda.min
    res.min.pld[i] = min(enet.cv$cvm)
    ## Number of coefficients different from 0 of the model with optimal lambda
    res.coef.opt[[i]] = as.matrix(coef(enet.cv, s = enet.cv$lambda.min))
  }
  
  ibest <- which.min(res.min.pld)
  enet.coef <- t(as.matrix(res.coef.opt[[ibest]]))
  lambda_min <- res.lambda[[ibest]]
  
  return(list(enet.coef = enet.coef, lambda = lambda_min))
}
##


## Function for Lasso/ridge/enet ----------------------------
doRegularization <- function(dfs, alpha = NULL, enet = FALSE, vars) {
  if (!enet) {
    models <- future_map(dfs, function(df) {
      X <- as.matrix(df[,1:vars])
      y <- Surv(df[,"time"], df[,"status"])
      
      set.seed(16)
      flds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE) ## to always obtain the same results
      foldids <- rep(1,length(y))
      foldids[flds$Fold02] = 2; foldids[flds$Fold03] = 3; foldids[flds$Fold04] = 4; foldids[flds$Fold05] = 5; foldids[flds$Fold06] = 6; 
      foldids[flds$Fold07] = 7; foldids[flds$Fold08] = 8; foldids[flds$Fold09] = 9; foldids[flds$Fol10] = 10
      
      cv.glmnet(X, y, family="cox", alpha = alpha, 
                nfolds = 10, foldid = foldids) # standardize = TRUE by default
    })
    
    lambda_mins <- future_map(models, function(model) model[["lambda.min"]])
    coefs <- future_map(models, function(m) coefficients(m, s = m[["lambda.min"]])) %>% 
      map(~ t(as.matrix(.x))) %>% 
      purrr::reduce(rbind) %>% 
      as.data.frame(row.names = F)
    
  } else {
    ## apply function get.enet.coef for each data in dfs
    coefs <- future_map(dfs, function(df) {
      X <- as.matrix(df[,1:vars])
      y <- Surv(df[,"time"], df[,"status"])
      
      set.seed(16)
      flds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE) ## to always obtain the same results
      foldids <- rep(1,length(y))
      foldids[flds$Fold02] = 2; foldids[flds$Fold03] = 3; foldids[flds$Fold04] = 4; foldids[flds$Fold05] = 5; foldids[flds$Fold06] = 6; 
      foldids[flds$Fold07] = 7; foldids[flds$Fold08] = 8; foldids[flds$Fold09] = 9; foldids[flds$Fol10] = 10
      
      get.enet.coef(X, y, set.alpha, res.lambda, res.min.pld, res.coef.opt, foldid = foldids)
    })
    
    lambda_mins <- future_map(coefs, function(x) x[[2]])
    
    coefs <- future_map(coefs, function(x) x[[1]])
    coefs <- future_map(coefs, ~ as.matrix(.)) %>%
      reduce(rbind) %>% 
      as.data.frame(row.names = F)
  }
  
  #return(coefs)
  return(list(coefs = coefs, lambdas = lambda_mins))
}
##


## Function for calculating confidence intervals ------------------
library(progress)
pb <- progress_estimated(length(dfs))
bootstrap_regularization <- function(df, B = 100, alph, enet = FALSE, vars, lambda) {
  #pb$tick()$print()
  X <- as.matrix(df[, 1:vars])
  Y <- Surv(df[,"time"], df[,"status"])
  set.alpha <- c(0, 0.2, 0.4, 0.6, 0.7, 0.8, 0.9, 1)
  
  coeff_vector <- NULL
  coeff_vector2 <- lapply(1:B, function(i) {
    s <- sample(x = 1: nrow(df), size = nrow(df), replace = TRUE)
    Xs <- X[s,]
    Ys <- Y[s,]
    
    if (!enet) {
      glmnet_model <- glmnet(x = Xs, y = Ys, data = df, family = "cox", alpha = alph, lambda = lambda)  
      coeff <- as.numeric(coefficients(glmnet_model, s = glmnet_model$lambda.min))
    } else {
      res.min.pld <- NULL
      res.coef.opt <- NULL
      for (i in 1:length(set.alpha)) {
        enet.cv = glmnet(x = Xs, y = Ys,
                         alpha = set.alpha[i], lambda = lambda,
                         family="cox", standardize=T, parallel=T)
        ## optimum lambda and minimum partial likelihood (pld)
        #res.lambda[i] = enet.cv$lambda.min
        res.min.pld[i] = min(enet.cv$cvm)
        ## Number of coefficients different from 0, for the optimum lambda model
        res.coef.opt[[i]] = as.matrix(coef(enet.cv, s = enet.cv$lambda.min))
      }
      ibest <- which.min(res.min.pld)
      coeff <- t(as.matrix(res.coef.opt[[ibest]]))
    }
    
    coeff_vector <- cbind(coeff_vector, coeff)
    return(coeff_vector)
  })
  
  ci <- apply(as.data.frame(coeff_vector2), 1, function(elem) {  ## each row is a coefficient associated to a variable_row
    c(quantile(elem, 0.025), quantile(elem, 0.975))
  })
  
  return(ci = ci)
}


