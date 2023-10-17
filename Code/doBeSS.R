## Function for Bess ---------------------------------------
doBeSS <- function(dfs, vars) {
  model.besss <- map(dfs, function(df) 
    bess(as.matrix(df[,1:vars]), as.matrix(df[,c("time", "status")]),  ## bess(x,y,...) x: input matrix of dim nxp; y: response if cox, columns named time and status
         method = "sequential",   
         s.list = 1:vars,
         normalize = TRUE, cox.max = 50,
         family = "cox", ic.type = "GIC"))  
  
  ## coefs
  coefs.besss <- map(model.besss, "bestmodel") %>% map(~coefficients(.x))
  ## exception when the output returns only xbest
  id <- which(sapply(coefs.besss, function(x) any(names(x) == "xbest")))
  if (length(id)>0) {
    for (i in 1:length(id)) {
      fit <- model.besss[id[[i]]][[1]]
      idx <- which(coef(fit$bestmodel) == coef(fit, sparse = T))
      names(coefs.besss[[id[[i]]]]) <- paste0("xbest", rownames(coef(fit))[idx])
    }
  }
  
  ## put proper names 
  coefs.besss <- map(coefs.besss, function(i) {
    names(i) <- sapply(strsplit(names(i), split = 'xbest', fixed = TRUE), function(a) (a[2]))
    return(i)
  })
  coefnames <- map(coefs.besss, names)
  
  ## put 0 to not selected variables
  p <- vars
  vars <- paste0("X", 1:p)
  coefs.bessNOT <- map(coefs.besss, function(i) {
    ind <- which(!(vars %in% names(i)))
    a <- rep(0, length(ind))
    names(a) <- paste0("X", ind)
    return(a)
  }) 
  coefs.besss <- map2(coefs.besss, coefs.bessNOT, function(i, j) c(i, j))
  coefs.besss <- map(coefs.besss, function(i) i[order(nchar(names(i)), names(i))]) %>% 
    reduce(rbind) %>% 
    as.data.frame(row.names = F)
  
  ## confidence intervals
  conf.besss <- map(model.besss, "bestmodel") %>%  map(~log(summary(.x)$conf.int[,3:4])) # take the logarithm!!
  conf.besss <- map2(conf.besss, coefnames, function(conf, name) {
    m <- data.frame(matrix(conf, ncol = 2, byrow = T), row.names = name)
  })
  ## lower and upper limits
  conf.bessLow <- map(conf.besss, ~.x[,1])
  conf.bessUp  <- map(conf.besss, ~.x[,2])
  ## put proper names 
  conf.bessLow <- map2(conf.bessLow, coefnames, function(i, name) {
    names(i) <- name
    return(i)
  })
  conf.bessUp <- map2(conf.bessUp, coefnames, function(i, name) {
    names(i) <- name
    return(i)
  })
  ## put 0 coefficients variables
  conf.bessLow <- map2(conf.bessLow, coefs.bessNOT, function(i, j) c(i, j))
  conf.bessLow <- map(conf.bessLow, function(i) i[order(nchar(names(i)), names(i))]) %>% 
    reduce(rbind) %>% 
    as.data.frame()
  conf.bessUp <- map2(conf.bessUp, coefs.bessNOT, function(i, j) c(i, j))
  conf.bessUp <- map(conf.bessUp, function(i) i[order(nchar(names(i)), names(i))]) %>% 
    reduce(rbind) %>% 
    as.data.frame()
  
  res <- list(coefs = coefs.besss, low = conf.bessLow, up = conf.bessUp)
  return(res)
}


