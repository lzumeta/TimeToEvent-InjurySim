mean_median_curves <- function(brier_dfs, times) {
  
  brier_dfs <- brier_dfs %>% reduce(rbind)
  
  a <- NULL  ##  empty dataframe
  a$brier_mean   <- colMeans(brier_dfs, na.rm = T)
  a$brier_median <- apply(brier_dfs, 2, median, na.rm = T)
  a$brier_2.5 <- apply(brier_dfs, 2, function(x) quantile(x, probs = 0.025, na.rm = T))
  a$brier_97.5 <- apply(brier_dfs, 2, function(x) quantile(x, probs = 0.975, na.rm = T))
  a <- as.data.frame(a)
  a$Brier_time <- times
  
  return(a)
}

## Plot settings

theme_set(theme_bw() +
            theme(axis.text = element_text(size=12),
                  axis.title = element_text(size=14),
                  legend.text = element_text(size=14),
                  legend.title = element_blank(),
                  title = element_text(size=15)))

plot_brier_curves <- function(b, title, ylim_up = 1) {
  ggplot() + 
    #geom_step(aes(x = Brier_time, Brier_bsc, group = id), col = "grey", size = 0.1) + xlim(0, Tmax) + ylim(0,1) +
    geom_ribbon(aes(x = Brier_time, ymin = brier_2.5, ymax = brier_97.5), data =  b, fill = "grey90") +
    geom_step(aes(x = Brier_time, brier_mean, color = "Mean"), data  = b, size = 0.4) +
    geom_step(aes(x = Brier_time, brier_median,  col = "Median"), data = b, size = 0.4) + 
    geom_abline(intercept  = 0.33, slope = 0,  col = "gray23") +
    ylim(c(0,ylim_up)) +
    xlab("Time") + ylab("Prediction error") + ggtitle(title) 
}

plot_all_briers <- function(b0,b1,b2,b3,b4,b5,b6, stat = "median", ylim_up = 0.4, splitMethod, times) {
  st <- ifelse(stat=="median", "brier_median", "brier_mean")
  ggplot() +
    geom_step(aes(x = Brier_time, get(st), color = "BeSS"), data  = b1) +
    geom_step(aes(x = Brier_time, get(st), color = "Lasso"), data  = b2) + 
    geom_step(aes(x = Brier_time, get(st), color = "Elastic net"), data  = b3) +
    geom_step(aes(x = Brier_time, get(st), color = "Ridge"), data  = b4) + 
    geom_step(aes(x = Brier_time, get(st), color = "GroupLasso"), data  = b5) +
    geom_step(aes(x = Brier_time, get(st), color = "CoxBoost"), data  = b6) +
    #geom_step(aes(x = Brier_time, get(st), col = "Reference"), data = b0, size = 0.4) +
    geom_abline(intercept  = 0.33, slope = 0,  col = "gray23") +
    scale_color_manual(values = c("BeSS" = "#1B9E77", "Lasso" = "#D95F02", "Elastic net" = "#7570B3", 
                                  "Ridge" = "#E7298A", "GroupLasso" = "#66A61E", "CoxBoost" = "#E6AB02", 
                                  "Reference" = "black")) +
    xlim(0, times[[length(times)]]) + ylim(0, ylim_up) +
    xlab("Time") + ylab("Prediction error") + 
    ggtitle(paste0(splitMethod, " Brier Scores ", stat,  "\nComparison of all methods")) 
}


## Main plot function
plot_Brier_Curves <- function(pec_bess, pec_lasso, pec_enet, pec_ridge, pec_grouplasso, pec_coxboost, splitMethod) {
  
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
  
  ## extract resutls Boot632plus -----
  times <- pec_bess[[1]]$time
  briers_ref   <- map(pec_bess, function(elem) elem[[splitMethod]][["Reference"]])
  briers_bess  <- map(pec_bess, function(elem) elem[[splitMethod]][[2]]) ## 2 = coxph.penal or coxph
  briers_lasso <- map(pec_lasso, function(elem) elem[[splitMethod]][[2]])
  briers_enet  <- map(pec_enet, function(elem) elem[[splitMethod]][[2]])
  briers_ridge <- map(pec_ridge, function(elem) elem[[splitMethod]][[2]])
  briers_grouplasso <- map(pec_grouplasso, function(elem) elem[[splitMethod]][[2]])
  briers_coxboost <- map(pec_coxboost, function(elem) elem[[splitMethod]][[2]])
  
  add_try_error <- function(briers) {
    n <- length(briers)
    if (n < 100) {
      idx <- 100 - n
      for (i in seq_along(idx)) {
        briers[[n+i]] <- rep(0.25, times = length(times))
      }
    }
    return(briers)
  }
  
  briers_bess <-  add_try_error(briers_bess)
  briers_lasso <-  add_try_error(briers_lasso)
  briers_enet <-  add_try_error(briers_enet)
  briers_ridge <-  add_try_error(briers_ridge)
  briers_grouplasso <- add_try_error(briers_grouplasso)
  briers_coxboost <-  add_try_error(briers_coxboost)
  
  ## Calculate median, mean curves
  #b0 <- mean_median_curves(briers_reference); a0 <- b0[[1]]; b0 <- b0[[2]] 
  b1 <- mean_median_curves(briers_bess, times);      
  b2 <- mean_median_curves(briers_lasso, times);    
  b3 <- mean_median_curves(briers_enet, times);      
  b4 <- mean_median_curves(briers_ridge, times);  
  b5 <- mean_median_curves(briers_grouplasso, times);
  b6 <- mean_median_curves(briers_coxboost, times);
  #b5 <- mean_median_curves(briers_rpart, times);    
  ylim_up <- lapply(list(briers_bess, briers_lasso, briers_enet, briers_ridge, 
                         briers_grouplasso, briers_coxboost),
                    function(l) reduce(l, rbind)) %>% 
    reduce(rbind) %>% 
    max
  
  p1 <- plot_brier_curves(b1, paste0(splitMethod, " Brier Score of\nBeSS method"), ylim_up = ylim_up)
  p2 <- plot_brier_curves(b2, paste0(splitMethod, " Brier Score of\nLasso method"), ylim_up = ylim_up)
  p3 <- plot_brier_curves(b3, paste0(splitMethod, " Brier Score of\nElastic net method"), ylim_up = ylim_up)
  p4 <- plot_brier_curves(b4, paste0(splitMethod, " Brier Score of\nRidge method"), ylim_up = ylim_up)  
  p5 <- plot_brier_curves(b5, paste0(splitMethod, " Brier Score of\nGroupLasso method"), ylim_up = ylim_up)
  p6 <- plot_brier_curves(b6, paste0(splitMethod, " Brier Score of\nCoxboost method"), ylim_up = ylim_up)
  
  grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol =2)
  p6 <- plot_all_briers(b0=NULL,b1,b2,b3,b4,b5,b6, stat="mean", ylim_up, splitMethod = splitMethod, times = times)
  print(p6)
  p6 <- plot_all_briers(b0=NULL,b1,b2,b3,b4,b5,b6, stat="median", ylim_up, splitMethod = splitMethod, times = times)
  print(p6)
}




