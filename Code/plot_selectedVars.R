library(magrittr)
library(tidyverse)
library(ggnewscale) ## new_scale_fill()

plot_selectedVars <- function(ci = FALSE, CaseStudy = FALSE) {
  
  ## How many non-zero variables are detected? 
  vars.bess.nonzero  <- apply(coefs.bess,  1, function(x) x != 0) %>% rowMeans()
  vars.lasso.nonzero <- apply(coefs.lasso, 1, function(x) x != 0) %>% rowMeans()
  vars.ridge.nonzero <- apply(coefs.ridge, 1, function(x) x != 0) %>% rowMeans()
  vars.enet.nonzero  <- apply(coefs.enet05, 1, function(x) x != 0) %>% rowMeans()
  
  if (ci) {
    vars.ridge.nonzero <- vars.ridge2 %>% unlist() %>% table()/Nsim %>% as.vector() 
    vars.ridge.nonzero <- vars.ridge.nonzero[paste0("X", 1:xvars)] ## order
  }
  
  vars.grouplasso.nonzero <- apply(coefs.grouplasso, 1, function(x) x != 0) %>% rowMeans()
  vars.coxboost.nonzero   <- apply(coefs.coxboost, 1, function(x) x != 0) %>% rowMeans()
  ##
  
  ## Create table
  mat_selected <- t(rbind(vars.bess.nonzero, vars.lasso.nonzero, vars.enet.nonzero, vars.ridge.nonzero,
                          vars.grouplasso.nonzero, vars.coxboost.nonzero))[,6:1]
  if(CaseStudy) {
    labels <- names(data)[9:ncol(data)]
    nrelev_vars <- ifelse(var_method == "BESS", 2, 
                          ifelse(var_method == "LASSO", 6, 4)
                          )
  } else {
    labels <- paste0("X", 1:xvars)
    nrelev_vars <- 5
  }
  
  upmargin <- ifelse(CaseStudy, 10, 5)
  
  ## New plot
  mat_selected2 <- as.data.frame(mat_selected)
  mat_selected2$type <- c(rep("relevant", nrelev_vars), rep("nonrelevant", xvars-nrelev_vars))
  mat_selected2$vars <- paste0("X", 1:xvars)
  
  mat_selected2 <- gather(mat_selected2, key = selection, value = value, -type, -vars) %>% 
    mutate(type = factor(type, levels = c("relevant", "nonrelevant")),
           selection = factor(selection,
                              levels = c("vars.coxboost.nonzero", "vars.grouplasso.nonzero", "vars.ridge.nonzero", 
                                         "vars.enet.nonzero", "vars.lasso.nonzero", "vars.bess.nonzero"), 
                              label = c("Boosting", "Group Lasso", "Ridge", "Elastic Net", "Lasso", "BeSS")),
           vars = factor(vars, levels = paste0("X", 1:xvars)))
  
  mat_selected2_rev <- mat_selected2[mat_selected2$type == "relevant",]
  mat_selected2_nonrev <- mat_selected2[mat_selected2$type == "nonrelevant",]
  
  ggplot(data = mat_selected2, aes(y = selection, x = vars)) +
    geom_tile(colour="white", size=0.7) + 
    geom_tile(dat = mat_selected2_rev, aes(fill= value), colour="white", size=0.7) +
    scale_fill_gradient(low = "white", high = "blue", name = "Frequency of selected\nrelevant variables",
                        breaks = seq(0, 1, length.out = 11), limits = c(0,1)) +
    new_scale_fill()+ 
    geom_tile(dat = mat_selected2_nonrev, aes(fill= value), colour="white", size=0.7) +
    scale_fill_gradient(low = "white", high = "red", name = "Frequency of selected\nnon-relevant variables",
                        breaks = seq(0, 1, length.out = 11), limits = c(0,1)) +
    xlab("") + ylab("") +
    scale_x_discrete(position = "top") +
    coord_cartesian(expand=FALSE)  +
    theme_bw() +
    theme(panel.grid=element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 27),
          legend.text = element_text(size = 19),
          legend.title = element_text(size = 23),
          legend.key.size = unit(3.5, "lines")) 
  
} 

