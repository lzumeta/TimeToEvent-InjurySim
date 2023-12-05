library(magrittr)
library(tidyverse)
library(ggnewscale) ## new_scale_fill()
library(RColorBrewer)

plot_selectedVars <- function(ci = FALSE, CaseStudy = FALSE, title = NULL) {
  
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
           vars = factor(vars, levels = paste0("X", 1:xvars)),
           valueFac = cut(value, breaks = c(-1, 0.1, 0.3, 0.5, 0.7, 0.85, 1),
                          labels = c("[0,0.1]", "(0.1,0.3]", "(0.3-0.5]", "(0.5-0.7]", "(0.7-0.85]", "(0.85-1]")))
  
  mat_selected2_rev    <- mat_selected2[mat_selected2$type == "relevant",]
  mat_selected2_nonrev <- mat_selected2[mat_selected2$type == "nonrelevant",]
  
  ggplot(data = mat_selected2, aes(y = selection, x = vars)) +
    geom_tile(colour="white", size=0.7) + 
    geom_tile(dat = mat_selected2_rev, aes(fill= valueFac), colour="white", size=0.7) +
    scale_fill_brewer(type = "seq", palette = "Greens", drop = FALSE,
                      name = "Proportion of correctly selected variables  ") +
    guides(
      fill = guide_legend(
        title.position = 'left',
        keywidth = unit(6, 'cm'), nrow = 1,
        label.position = "bottom")
    ) +
    new_scale_fill() + 
    geom_tile(dat = mat_selected2_nonrev, aes(fill= valueFac), colour="white", size=0.7) +
    scale_fill_brewer(type = "seq", palette = "Reds", drop = FALSE,
                      name = "Proportion of wrongly selected variables  ") +
    xlab("") + ylab("") +
    ggtitle(title) + 
    scale_x_discrete(position = "top") +
    coord_cartesian(expand=FALSE)  +
    guides(
      fill = guide_legend(
        title.position = 'left',
        keywidth = unit(6, 'cm'), nrow = 1,
        label.position = "bottom")
    ) +
    theme_bw() +
    theme(panel.grid=element_blank(),
          plot.title = element_text(size = rel(2.8)),
          axis.text.x = element_text(size = rel(1.1)),
          axis.text.y = element_text(size = rel(3)),
          legend.position = "bottom",
          legend.text = element_text(size = rel(2.2)),
          legend.title = element_text(size = rel(2.8), vjust = 0.8),
          legend.spacing.x = unit(0, "cm"),
          legend.key.size = unit(2, "lines"),
          legend.box = "vertical",
          legend.justification = "left") 
  
} 





