# Figure to compare IBS performance for varying data size and scenarios

library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(ggpubr)

source("Code/plot_IBS_boxplots.R")


dir <- "Results/"
for (i in 1:8) {
  j <- i %% 4
  if (j == 0) j <- 4
  scenario <- ifelse(i<=4, "Scenario2/", "Scenario3/")
  direct <- paste0(dir, scenario)
  setting <- dir(direct)[[j]]
  
  # Load and plot individual data:
  load(paste0(direct, setting, "/pecBoot632plus_", setting, ".rds"))
  plotObjectName <- paste0("plot", i)
  
  scenario <- ifelse(i<=4, "Scenario 2", "Scenario 3")
  setting <- ifelse(j == 1, "(three teams)",
                    ifelse(j == 2, "(six teams)", 
                           ifelse(j ==3, "(single team)", "(ten teams)")))
  title <- paste(scenario, setting)
  
  assign(plotObjectName, plot_IBS_boxplots(pecBoot632plus_bess, pecBoot632plus_lasso,
                                           pecBoot632plus_enet, pecBoot632plus_ridge, 
                                           pecBoot632plus_grouplasso, pecBoot632plus_coxboost, 
                                           splitMethod = "Boot632plusErr") +
           ggtitle(title) + ylim(0, 0.3)+
           theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.text.y = element_text(size = 11),
                 axis.ticks.x = element_blank(),
                 legend.title = element_text(size = 18),
                 legend.text = element_text(size = 16),
                 title = element_text(size = 12)))
}


# Visualize everything:
## plot3 and plot7: very small sample size setting scenario 2 and 3
## plot1 and plot5 small sample size setting scenario 2 and 3
## plot4 and plot6 large sample size setting scenario 2 and 3
## plot 4 and plot8 very large sample size setting scenario 2 and 3
plot <- ggarrange(plot3, plot1, plot2, plot4, plot7, plot5, plot6, plot8,
                  label.y = "IBS",
                  nrow = 2, ncol = 4, common.legend = TRUE, legend = "bottom")
plot <- annotate_figure(plot,
                        left = text_grob("IBS", size = 22, rot = 90),
)
ggexport(plot, filename = "Figures/IBS_boxplots.pdf", width = 12, height = 8)


