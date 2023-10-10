#Purpose: Generates SHAP visualizations
library(shapviz)
library(ggplot2)
library(dplyr)
library(ggallin)
library(ggeasy)
source("00_functions.R")

outcomeShap <- readRDS("data/processed/outcomeShap.rds")

probShap <- special_transform(outcomeShap)

#outcome viz
outcomeShap2 <- probShap
outcomeShap2$S <- cleanOutcomeShap(outcomeShap2$S,noFactors = F,treeshap = F)
outcomeShap2$X <- cleanOutcomeShap(outcomeShap2$X,noFactors = F,treeshap=F)
outcomeShap2$X <- cleanXmat(outcomeShap2$X)

outcomeShap_plot <- sv_importance(outcomeShap2, kind = "beeswarm",
                                  show_numbers = F,max_display = 10,
                                  bee_adjust = .50,
                                  viridis_args=list(begin=0.25,
                                                    end=.85,
                                                    option="mako"),
                                  alpha=.5)
outcomePlot <- outcomeShap_plot + theme_classic() +
  easy_remove_axes(which="y") +
  xlab("Impact on estimated probability of lead exposure") +
  ggtitle("Local variable explanations") + scale_x_continuous(breaks=seq(-1,1,by=.25))

outcomeShap_imp <- sv_importance(outcomeShap2,kind="bar",
                                 max_display=10,fill="#3488A6FF")
impPlot <- outcomeShap_imp + theme_classic() +
  # scale_x_continuous(breaks=seq(0,0.15,by=0.05)) +
  xlim(c(0,.15))+
  xlab("mean(|Contribution to estimate|)") +
  ggtitle("Global variable importance")

#dependence plot
blockPopPlot <- sv_dependence(probShap,v="blockPopulation",color_var=NULL,
              color="#3488A6FF",alpha=.2) + scale_x_continuous(trans='log10',
                                                               breaks=c(1,2,3,4,5,10,100,1000)) +
  theme_classic() + xlab("Block population") + 
  ylab(expression(atop("Impact on estimated", "probability of lead exposure")))

buildingsPlot <- sv_dependence(probShap,v="nBlocks",color_var=NULL,
              color="#3488A6FF",alpha=.2) + scale_x_continuous(trans='log10',
                                                               breaks=c(1,2,3,4,5,10,100)) +
  theme_classic() + xlab("No. of buildings (Block)") + ylab(expression(atop("Impact on estimated", "probability of lead exposure")))


caShapMap <- readRDS("data/processed/caShapMap.rds")
map_legend <- get_legend(
  caShapMap + theme(legend.box.margin = margin(0, 0, 0, 12))
)
mapPlot <- plot_grid(
  caShapMap + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  nrow = 1
)


library(egg)
featurePlot <- ggarrange(impPlot,outcomePlot+easy_remove_axes(which="y"),
          nrow=1)

library(patchwork)
patchwork <- (wrap_elements(full = featurePlot))/(caShapMap+blockPopPlot+buildingsPlot)
patchwork2 <- patchwork + plot_annotation(tag_levels='A') &
  theme(plot.tag = element_text(size = 12,face='bold'))
ggsave("figs/multiShap.png",patchwork2,width=12,height=6)
ggsave("figs/multiShap.pdf",patchwork2,width=12,height=6)
