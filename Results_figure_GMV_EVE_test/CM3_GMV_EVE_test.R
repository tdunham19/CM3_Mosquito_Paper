#######
# R code for Guadeloupe Mosquito Virus (GMV) Endogenous Viral Element (EVE) Test Results
# Created by Tillie Dunham on 12.04.24
#######

library(tidyverse)
library(readxl)

# read in all data
df <- read_excel("CM3_GMV_EVE_test.xlsx")

ggplot(df)+
  geom_point(aes(x=factor(Colony, level=c('New Orleans', 'Poza Rica', 'Vergel','Negative Control', 'Positive Control')), 
                 y=GMV_Ct, fill=Colony), shape=21, size=3, stroke=0.25) +
  theme_bw() +
  xlab("") +
  ylab("GMV Ct") + 
  ggtitle("Testing for GMV as an EVE") + 
  theme(legend.position = "none")

ggsave("GMV_EVE_test_plot.pdf", height = 6, width = 8, units = "in")
