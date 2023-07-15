library(tidyverse)
library(readxl)

# Offspring Guadeloupe Mosquito Virus

df <- read_excel("Off_GMV_T.xlsx")

ggplot() + geom_jitter(data = filter(df, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= Viral_Load), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-2),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_wrap(~factor(Name, levels = c("Tapachula F x New Orleans M (Rep. 1)", "Tapachula F x Vergel M (Rep. 1)", "Tapachula M x New Orleans F (Rep. 1)", "Tapachula M x Vergel F (Rep. 1)", 
                                      "Tapachula F x New Orleans M (Rep. 2)", "Tapachula F x Vergel M (Rep. 2)", "Tapachula M x New Orleans F (Rep. 2)",
                                      "Tapachula M x Vergel F (Rep. 2)")), nrow = 2) +
  theme(strip.text.x = element_text(size = 6)) +
  ylab("Viral Load") + 
  ggtitle("Starting Prevalence of Guadeloupe Mosquito Virus", subtitle = "Offspring of Tapachula, New Orleans & Vergel") +
  theme(legend.position = "none") 

ggsave("Offspring GMV T.pdf", height=6, width=8, units="in")

# Offspring Phasi Charoen Virus

df2 <- read_excel("Off_Phasi_T.xlsx")

ggplot() + geom_jitter(data = filter(df2, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= Viral_Load), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df2, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-2),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_wrap(~factor(Name, levels = c("Tapachula F x New Orleans M (Rep. 1)", "Tapachula F x Vergel M (Rep. 1)", "Tapachula M x New Orleans F (Rep. 1)", "Tapachula M x Vergel F (Rep. 1)", 
                                      "Tapachula F x New Orleans M (Rep. 2)", "Tapachula F x Vergel M (Rep. 2)", "Tapachula M x New Orleans F (Rep. 2)",
                                      "Tapachula M x Vergel F (Rep. 2)")), nrow = 2) +
  theme(strip.text.x = element_text(size = 6)) +
  ylab("Viral Load") + 
  ggtitle("Starting Prevalence of Phasi Charoen Virus", subtitle = "Offspring of Tapachula, New Orleans & Vergel") +
  theme(legend.position = "none") 

ggsave("Offspring Phasi T.pdf", height=6, width=8, units="in")
