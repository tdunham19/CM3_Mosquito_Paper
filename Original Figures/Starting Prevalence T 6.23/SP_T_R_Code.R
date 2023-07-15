library(tidyverse)
library(readxl)

# Starting Prevalence Phasi Charoen

df <- read_excel("Phasi_SP.xlsx")

ggplot() + geom_jitter(data = filter(df, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= Viral_Load), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-2),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_wrap(~Name) +
  theme(strip.text.x = element_text(size = 6)) +
  ylab("Viral Load") + 
  ggtitle("Starting Prevalence of Phasi Charoen Virus", subtitle = "Tapachula, New Orleans & Vergel") +
  theme(legend.position = "none") 

ggsave("Starting Prevalence Phasi.pdf", height=6, width=8, units="in")

# Starting Prevalence Guadeloupe Mosquito Virus

df2 <- read_excel("GMV_SP_T.xlsx")

ggplot() + geom_jitter(data = filter(df2, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= Viral_Load), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df2, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-2),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_wrap(~Name) +
  theme(strip.text.x = element_text(size = 6)) +
  ylab("Viral Load") + 
  ggtitle("Starting Prevalence of Guadeloupe Mosquito Virus", subtitle = "Tapachula, New Orleans & Vergel") +
  theme(legend.position = "none") 

ggsave("Starting Prevalence GMV.pdf", height=6, width=8, units="in")
