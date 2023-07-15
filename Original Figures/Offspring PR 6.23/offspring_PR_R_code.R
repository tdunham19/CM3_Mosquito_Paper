library(tidyverse)
library(readxl)

# Offspring Anphevirus

df <- read_excel("Off_Anphe_PR.xlsx")

ggplot() + geom_jitter(data = filter(df, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= Viral_Load), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-2),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_wrap(~factor(Name, levels = c("Poza Rica F x New Orleans M (Rep. 1)", "Poza Rica F x Vergel M (Rep. 1)", "Poza Rica M x New Orleans F (Rep. 1)", "Poza Rica M x Vergel F (Rep. 1)", 
                                      "Poza Rica F x New Orleans M (Rep. 2)", "Poza Rica F x Vergel M (Rep. 2)", "Poza Rica M x New Orleans F (Rep. 2)",
                     "Poza Rica M x Vergel F (Rep. 2)")), nrow = 2) +
  theme(strip.text.x = element_text(size = 6)) +
  ylab("Viral Load") + 
  ggtitle("Starting Prevalence of Anphevirus", subtitle = "Offspring of Poza Rica, New Orleans & Vergel") +
  theme(legend.position = "none") 

ggsave("Offspring Anphevirus.pdf", height=6, width=8, units="in")

# Offspring Verdadero Virus

df2 <- read_excel("Off_Verdadero_PR.xlsx")

ggplot() + geom_jitter(data = filter(df2, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= Viral_Load), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df2, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-2),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_wrap(~factor(Name, levels = c("Poza Rica F x New Orleans M (Rep. 1)", "Poza Rica F x Vergel M (Rep. 1)", "Poza Rica M x New Orleans F (Rep. 1)", "Poza Rica M x Vergel F (Rep. 1)", 
                                      "Poza Rica F x New Orleans M (Rep. 2)", "Poza Rica F x Vergel M (Rep. 2)", "Poza Rica M x New Orleans F (Rep. 2)",
                                      "Poza Rica M x Vergel F (Rep. 2)")), nrow = 2) +
  theme(strip.text.x = element_text(size = 6)) +
  ylab("Viral Load") + 
  ggtitle("Starting Prevalence of Verdadero Virus", subtitle = "Offspring of Poza Rica, New Orleans & Vergel") +
  theme(legend.position = "none") 

ggsave("Offspring Verdadero.pdf", height=6, width=8, units="in")

# Offspring Guadeloupe Mosquito Virus

df3 <- read_excel("Off_GMV_PR.xlsx")

ggplot() + geom_jitter(data = filter(df3, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= Viral_Load), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df3, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-2),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_wrap(~factor(Name, levels = c("Poza Rica F x New Orleans M (Rep. 1)", "Poza Rica F x Vergel M (Rep. 1)", "Poza Rica M x New Orleans F (Rep. 1)", "Poza Rica M x Vergel F (Rep. 1)", 
                                      "Poza Rica F x New Orleans M (Rep. 2)", "Poza Rica F x Vergel M (Rep. 2)", "Poza Rica M x New Orleans F (Rep. 2)",
                                      "Poza Rica M x Vergel F (Rep. 2)")), nrow = 2) +
  theme(strip.text.x = element_text(size = 6)) +
  ylab("Viral Load") + 
  ggtitle("Starting Prevalence of Guadeloupe Mosquito Virus", subtitle = "Offspring of Poza Rica, New Orleans & Vergel") +
  theme(legend.position = "none") 

ggsave("Offspring GMV.pdf", height=6, width=8, units="in")
