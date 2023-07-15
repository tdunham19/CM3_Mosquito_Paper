library(tidyverse)
library(readxl)

# Vertical Transmission Tapachula crosses

df <- read_excel("Tapachula_crosses_verical_list.xlsx")

ggplot() + geom_jitter(data = filter(df, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= qPCR_Ct, fill=Virus, color = Virus), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-6),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_grid(vars(Virus), vars(Name)) +
  theme(strip.text.x = element_text(size = 6)) +
  ylim(0,40) +
  ylab("Virus Ct") + xlab("") + 
  ggtitle("Vertical Transmission of Viruses in Aedes Aegypti", subtitle = "Tapachula, New Orleans and Vergel Ae.aegypti Crosses") +
  scale_color_manual(values=c("#66CC99", "#56B4E9")) +
  scale_fill_manual(values=c("#66CC99", "#56B4E9")) +
  theme(legend.position = "none")

ggsave("T_crosses_vertical_Rplot.pdf", height = 10, width = 20, units = "in")


# Horizontal Transmission Tapachula crosses

df <- read_excel("T_crosses_horizontal_list.xlsx")

ggplot() + geom_jitter(data = filter(df, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= qPCR_Ct, fill=Virus, color = Virus), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-6),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_grid(vars(Virus), vars(Name)) +
  theme(strip.text.x = element_text(size = 6)) +
  ylim(0,40) +
  ylab("Virus Ct") + xlab("Sex and Cross") + 
  ggtitle("Horizontal Transmission of Viruses in Aedes Aegypti", subtitle = "Tapachula, New Orleans and Vergel Ae.aegypti Crosses") +
  scale_color_manual(values=c("#66CC99", "#56B4E9")) +
  scale_fill_manual(values=c("#66CC99", "#56B4E9")) +
  theme(legend.position = "none")

ggsave("T_crosses_horizontal_Rplot.pdf", height = 10, width = 20, units = "in")
