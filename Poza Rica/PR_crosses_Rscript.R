library(tidyverse)
library(readxl)

# Vertical Transmission Cross + Virus Graph Poza Rica x Vergel

df <- read_excel("PRxV_vertical_virus_list.xlsx")
     
  ggplot() + geom_jitter(data = filter(df, Virus_TF == "T"), 
              mapping = aes(x= Sex, y= Virus_Ct, fill=Virus_type, color = Virus_type), 
              width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-2),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_grid(vars(Virus_type), vars(Name)) +
    theme(strip.text.x = element_text(size = 6)) +
  ylim(0,40) +
  ylab("Virus Ct") + xlab("Sex and Cross") + 
    ggtitle("Vertical Transmission of Viruses From Parent to Offspring", subtitle = "Poza Rica & Vergel Ae.aegypti Crosses") +
  scale_color_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  scale_fill_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  theme(legend.position = "none")
  
  ggsave("PRxV_Vertical_Grid_plot", height=7, width=10, units="in")
  
  # Horizontal Transmission Cross + Virus Graph Poza Rica x Vergel
  
  df <- read_excel("PRxV_horizontal_virus_list.xlsx")
                   
ggplot() + geom_jitter(data = filter(df, Virus_TF == "T"), 
          mapping = aes(x= Sex, y= Virus_Ct, fill=Virus_type, color = Virus_type), 
          width=0.15, height = 0, shape=21, size=2) +
      geom_jitter(data = filter(df, Virus_TF == "F"),
           mapping = aes(x=Sex, y=1e-2),
           width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
        facet_grid(vars(Virus_type), vars(Name)) +
         ylim(0,40) +
  theme(strip.text.x = element_text(size = 6)) +
  ylab("Virus Ct") + xlab("Sex and Cross") + 
  ggtitle("Horizontal Transmission of Viruses During Mating", subtitle = "Poza Rica & Vergel Ae.aegypti Crosses") +
  scale_color_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  scale_fill_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  theme(legend.position = "none")


# Vertical Transmission Cross + Virus Graph Poza Rica x New Orleans

df <- read_excel("PRxNO_vertical_virus_list.xlsx")

ggplot() + geom_jitter(data = filter(df, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= Virus_Ct, fill=Virus_type, color = Virus_type), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-2),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_grid(vars(Virus_type), vars(Name)) +
  theme(strip.text.x = element_text(size = 6)) +
  ylim(0,40) +
  ylab("Virus Ct") + xlab("Sex and Cross") + 
  ggtitle("Vertical Transmission of Viruses From Parent to Offspring", subtitle = "Poza Rica & New Orleans Ae.aegypti Crosses") +
  scale_color_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  scale_fill_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  theme(legend.position = "none")

ggsave("PRxV_Vertical_Grid_plot", height=7, width=10, units="in")

# Horizontal Transmission Cross + Virus Graph Poza Rica x New Orleans

df <- read_excel("PRxNO_Horizontal_virus_list.xlsx")

ggplot() + geom_jitter(data = filter(df, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= Virus_Ct, fill=Virus_type, color = Virus_type), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-2),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_grid(vars(Virus_type), vars(Name)) +
  ylim(0,40) +
  theme(strip.text.x = element_text(size = 6)) +
  ylab("Virus Ct") + xlab("Sex and Cross") + 
  ggtitle("Horizontal Transmission of Viruses During Mating", subtitle = "Poza Rica & New Orleans Ae.aegypti Crosses") +
  scale_color_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  scale_fill_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  theme(legend.position = "none")

