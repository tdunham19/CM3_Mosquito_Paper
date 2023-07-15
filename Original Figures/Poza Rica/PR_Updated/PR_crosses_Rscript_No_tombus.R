library(tidyverse)
library(readxl)

# Vertical Transmission Cross + Virus Graph Poza Rica x Vergel

df <- read_excel("PRxV_vertical_list_no_tombus.xlsx")
     
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
  theme(legend.position = "none") + theme_grey() + theme(legend.position = "none") +
    theme(strip.text.x = element_text(size = 5), 
          strip.text.y = element_text(size = 8)) 


  # Horizontal Transmission Cross + Virus Graph Poza Rica x Vergel
  
  df2 <- read_excel("PRxV_horizontal_list_no_tombus.xlsx")
                   
ggplot() + geom_jitter(data = filter(df2, Virus_TF == "T"), 
          mapping = aes(x= Sex, y= Virus_Ct, fill=Virus_type, color = Virus_type), 
          width=0.15, height = 0, shape=21, size=2) +
      geom_jitter(data = filter(df2, Virus_TF == "F"),
           mapping = aes(x=Sex, y=1e-2),
           width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
        facet_grid(vars(Virus_type), vars(Name)) +
         ylim(0,40) +
  theme(strip.text.x = element_text(size = 6)) +
  ylab("Virus Ct") + xlab("Sex and Cross") + 
  ggtitle("Horizontal Transmission of Viruses During Mating", subtitle = "Poza Rica & Vergel Ae.aegypti Crosses") +
  scale_color_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  scale_fill_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  theme(legend.position = "none")  + theme_grey() + theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 5), 
        strip.text.y = element_text(size = 8)) 


# Vertical Transmission Cross + Virus Graph Poza Rica x New Orleans

df3 <- read_excel("PRxNO_vertical_list_no_tombus.xlsx")

ggplot() + geom_jitter(data = filter(df3, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= Virus_Ct, fill=Virus_type, color = Virus_type), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df3, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-2),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_grid(vars(Virus_type), vars(Name)) +
  theme(strip.text.x = element_text(size = 6)) +
  ylim(0,40) +
  ylab("Virus Ct") + xlab("Sex and Cross") + 
  ggtitle("Vertical Transmission of Viruses From Parent to Offspring", subtitle = "Poza Rica & New Orleans Ae.aegypti Crosses") +
  scale_color_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  scale_fill_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  theme(legend.position = "none") + theme_grey() + theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 5), 
        strip.text.y = element_text(size = 8)) 

# Horizontal Transmission Cross + Virus Graph Poza Rica x New Orleans

df4 <- read_excel("PRxNO_Horizontal_list_no_tombus.xlsx")

ggplot() + geom_jitter(data = filter(df4, Virus_TF == "T"), 
                       mapping = aes(x= Sex, y= Virus_Ct, fill=Virus_type, color = Virus_type), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df4, Virus_TF == "F"),
              mapping = aes(x=Sex, y=1e-2),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  facet_grid(vars(Virus_type), vars(Name)) +
  ylim(0,40) +
  theme(strip.text.x = element_text(size = 6)) +
  ylab("Virus Ct") + xlab("Sex and Cross") + 
  ggtitle("Horizontal Transmission of Viruses During Mating", subtitle = "Poza Rica & New Orleans Ae.aegypti Crosses") +
  scale_color_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  scale_fill_manual(values=c("#CC6666", "#66CC99", "#9999CC", "#56B4E9")) +
  theme(legend.position = "none") + theme_grey() + theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 5), 
        strip.text.y = element_text(size = 8)) 

