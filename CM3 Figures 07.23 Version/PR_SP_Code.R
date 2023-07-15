library(tidyverse)
library(readxl)

# Starting Prevalence Anphevirus

df <- read_excel("PR_SP_Anphe.xlsx")

# calculate prevalences, by group and sex
df_prevalence_group_and_sex <- df %>% count(Strain, Sex, Infected)
df_prevalence_group_and_sex <- df_prevalence_group_and_sex %>% group_by(Strain, Sex) %>% mutate(total = sum(n))
df_prevalence_group_and_sex <- df_prevalence_group_and_sex %>% mutate(fraction_positive =  n / total) %>% filter(Infected == "Positive")

df_prevalence_group_simple <- df_prevalence_group_and_sex %>% select(Strain, Sex, fraction_positive)

# merge in prevalence info into main data frame
df <- left_join(df, df_prevalence_group_simple)
# missing values (no positives in group) had prevalence of NA -> switch to 0
df$fraction_positive <- replace_na(df$fraction_positive, 0)
# convert to %
df <- df %>% mutate(percent_positive = sprintf("%.0f", fraction_positive * 100))
# make a fancy label with prevalence info embedded in it
df <- df %>% mutate(group_with_prev = paste0(Strain, Sex, " (", percent_positive, "% positive)"))

# set the Y axis minimum: we will plot negatives at this value: these will be colored differently
# (in grey) to indicate they are negative.
y_axis_min <- 1e-5

ggplot() + geom_jitter(data = filter(df, Infected == "Positive"), 
                       mapping = aes(x=group_with_prev, y= Viral_Load), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Infected == "Negative"),
              mapping = aes(x=group_with_prev, y=y_axis_min),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  theme(strip.text.x = element_text(size = 6)) +
  facet_grid(~Strain, scales = "free") +
  ylab("Viral Load") + 
  ggtitle("Starting Prevalence of Anphevirus", subtitle = "Poza Rica, New Orleans & Vergel") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.title.x = element_blank()) + 
  scale_y_log10()

ggsave("PR SP Anphe.pdf", height=8, width=8, units="in")



# Starting Prevalence GMV

df <- read_excel("PR_SP_GMV.xlsx")

# calculate prevalences, by group and sex
df_prevalence_group_and_sex <- df %>% count(Strain, Sex, Infected)
df_prevalence_group_and_sex <- df_prevalence_group_and_sex %>% group_by(Strain, Sex) %>% mutate(total = sum(n))
df_prevalence_group_and_sex <- df_prevalence_group_and_sex %>% mutate(fraction_positive =  n / total) %>% filter(Infected == "Positive")

df_prevalence_group_simple <- df_prevalence_group_and_sex %>% select(Strain, Sex, fraction_positive)

# merge in prevalence info into main data frame
df <- left_join(df, df_prevalence_group_simple)
# missing values (no positives in group) had prevalence of NA -> switch to 0
df$fraction_positive <- replace_na(df$fraction_positive, 0)
# convert to %
df <- df %>% mutate(percent_positive = sprintf("%.0f", fraction_positive * 100))
# make a fancy label with prevalence info embedded in it
df <- df %>% mutate(group_with_prev = paste0(Strain, Sex, " (", percent_positive, "% positive)"))

# set the Y axis minimum: we will plot negatives at this value: these will be colored differently
# (in grey) to indicate they are negative.
y_axis_min <- 1e-5

ggplot() + geom_jitter(data = filter(df, Infected == "Positive"), 
                       mapping = aes(x=group_with_prev, y= Viral_Load), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Infected == "Negative"),
              mapping = aes(x=group_with_prev, y=y_axis_min),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  theme(strip.text.x = element_text(size = 6)) +
  facet_grid(~Strain, scales = "free") +
  ylab("Viral Load") + 
  ggtitle("Starting Prevalence of Guadeloupe Mosquito Virus", subtitle = "Poza Rica, New Orleans & Vergel") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.title.x = element_blank()) + 
  scale_y_log10()

ggsave("PR SP GMV.pdf", height=8, width=8, units="in")




# Starting Prevalence Verdadero

df <- read_excel("PR_SP_Verd.xlsx")

# calculate prevalences, by group and sex
df_prevalence_group_and_sex <- df %>% count(Strain, Sex, Infected)
df_prevalence_group_and_sex <- df_prevalence_group_and_sex %>% group_by(Strain, Sex) %>% mutate(total = sum(n))
df_prevalence_group_and_sex <- df_prevalence_group_and_sex %>% mutate(fraction_positive =  n / total) %>% filter(Infected == "Positive")

df_prevalence_group_simple <- df_prevalence_group_and_sex %>% select(Strain, Sex, fraction_positive)

# merge in prevalence info into main data frame
df <- left_join(df, df_prevalence_group_simple)
# missing values (no positives in group) had prevalence of NA -> switch to 0
df$fraction_positive <- replace_na(df$fraction_positive, 0)
# convert to %
df <- df %>% mutate(percent_positive = sprintf("%.0f", fraction_positive * 100))
# make a fancy label with prevalence info embedded in it
df <- df %>% mutate(group_with_prev = paste0(Strain, Sex, " (", percent_positive, "% positive)"))

# set the Y axis minimum: we will plot negatives at this value: these will be colored differently
# (in grey) to indicate they are negative.
y_axis_min <- 1e-5

ggplot() + geom_jitter(data = filter(df, Infected == "Positive"), 
                       mapping = aes(x=group_with_prev, y= Viral_Load), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Infected == "Negative"),
              mapping = aes(x=group_with_prev, y=y_axis_min),
              width=0.15, height = 0, shape=21, fill="grey70", color="black", size=2, alpha=0.25) +
  theme(strip.text.x = element_text(size = 6)) +
  facet_grid(~Strain, scales = "free") +
  ylab("Viral Load") + 
  ggtitle("Starting Prevalence of Verdadero Virus", subtitle = "Poza Rica, New Orleans & Vergel") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.title.x = element_blank()) + 
  scale_y_log10()

ggsave("PR SP Verdadero.pdf", height=8, width=8, units="in")


