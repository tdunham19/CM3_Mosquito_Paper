library(tidyverse)
library(readxl)

# Maternal Transmission to Offspring - Phasi Charoen

df <- read_excel("Off_Phasi_T_maternal.xlsx")

# calculate prevalences by group 
df_prevalence_group <- df %>% count(Sex, Infected)
df_prevalence_group <- df_prevalence_group %>% group_by(Sex) %>% mutate(total = sum(n))
df_prevalence_group <- df_prevalence_group %>% mutate(fraction_positive =  n / total) %>% filter(Infected == "Positive")
df_prevalence_group_simple <- df_prevalence_group %>% select(Sex, fraction_positive)

# merge in prevalence info into main data frame
df <- left_join(df, df_prevalence_group_simple)
# missing values (no positives in group) had prevalence of NA -> switch to 0
df$fraction_positive <- replace_na(df$fraction_positive, 0)
# convert to %
df <- df %>% mutate(percent_positive = sprintf("%.0f", fraction_positive * 100))
# make a fancy label with prevalence info embedded in it
df <- df %>% mutate(group_with_prev = paste0(Sex, " (", percent_positive, "% positive)"))

# set the Y axis minimum: we will plot negatives at this value: these will be colored differently
# (in grey) to indicate they are negative.
y_axis_min <- 1e-5

ggplot() + geom_jitter(data = filter(df, Infected == "Positive"), 
                       mapping = aes(x= group_with_prev, y= Viral_Load, fill=Replicate), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Infected == "Negative"),
              mapping = aes(x=group_with_prev, y=y_axis_min, fill=Replicate),
              width=0.15, height = 0, shape=21, size=2, alpha=0.25) +
  theme(strip.text.x = element_text(size = 6)) +
  facet_wrap(~factor(Name), nrow = 1) +
  ylab("Viral Load") + 
  ggtitle("Maternal Transmission of Phasi Charoen Virus", subtitle = "Offspring of Tapachula Female and New Orleans & Vergel Male") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.title.x = element_blank()) + 
  scale_fill_manual(values=c("#006666", "#66CCCC")) + scale_y_log10()

ggsave(" T Maternal Transmission to Offspring Phasi.pdf", height=8, width=8, units="in")

# Paternal Transmission to Offspring - Phasi

df <- read_excel("Off_Phasi_T_paternal.xlsx")

# calculate prevalences by group 
df_prevalence_group <- df %>% count(Sex, Infected)
df_prevalence_group <- df_prevalence_group %>% group_by(Sex) %>% mutate(total = sum(n))
df_prevalence_group <- df_prevalence_group %>% mutate(fraction_positive =  n / total) %>% filter(Infected == "Positive")
df_prevalence_group_simple <- df_prevalence_group %>% select(Sex, fraction_positive)

# merge in prevalence info into main data frame
df <- left_join(df, df_prevalence_group_simple)
# missing values (no positives in group) had prevalence of NA -> switch to 0
df$fraction_positive <- replace_na(df$fraction_positive, 0)
# convert to %
df <- df %>% mutate(percent_positive = sprintf("%.0f", fraction_positive * 100))
# make a fancy label with prevalence info embedded in it
df <- df %>% mutate(group_with_prev = paste0(Sex, " (", percent_positive, "% positive)"))

# set the Y axis minimum: we will plot negatives at this value: these will be colored differently
# (in grey) to indicate they are negative.
y_axis_min <- 1e-5

ggplot() + geom_jitter(data = filter(df, Infected == "Positive"), 
                       mapping = aes(x= group_with_prev, y= Viral_Load, fill=Replicate), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Infected == "Negative"),
              mapping = aes(x=group_with_prev, y=y_axis_min, fill=Replicate),
              width=0.15, height = 0, shape=21, size=2, alpha=0.25) +
  theme(strip.text.x = element_text(size = 6)) +
  facet_wrap(~factor(Name), nrow = 1) +
  ylab("Viral Load") + 
  ggtitle("Paternal Transmission of Phasi Charoen Virus", subtitle = "Offspring of Tapachula Male and New Orleans & Vergel Female") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.title.x = element_blank()) + 
  scale_fill_manual(values=c("#006666", "#66CCCC")) + scale_y_log10()

ggsave("T Paternal Transmission to Offspring Phasi.pdf", height=8, width=8, units="in")







# Maternal Transmission to Offspring - GMV

df <- read_excel("Off_GMV_T_maternal.xlsx")

# calculate prevalences by group 
df_prevalence_group <- df %>% count(Sex, Infected)
df_prevalence_group <- df_prevalence_group %>% group_by(Sex) %>% mutate(total = sum(n))
df_prevalence_group <- df_prevalence_group %>% mutate(fraction_positive =  n / total) %>% filter(Infected == "Positive")
df_prevalence_group_simple <- df_prevalence_group %>% select(Sex, fraction_positive)

# merge in prevalence info into main data frame
df <- left_join(df, df_prevalence_group_simple)
# missing values (no positives in group) had prevalence of NA -> switch to 0
df$fraction_positive <- replace_na(df$fraction_positive, 0)
# convert to %
df <- df %>% mutate(percent_positive = sprintf("%.0f", fraction_positive * 100))
# make a fancy label with prevalence info embedded in it
df <- df %>% mutate(group_with_prev = paste0(Sex, " (", percent_positive, "% positive)"))

# set the Y axis minimum: we will plot negatives at this value: these will be colored differently
# (in grey) to indicate they are negative.
y_axis_min <- 1e-5

ggplot() + geom_jitter(data = filter(df, Infected == "Positive"), 
                       mapping = aes(x= group_with_prev, y= Viral_Load, fill=Replicate), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Infected == "Negative"),
              mapping = aes(x=group_with_prev, y=y_axis_min, fill=Replicate),
              width=0.15, height = 0, shape=21, size=2, alpha=0.25) +
  theme(strip.text.x = element_text(size = 6)) +
  facet_wrap(~factor(Name), nrow = 1) +
  ylab("Viral Load") + 
  ggtitle("Maternal Transmission of Guadeloupe Mosquito Virus", subtitle = "Offspring of Tapachula Female and New Orleans & Vergel Male") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.title.x = element_blank()) + 
  scale_fill_manual(values=c("#006666", "#66CCCC")) + scale_y_log10()

ggsave("T Maternal Transmission to Offspring GMV.pdf", height=8, width=8, units="in")

# Paternal Transmission to Offspring - GMV

df <- read_excel("Off_GMV_T_paternal.xlsx")

# calculate prevalences by group 
df_prevalence_group <- df %>% count(Sex, Infected)
df_prevalence_group <- df_prevalence_group %>% group_by(Sex) %>% mutate(total = sum(n))
df_prevalence_group <- df_prevalence_group %>% mutate(fraction_positive =  n / total) %>% filter(Infected == "Positive")
df_prevalence_group_simple <- df_prevalence_group %>% select(Sex, fraction_positive)

# merge in prevalence info into main data frame
df <- left_join(df, df_prevalence_group_simple)
# missing values (no positives in group) had prevalence of NA -> switch to 0
df$fraction_positive <- replace_na(df$fraction_positive, 0)
# convert to %
df <- df %>% mutate(percent_positive = sprintf("%.0f", fraction_positive * 100))
# make a fancy label with prevalence info embedded in it
df <- df %>% mutate(group_with_prev = paste0(Sex, " (", percent_positive, "% positive)"))

# set the Y axis minimum: we will plot negatives at this value: these will be colored differently
# (in grey) to indicate they are negative.
y_axis_min <- 1e-5

ggplot() + geom_jitter(data = filter(df, Infected == "Positive"), 
                       mapping = aes(x= group_with_prev, y= Viral_Load, fill=Replicate), 
                       width=0.15, height = 0, shape=21, size=2) +
  geom_jitter(data = filter(df, Infected == "Negative"),
              mapping = aes(x=group_with_prev, y=y_axis_min, fill=Replicate),
              width=0.15, height = 0, shape=21, size=2, alpha=0.25) +
  theme(strip.text.x = element_text(size = 6)) +
  facet_wrap(~factor(Name), nrow = 1) +
  ylab("Viral Load") + 
  ggtitle("Paternal Transmission of Guadeloupe Mosquito Virus", subtitle = "Offspring of Tapachula Male and New Orleans & Vergel Female") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.title.x = element_blank()) + 
  scale_fill_manual(values=c("#006666", "#66CCCC")) + scale_y_log10()

ggsave("T Paternal Transmission to Offspring GMV.pdf", height=8, width=8, units="in")



