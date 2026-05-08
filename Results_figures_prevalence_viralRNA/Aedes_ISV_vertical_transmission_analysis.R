library(tidyverse)
library(patchwork)
library(readxl)
library(binom)
library(lme4)
library(sjPlot)
library(ResourceSelection)
library(formatdown)
library(flextable)
library(modelsummary)


# This script process, analyzes, and plots data
# from Aedes aegypti ISV VT experiments
# 
# TJD, MDS, 2024-2026


# create an output file and text variable to capture text output from this analysis
plot_output_dir   <- "../paper/figures/"
table_output_dir  <- "../paper/tables/"
text_output_dir   <- "../paper/text/"
output_dir  <- "./"
output_file <-file(paste0(output_dir, "paper_text.txt"))
output_text <- ""


# ************************************************************************************************
# ------------------------
# DATE INPUT / WRANGLING
# ------------------------
# ************************************************************************************************

# read in all data
df <- read_excel("all_data.xlsx")

# don't include Tapachula data
# head(df)
# df <- df %>% filter(!str_detect(Name, "Tapachula"))

# make a boolean version of infected (character)
df <- df %>% mutate(infected_boolean = if_else(Infected == "Positive", T, F))

# what was infected parent colony?
df <- df %>% mutate(infected_parent_colony = case_when(
  Transmission_mode == "maternal"   ~ Maternal_colony ,
  Transmission_mode == "paternal"   ~ Paternal_colony,
  # for HT, Poza Rica & Tapachula will be considered the "infected" parent, even though technically Vergel & New Orleans both infected at low levels
  # Transmission_mode == "Horizontal" ~ if_else(str_detect(Name, "Poza|Tapachula"), Strain, NA_character_),
  Transmission_mode == "Horizontal" ~ Crossed_with,
  .default = NA_character_
))

# what was non-infected parent colony?
df <- df %>% mutate(non_infected_parent_colony = case_when(
  Transmission_mode == "maternal"   ~ Paternal_colony ,
  Transmission_mode == "paternal"   ~ Maternal_colony,
  # for HT, Poza Rica & Tapachula will be considered the "infected" parent, even though technically Vergel & New Orleans both infected at low levels
  Transmission_mode == "Horizontal" ~ if_else(str_detect(Strain, "Orleans|Vergel"), Strain, NA_character_),
  .default = NA_character_
))

# number the crosses: 1 -> whatever
cross_numbers <- df %>% group_by(Name) %>% summarize() %>% mutate(cross_number = row_number())
df            <- left_join(df, cross_numbers)
# make a new grouping random effect variable: cross x replication
df            <- df %>% mutate(cross_rep = paste0(cross_number, "_", Replicate)) 

# how many crosses were there?
number_crosses <- nrow(df %>% group_by(cross_rep) %>% summarize)

output_text <- paste0("We performed ", number_crosses, " crosses.\n\n")
output_text

# uniquely number each mosquito within groups
df <- df %>%
  group_by(Name, Replicate, Sex, Virus_target) %>%
  mutate(mosquito_number = row_number())
  
# ------------------------------
# calculate prevalences by group 
# ------------------------------

# groups defined by Name (expt), Replicate, Strain (tested), Sex, Stage, Transmission_mode, Virus_target, Crossed_with
df_group_sizes <- df %>% 
  # group_by(Name, Replicate, Strain, Sex, Stage, Transmission_mode, Virus_target) %>%
  group_by(Name, Replicate, Strain, Sex, Stage, Transmission_mode, Virus_target) %>%
  summarize(n = n(), .groups="drop")

# most groups should have 12 individuals: check that 
# a few have 10 or 11, (because of inclusion of PCR ctrls in 96 well plates)
print(filter(df_group_sizes, n != 12), n=40)

df_positives_per_group <- df %>%
  filter(Infected == "Positive") %>%
  group_by(Name, Replicate, Strain, Sex, Stage, Transmission_mode, Virus_target) %>% 
  summarize(n_positive = n(), .groups="drop")

# merge # positives with group sizes
df_prevalence_by_group <- left_join(df_group_sizes, df_positives_per_group)

# missing values (no positives in group) had prevalence of NA -> switch to 0
df_prevalence_by_group$n_positive <- replace_na(df_prevalence_by_group$n_positive, 0)

# calculate prevalences by group and binomial confidence intervals
prev_conf_int <- binom.confint(df_prevalence_by_group$n_positive, 
                               df_prevalence_by_group$n, 
                               method="exact")

# create new columns with means and confint
df_prevalence_by_group$percent_positive  <- prev_conf_int$mean * 100
df_prevalence_by_group$lower_conf_int    <- prev_conf_int$lower * 100
df_prevalence_by_group$upper_conf_int    <- prev_conf_int$upper * 100

# convert to %
df_prevalence_by_group <- df_prevalence_by_group %>% 
  mutate(percent_positive_text = sprintf("%.0f", percent_positive))

# make a fancy label with prevalence info embedded in it
df_prevalence_by_group <- df_prevalence_by_group %>% 
  mutate(group_with_prev = paste0(Sex, 
                                  " ", 
                                  n_positive,
                                  "/",
                                  n, 
                                  " (", 
                                  percent_positive_text, 
                                  "%)"))

# set the Y axis minimum: we will plot negatives at this value: these will be colored differently
# (in grey) to indicate they are negative.
y_axis_min <- 1e-5
y_axis_max <- 1e+4

# Replace negative values with the lowest point on plot (negative)
df <- df %>% mutate(Viral_Load = if_else(Infected != "Positive", y_axis_min, Viral_Load))

# merge in prevalence info into main DF
df <- left_join(df, df_prevalence_by_group)

# calculate average loads per group
df_load_per_group <- df %>%
  filter(Infected == "Positive") %>%
  group_by(Name, Strain, Sex, Stage, Transmission_mode, Virus_target) %>% 
  summarize(mean_load   = mean(Viral_Load),
            median_load = median(Viral_Load),
            stdev_load  = sd(Viral_Load),
            .groups = "drop")


# ************************************************************************************************
# ------------------
# MODELING / STATS
# ------------------
# ************************************************************************************************

# we will assess VT efficiencies as represented by the fraction of
# offspring infected.  So get the offspring
# possible todo: consider prevalence in parents in model?
df_offspring <- df %>% filter(Stage == "Offspring") %>% ungroup()

# make modeling variables factors
df_offspring$Infected_parent_colony       <- as.factor(df_offspring$infected_parent_colony)
df_offspring$Non_infected_parent_colony   <- as.factor(df_offspring$non_infected_parent_colony)
df_offspring$Strain                       <- as.factor(df_offspring$Strain)
df_offspring$Transmission_mode            <- as.factor(df_offspring$Transmission_mode)
df_offspring$Virus_target                 <- as.factor(df_offspring$Virus_target)
df_offspring$Offspring_sex                <- as.factor(df_offspring$Sex)
df_offspring$Cross_rep                    <- as.factor(df_offspring$cross_rep)

df_offspring_m <- df_offspring %>% select(infected_boolean, 
                                          Offspring_sex, 
                                          Transmission_mode, 
                                          Infected_parent_colony, 
                                          Non_infected_parent_colony, 
                                          Strain, 
                                          Virus_target, 
                                          Cross_rep)

# everything model
# estimate the model and store results in m
full_model <- glmer(infected_boolean ~ 
                      Virus_target + 
                      Offspring_sex + 
                      # Infected_parent_colony + 
                      Non_infected_parent_colony + 
                      Transmission_mode + 
                      (1 | Cross_rep), 
                    data = df_offspring_m, 
                    family = binomial) 

summary(full_model)

# significant:
# virus 
# infected parent colony (starting prev.?)
# transmission mode 

# what if we do it one virus at a time?

# --------------------
# Anphevirus modeling 
# --------------------

df_offspring_anphe <- filter(df_offspring_m, Virus_target == "Aedes Anphevirus")
anphe_model_with_random <- glmer(infected_boolean ~ 
                                   Offspring_sex + 
                                   Non_infected_parent_colony + 
                                   Transmission_mode + 
                                   (1 | Cross_rep), 
                                 data = df_offspring_anphe,
                                 family = binomial) 

summary(anphe_model_with_random)

# anphe-only model, significant: 
# paternal vs. maternal 
# 
# not significant:
# sex of offspring
# non-infected paternal colony  

# what about fixed effects only?
anphe_model_fixed_only <- glm(infected_boolean ~ 
                                Offspring_sex + 
                                Non_infected_parent_colony + 
                                Transmission_mode,
                              data = df_offspring_anphe,
                              family = binomial) 

summary(anphe_model_fixed_only)

# what about if we include interactions?
anphe_model_with_interactions <- glm(infected_boolean ~ 
                                       Offspring_sex + 
                                       Non_infected_parent_colony +
                                       Transmission_mode +
                                       Offspring_sex:Non_infected_parent_colony +
                                       Offspring_sex:Transmission_mode +
                                       Non_infected_parent_colony:Transmission_mode,
                              data = df_offspring_anphe,
                              family = binomial) 

summary(anphe_model_with_interactions)


# is model including random effect signif better?
# see: http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#testing-significance-of-random-effects

# present model results in table

# supplemental table showing models with and without random effects
tab_model(anphe_model_with_random, anphe_model_fixed_only, anphe_model_with_interactions, show.aic = T) 

# compare models with anova
anova(anphe_model_with_random, anphe_model_fixed_only)
anova(anphe_model_with_interactions, anphe_model_fixed_only)

# conclusion: the more complicated models are not significantly better fitting: 
# prefer simpler model: use the fixed-only model
anphe_model <- anphe_model_fixed_only

# slight tangent: can use models to predict responses
# based on categorical explanatory variables
# do this to as sanity check on models
df_categories <- df_offspring %>% group_by(Infected_parent_colony, Non_infected_parent_colony, Virus_target, Transmission_mode, Offspring_sex ) %>% summarize(.groups="drop")
df_anphe_predict <- df_categories %>% filter(Virus_target == "Aedes Anphevirus")
anphe_pred <- predict(anphe_model_fixed_only, df_anphe_predict, type="response")
df_anphe_predict$predicted_response <- as.numeric(t(anphe_pred))
# this table includes the predicted prevalence in different groups as a prediction of model
df_anphe_predict

# results: 
# - offspring sex not signficant
# - maternal vs paternal significant
# - vergel vs. NO doesn't matter

# --------------------------------------------
# anphevirus horizontal transmission modeling
# --------------------------------------------

# we will assess HT efficiencies as represented by the fraction of
# cohabiting mosquitoes infected.  
# this does not consider Poza Rica or Tapachula because they were already infected
df_ht <- df %>% filter(Stage == "After cohabitating", !str_detect(Strain, "Poza|Tapachula")) %>% ungroup()

# make modeling variables factors
df_ht$Transmission_mode            <- as.factor(df_ht$Transmission_mode)
df_ht$Virus_target                 <- as.factor(df_ht$Virus_target)
df_ht$Offspring_sex                <- as.factor(df_ht$Sex)
df_ht$Infected_parent_colony       <- as.factor(df_ht$infected_parent_colony)
df_ht$Non_infected_parent_colony   <- as.factor(df_ht$non_infected_parent_colony)
df_ht$Cross_rep                    <- as.factor(df_ht$cross_rep)

# double check group sizes
df_ht %>% group_by(Non_infected_parent_colony, Infected_parent_colony, Virus_target) %>% summarize(n=n())

df_ht_m <- df_ht %>% select(infected_boolean, 
                            Offspring_sex, 
                            Infected_parent_colony, 
                            Non_infected_parent_colony, 
                            Virus_target, 
                            Cross_rep) 

# pull out anphevirus horizontal transmission data
df_ht_anphe <- filter(df_ht_m, Virus_target == "Aedes Anphevirus")

# model of anphevirus HT that includes replicate as a random effect
anphe_ht_model_with_random <- glmer(infected_boolean ~ 
                                      Offspring_sex + 
                                      Non_infected_parent_colony + 
                                      (1 | Cross_rep), 
                                    data = df_ht_anphe,
                                    family = binomial) 

summary(anphe_ht_model_with_random)

# model of anphevirus HT that does not include random effects
anphe_ht_model_fixed_only  <- glm(infected_boolean ~ 
                                    Offspring_sex + 
                                    Non_infected_parent_colony,  
                                  data = df_ht_anphe,
                                  family = binomial) 
summary(anphe_ht_model_fixed_only)

# model of anphevirus HT with interactions
anphe_ht_model_with_interactions  <- glm(infected_boolean ~ 
                                           Offspring_sex * 
                                           Non_infected_parent_colony,  
                                         data = df_ht_anphe,
                                         family = binomial) 
summary(anphe_ht_model_with_interactions)

tab_model(anphe_ht_model_with_random, anphe_ht_model_fixed_only, anphe_ht_model_with_interactions, show.aic = T) 

# compare models +/- random effect
# including replicate as a random effect or interaction terms does not produce better 
# fitting models (it's the opposite)
anova(anphe_ht_model_with_random, anphe_ht_model_fixed_only)
anova(anphe_ht_model_with_interactions, anphe_ht_model_fixed_only)

# use fixed effect only model: best fitting
anphe_ht_model <- anphe_ht_model_fixed_only

# output table showing anphevirus VT and HT model results
# to screen
tab_model(anphe_model, anphe_ht_model, 
          show.aic = T, 
          dv.labels = c("Vertical transmission", "Horizontal transmission"),
          pred.labels = c("(Intercept)", "Offspring or exposed parent sex [male]", "Non-infected parent colony [Vergel]", "Vertical transmission mode [paternal]"),
          title="Aedes anphevirus")

# to file
tab_model(anphe_model, anphe_ht_model, 
          show.aic = T, 
          dv.labels = c("Vertical transmission", "Horizontal transmission"),
          pred.labels = c("(Intercept)", "Offspring or exposed parent sex [male]", "Non-infected parent colony [Vergel]", "Vertical transmission mode [paternal]"),
          file=paste0(table_output_dir, "anphe_model_table.txt"))

# ------------------------
# Verdadero virus modeling
# ------------------------

df_offspring_verdadero <- filter(df_offspring_m, Virus_target == "Verdadero Virus")
verdadero_model_with_random <- glmer(infected_boolean ~ 
                                       Offspring_sex + 
                                       Non_infected_parent_colony + 
                                       Transmission_mode + 
                                       (1 | Cross_rep), 
                                     data = df_offspring_verdadero,
                                     family = binomial) 

summary(verdadero_model_with_random)

# got boundary (singular) fit which means random effects very small so omit from model
verdadero_model_fixed_only  <- glm(infected_boolean ~ 
                                       Offspring_sex + 
                                       Non_infected_parent_colony + 
                                       Transmission_mode,
                                     data = df_offspring_verdadero,
                                     family = binomial) 

summary(verdadero_model_fixed_only)

# model with interaction terms
verdadero_model_with_interactions  <- glm(infected_boolean ~ 
                                            Offspring_sex + 
                                            Non_infected_parent_colony + 
                                            Transmission_mode +
                                            Offspring_sex:Non_infected_parent_colony +
                                            Offspring_sex:Transmission_mode +
                                            Non_infected_parent_colony:Transmission_mode,
                                          data = df_offspring_verdadero,
                                          family = binomial) 

summary(verdadero_model_with_interactions)

# show models
tab_model(verdadero_model_with_random, verdadero_model_fixed_only, verdadero_model_with_interactions, show.aic = T) 

# models with and without random effect or interactions do not produce significantly better fits
anova(verdadero_model_with_random, verdadero_model_fixed_only)
anova(verdadero_model_with_interactions, verdadero_model_fixed_only)

# proceed with fixed effects only model
verdadero_model <- verdadero_model_fixed_only

# verdadero results: same as Aedes anphevirus:
#  - offspring sex doesn't matter
#  - Vergel vs. NO doesn't matter
#  - paternal vs. maternal matters (maternal better)

# pull out verdadero virus horizontal transmission data
df_ht_verdadero <- filter(df_ht_m, Virus_target == "Verdadero Virus")

# model of verdadero virus HT that includes replicate as a random effect
verdadero_ht_model_with_random <- glmer(infected_boolean ~ 
                                          Offspring_sex + 
                                          Non_infected_parent_colony + 
                                          (1 | Cross_rep), 
                                        data = df_ht_verdadero,
                                        family = binomial) 

summary(verdadero_ht_model_with_random)

# model of verdadero virus HT that does not include random effects
verdadero_ht_model_fixed_only  <- glm(infected_boolean ~ 
                                        Offspring_sex + 
                                        Non_infected_parent_colony,  
                                      data = df_ht_verdadero,
                                      family = binomial) 
summary(verdadero_ht_model_fixed_only)

# model of verdadero virus HT with interactions
verdadero_ht_model_with_interactions  <- glm(infected_boolean ~ 
                                               Offspring_sex * 
                                               Non_infected_parent_colony,  
                                             data = df_ht_verdadero,
                                             family = binomial) 
summary(verdadero_ht_model_with_interactions)

# output model info
tab_model(verdadero_ht_model_with_random, verdadero_ht_model_fixed_only, verdadero_ht_model_with_interactions, show.aic = T) 

# compare models +/- random effect
# including replicate as a random effect does not produce a better 
# fitting model (it's the opposite)
anova(verdadero_ht_model_with_random, verdadero_ht_model_fixed_only)
anova(verdadero_ht_model_with_interactions, verdadero_ht_model_fixed_only)

# use fixed effect only model: best fitting
verdadero_ht_model <- verdadero_ht_model_fixed_only

# output table showing verdadero virus VT and HT model results
# to screen
tab_model(verdadero_model, verdadero_ht_model, 
          show.aic = T, 
          dv.labels = c("Vertical transmission", "Horizontal transmission"),
          pred.labels = c("(Intercept)", "Offspring or exposed parent sex [male]", "Non-infected parent colony [Vergel]", "Vertical transmission mode [paternal]"),
          title="Verdadero virus")

# to file
tab_model(verdadero_model, verdadero_ht_model, 
          show.aic = T, 
          dv.labels = c("Vertical transmission", "Horizontal transmission"),
          pred.labels = c("(Intercept)", "Offspring or exposed parent sex [male]", "Non-infected parent colony [Vergel]", "Vertical transmission mode [paternal]"),
          file=paste0(table_output_dir, "verdadero_model_table.txt"))


# ---------------------------------- 
# Phasi-charoen-like virus
# ---------------------------------- 

# PCLV was detectable by RT-qPCR in a very small fraction of Tapachula (like 1 mosquito)
# and at very low RNA levels. Do not address further: It is not possible to conclude
# much about transmission efficiency given such low starting prevalences.

# ---------------------------------- 
# Guadeloupe mosquito virus modeling 
# ---------------------------------- 

# how many data points for each group?
df_offspring_m %>% group_by(Virus_target) %>% summarize(n=n())
# note GMV has more points: because 2 parental colonies infected

# get GMV VT data
df_offspring_gmv <- filter(df_offspring_m, Virus_target == "Guadeloupe Mosquito Virus")

# double check group sizes
df_offspring_gmv %>% group_by(Infected_parent_colony, Non_infected_parent_colony) %>% summarize(n=n())

# first model: as with verdadero and anphevirus, with random effects
# does not account for Poza Rica vs. Tapachula
gmv_model_1 <- glmer(infected_boolean ~ 
                       Offspring_sex + 
                       Non_infected_parent_colony + 
                       Transmission_mode + 
                       (1 | Cross_rep), 
                     data = df_offspring_gmv,
                     family = binomial) 

summary(gmv_model_1)

# now a model accounting for infected parent colony (Poza Rica vs Tapachula?)
gmv_model_2 <- glmer(infected_boolean ~ 
                       Offspring_sex + 
                       Infected_parent_colony +
                       Non_infected_parent_colony + 
                       Transmission_mode + 
                       (1 | Cross_rep), 
                     data = df_offspring_gmv,
                     family = binomial) 

# note: got boundary warning: random effect has variance near 0
summary(gmv_model_2)
 
# fixed effect only-model: got boundary fit warning so remove random effect
gmv_model_3 <- glm(infected_boolean ~ 
                     Offspring_sex + 
                     Infected_parent_colony +
                     Non_infected_parent_colony + 
                     Transmission_mode,
                   data = df_offspring_gmv,
                   family = binomial) 

summary(gmv_model_3)

# fixed effects, with interactions
gmv_model_4 <- glm(infected_boolean ~ 
                     Offspring_sex + 
                     Infected_parent_colony +
                     Non_infected_parent_colony + 
                     Transmission_mode +
                     Offspring_sex:Infected_parent_colony +
                     Offspring_sex:Non_infected_parent_colony +
                     Offspring_sex:Transmission_mode +
                     Infected_parent_colony:Non_infected_parent_colony +
                     Infected_parent_colony:Transmission_mode +
                     Non_infected_parent_colony:Transmission_mode,
                   data = df_offspring_gmv,
                   family = binomial) 

summary(gmv_model_4)

# compare models with anova
# first: models that do (#2) or do not (#1) account for Poza Rica vs Tapachula:
anova(gmv_model_1, gmv_model_2)
# gmv_model_2 outperforms gmv_model_1: so best to account for Poza Rica vs Tapachula

# second: random effects (#2) vs fixed only (#3)
anova(gmv_model_2, gmv_model_3)
# same: so use simpler model: fixed effects only

# third: fixex effects (#3) vs fixed with interaction terms (#4)
anova(gmv_model_4, gmv_model_3)
# same: so use simpler model: fixed effects only

# best model: gmv_model_3: fixed only, no interactions
gmv_model <- gmv_model_3

# ----------------------------
# GMV Horizontal Tranmission
# ----------------------------

# pull out GMV horizontal transmission data
df_ht_gmv <- filter(df_ht_m, Virus_target == "Guadeloupe Mosquito Virus")

# model of GMV HT that includes replicate as a random effect
gmv_ht_model_with_random <- glmer(infected_boolean ~ 
                                    Offspring_sex + 
                                    Infected_parent_colony +
                                    Non_infected_parent_colony + 
                                    (1 | Cross_rep), 
                                  data = df_ht_gmv,
                                  family = binomial) 

summary(gmv_ht_model_with_random)

# model of GMV virus HT that does not include random effects
gmv_ht_model_fixed_only  <- glm(infected_boolean ~ 
                                  Offspring_sex + 
                                  Infected_parent_colony +
                                  Non_infected_parent_colony,  
                                data = df_ht_gmv,
                                family = binomial) 
summary(gmv_ht_model_fixed_only)

# model of GMV virus HT with all 2-way interaction terms
gmv_ht_model_with_interactions  <- glm(infected_boolean ~ 
                                         Offspring_sex + 
                                         Infected_parent_colony +
                                         Non_infected_parent_colony +
                                         Offspring_sex:Infected_parent_colony +
                                         Offspring_sex:Non_infected_parent_colony +
                                         Infected_parent_colony:Non_infected_parent_colony ,  
                                       data = df_ht_gmv,
                                       family = binomial) 
summary(gmv_ht_model_with_interactions)

# model of GMV virus HT with fewer interaction terms
gmv_ht_model_with_some_interactions  <- glm(infected_boolean ~ 
                                         Offspring_sex + 
                                         Infected_parent_colony +
                                         Non_infected_parent_colony +
                                         Offspring_sex:Non_infected_parent_colony,
                                         # (1 | Cross_rep), 
                                       data = df_ht_gmv,
                                       family = binomial) 
summary(gmv_ht_model_with_some_interactions)

# show model info
tab_model(gmv_ht_model_with_random, gmv_ht_model_fixed_only, show.aic = T)
# models with interactions are producing crazy odds ratios: don't use
tab_model(gmv_ht_model_with_interactions, gmv_ht_model_with_some_interactions, show.aic = T)


# compare models +/- random effect
# including replicate as a random effect does produce a slightly better fit
anova(gmv_ht_model_with_random, gmv_ht_model_fixed_only)

# use model with random effect: better fitting
gmv_ht_model <- gmv_ht_model_with_random

# output table showing verdadero virus VT and HT model results
# to screen
tab_model(gmv_model, gmv_ht_model, 
          show.aic = T, 
          dv.labels = c("Vertical transmission", "Horizontal transmission"),
          pred.labels = c("(Intercept)", 
                          "Offspring or exposed parent sex [male]", 
                          "Main infected parent colony [Tapachula]", 
                          "Less infected parent colony [Vergel]", 
                          "Vertical transmission mode [paternal]"),
          show.icc     = T,
          show.re.var  = T,
          show.ngroups = T,
          title="Guadeloupe mosquito virus")

# to file
tab_model(gmv_model, gmv_ht_model, 
          show.aic = T, 
          dv.labels = c("Vertical transmission", "Horizontal transmission"),
          pred.labels = c("(Intercept)", 
                          "Offspring or exposed parent sex [male]", 
                          "Main infected parent colony [Tapachula]", 
                          "Less infected parent colony [Vergel]", 
                          "Vertical transmission mode [paternal]"),
          show.icc     = F,
          show.re.var  = F,
          show.ngroups = F,
          file=paste0(table_output_dir, "gmv_model_table.txt"))


# ************************************************************************************************
# -------------------------
# TEXT OUTPUT FOR PAPER
# -------------------------
# ************************************************************************************************

# ---------------------
# create text for paper 
# ---------------------

# this function returns text about what fraction of mosquitoes (and corresponding %) were infected
# by a particular virus
create_prev_text <- function (a_Name = NA, 
                              a_Name_RegEx = NA, 
                              a_Strain = NA, 
                              a_Strain_RegEx = NA, 
                              a_Sex = NA, 
                              a_Stage = NA, 
                              a_Transmission_mode = NA, 
                              a_Virus_target = NA) {
  
  df_to_use <- df_prevalence_by_group
  
  # progressivly filter the data
  if (!is.na(a_Name))              { df_to_use <- df_to_use %>% filter(Name == a_Name) }
  if (!is.na(a_Name_RegEx))        { df_to_use <- df_to_use %>% filter(str_detect(Name, a_Name_RegEx)) }
  if (!is.na(a_Strain))            { df_to_use <- df_to_use %>% filter(Strain == a_Strain) }
  if (!is.na(a_Strain_RegEx))      { df_to_use <- df_to_use %>% filter(str_detect(Strain, a_Strain_RegEx)) }
  if (!is.na(a_Sex))               { df_to_use <- df_to_use %>% filter(Sex == a_Sex) }
  if (!is.na(a_Stage))             { df_to_use <- df_to_use %>% filter(Stage == a_Stage) }
  if (!is.na(a_Transmission_mode)) { df_to_use <- df_to_use %>% filter(Transmission_mode == a_Transmission_mode) }
  if (!is.na(a_Virus_target))      { df_to_use <- df_to_use %>% filter(Virus_target == a_Virus_target) }
  
  
  # if we've filtered too stringently
  if (nrow(df_to_use) == 0) { 
    message("ERROR: overly filtered data (nrow == 0)")
    return ("")
  } 
  
  # after filtering, may still have multiple rows (multiple groups)
  # if they are all from the same virus, we can sum their observations
  # check that remaining rows all from same virus
  viruses_left <- nrow(df_to_use %>%  group_by(Virus_target) %>% summarize())
  if (viruses_left > 1) {
    message(paste0("ERROR: too many viruses after filtering (", viruses_left, ")") )
    print(df_to_use)
    return ("")
  }
  
  # group by whatever is remaining and summarize
  if (nrow(df_to_use) > 1) { 
    message(paste0("WARNING: summing filtered data from ", nrow(df_to_use), " rows." ))
    print(df_to_use)
    df_to_use <- df_to_use %>% 
      group_by(Virus_target) %>% 
      summarize(
        n_positive  = sum(n_positive),
        n           = sum(n))
  }
  
  n_pos <- df_to_use %>% pull(n_positive)
  n     <- df_to_use %>% pull(n)
  pct   <- n_pos * 100 / n
 
  # construct text for paper
  text <- paste0(n_pos, "/", n, " (", 
                 sprintf("%0.0f", pct),

                                  "%)")

  text
}

# test function
create_prev_text(a_Virus_target = "Aedes Anphevirus", 
                 a_Stage = "Offspring", 
                 a_Transmission_mode = "maternal",
                 a_Sex = "Male")

# create text related to RNA levels
create_levels_text <- function (a_Name = NA, 
                                a_Strain = NA, 
                                a_Strain_RegEx = NA,
                                a_Sex = NA, 
                                a_Stage = NA, 
                                a_Transmission_mode = NA, 
                                a_Virus_target = NA,
                                digits = 1) {
  
  df_to_use <- df_load_per_group
 
   
  # progressivly filter the data
  if (!is.na(a_Name))              { df_to_use <- df_to_use %>% filter(Name == a_Name) }
  if (!is.na(a_Strain))            { df_to_use <- df_to_use %>% filter(Strain == a_Strain) }
  if (!is.na(a_Strain_RegEx))      { df_to_use <- df_to_use %>% filter(str_detect(Strain, a_Strain_RegEx)) }
  if (!is.na(a_Sex))               { df_to_use <- df_to_use %>% filter(Sex == a_Sex) }
  if (!is.na(a_Stage))             { df_to_use <- df_to_use %>% filter(Stage == a_Stage) }
  if (!is.na(a_Transmission_mode)) { df_to_use <- df_to_use %>% filter(Transmission_mode == a_Transmission_mode) }
  if (!is.na(a_Virus_target))      { df_to_use <- df_to_use %>% filter(Virus_target == a_Virus_target) }
  
  # if we've filtered too stringently
  if (nrow(df_to_use) == 0) { 
    message("ERROR: overly filtered data (nrow == 0)")
    return ("")
  } 
  
  # after filtering, may still have multiple rows (multiple groups)
  # if they are all from the same virus, we can sum their observations
  # check that remaining rows all from same virus
  viruses_left <- nrow(df_to_use %>%  group_by(Virus_target) %>% summarize())
  if (viruses_left > 1) {
    message(paste0("ERROR: too many viruses after filtering (", viruses_left, ")") )
    print(df_to_use)
    return ("")
  }
  
  # group by whatever is remaining and summarize
  if (nrow(df_to_use) > 1) { 
    message(paste0("WARNING: summing filtered data from ", nrow(df_to_use), " rows." ))
    print(df_to_use)
    df_to_use <- df_to_use %>% 
      group_by(Virus_target) %>% 
      summarize(
        mean_load   = mean(mean_load))
  }
  
  mean_load   <- df_to_use %>% pull(mean_load)
 
  if (mean_load < 0.01) {
    text <- sci_notation(mean_load, 2)
  } else {
    format_text = paste0("%0.", digits, "f")
    text <- paste0( sprintf(format_text, mean_load) )
  }

  text
}

# test function
create_levels_text(a_Virus_target = "Aedes Anphevirus", 
                   a_Stage = "Offspring", 
                   a_Transmission_mode = "maternal",
                   a_Sex = "Male")


# ---------------------------
# Aedes anphevirus paper text
# ---------------------------
                            
anphe_text_parent <- 
  paste0("Nearly all tested adult mosquitoes in the Poza Rica colony were anphevirus-infected: ",
         create_prev_text(a_Sex = "Male", a_Strain = "Poza Rica", 
                          a_Virus_target = "Aedes Anphevirus", a_Stage = "Starting Prevalence"),
         " of males and ",
         create_prev_text(a_Sex = "Female", a_Strain = "Poza Rica", 
                          a_Virus_target = "Aedes Anphevirus", a_Stage = "Starting Prevalence"),
         " of females were positive by RT-qPCR (**[@fig-anphe]**).",
         " The mean level of anphevirus RNA in infected males and females was ",
         create_levels_text(a_Sex = "Male", a_Strain = "Poza Rica", 
                            a_Virus_target = "Aedes Anphevirus", a_Stage = "Starting Prevalence"),
         "x and ",
         create_levels_text(a_Sex = "Female", a_Strain = "Poza Rica", 
                            a_Virus_target = "Aedes Anphevirus", a_Stage = "Starting Prevalence"),
         "x the level of actin mRNA. ",
         "No mosquitoes in the New Orleans or Vergel colonies tested positive for AeAV by metagenomic sequencing or by RT-qPCR."
  )

anphe_text_parent


anphe_text_offspring <- 
  paste0("AeAV transmitted efficiently from infected mothers to offspring (**[@fig-anphe]**). ", 
         create_prev_text(a_Strain = "Hybrid",
                          a_Virus_target = "Aedes Anphevirus", a_Stage = "Offspring", 
                          a_Transmission_mode = "maternal"),
         " of offspring of Poza Rica mothers tested positive for anphevirus RNA. ",
         "Transmission from infected fathers was less efficient: ", 
         create_prev_text(a_Strain = "Hybrid",
                          a_Virus_target = "Aedes Anphevirus", a_Stage = "Offspring", 
                          a_Transmission_mode = "paternal"),
         " of offspring of infected fathers tested positive. ",
         "The mean level of anphevirus RNA in infected offspring was similar for maternal ",
         "and paternal transmission: ",
         create_levels_text(a_Strain = "Hybrid",
                            a_Virus_target = "Aedes Anphevirus", 
                            a_Stage = "Offspring",
                            a_Transmission_mode = "maternal"),
         "× and ",
         create_levels_text(a_Strain = "Hybrid",
                            a_Virus_target = "Aedes Anphevirus", 
                            a_Stage = "Offspring",
                            a_Transmission_mode = "paternal"),
         "× the level of actin mRNA, respectively. ",
         "Offspring exhibited a bimodal distribution of anphevirus RNA levels (**[@fig-anphe]**). ",
         "A subset of positive offspring had relatively high anphevirus RNA levels (higher than actin mRNA levels), ",
         "and a subset had levels ~1000× lower. ",
         "Transmission efficiencies were similar in crosses involving uninfected parents from the New Orleans and the Vergel colonies (**[@fig-anphe]**)."
  )

anphe_text_offspring

AeAV_vt_levels <- as.numeric(create_levels_text(a_Strain = "Hybrid",
                            a_Virus_target = "Aedes Anphevirus", 
                            a_Stage = "Offspring", 
                            digits = 4))
         
AeAV_ht_levels <- as.numeric(create_levels_text(a_Virus_target = "Aedes Anphevirus", 
                                     a_Strain_RegEx = "Orleans|Vergel",
                                     a_Stage = "After cohabitating",
                                     digits = 4))

AeAV_vt_hv_fold_diff <- AeAV_vt_levels/AeAV_ht_levels
         
anphe_text_horizontal <- 
  paste0("We tested previously uninfected parents after cohabitation and mating with infected parents ",
         "to assess possible horizontal transmission. ",
         create_prev_text(a_Sex = "Male", a_Strain_RegEx = "Orleans|Vergel",
                          a_Virus_target = "Aedes Anphevirus", a_Stage = "After cohabitating"),
         " of males and ",
         create_prev_text(a_Sex = "Female", a_Strain_RegEx = "Orleans|Vergel",
                          a_Virus_target = "Aedes Anphevirus", a_Stage = "After cohabitating"),
         " of females were positive by RT-qPCR following cohabitation with opposite sex infected mosquitoes (**[@fig-anphe]**). ",
         "However, levels of AeAV RNA in these mosquitoes were low, on average ",
         create_levels_text(a_Virus_target = "Aedes Anphevirus", 
                            a_Strain_RegEx = "Orleans|Vergel",
                            a_Stage = "After cohabitating",
                            digits = 2),
          "× levels of actin mRNA (",
         sprintf("%0.0f", AeAV_vt_hv_fold_diff),
          "× lower than average levels in infected offspring). ",
         
         "These positive signals may represent legitimate low-level infection following horizontal transmission ",
         "or could have resulted from cross-contamination of viral RNA during cohabitation or mating."
  )

anphe_text_horizontal

# extract p-value from best anphe model
summary(anphe_model)
coef(summary(anphe_model))
anphe_tm_pval <- coef(summary(anphe_model))[4,4]

summary(anphe_ht_model)
coef(summary(anphe_ht_model))
anphe_ep_pval <- coef(summary(anphe_ht_model))[2,4]

# creates markdown-formatted scientific notation text output
sci_notation <- function(value, digits) {
  # start with sprintf scientific notation
  t <- sprintf(paste0("%0.", digits, "e"), value)
  # replace zero padding in exponent
  t <- str_replace(t, "e0", "e")
  t <- str_replace(t, "e-0", "e-")
  t <- str_replace(t, "e\\+0", "e")
  # switch to markdown superscript
  t <- str_replace(t, "e", "×10^")
  t <- str_replace(t, "$", "^")
  t
}

anphe_text_vt_model <-  
  paste0("We used logistic regression to model anphevirus ",
         "transmission (**[@tbl-anphe-model]**). ",
         "Models for vertical transmission contained transmission mode (maternal vs. paternal), ",
         "uninfected parent colony (New Orleans vs Vergel), and offspring sex as fixed effects. ",
         "Models that included replicate as a random effect were not significantly better fitting. ",
         "For vertical transmission, transmission mode was the only significant predictor variable, ",
         "with offspring more likely to be infected via maternal transmission (p = ",
         sci_notation(anphe_tm_pval, digits = 1),
         " ; **[@tbl-anphe-model]**)."
)
anphe_text_vt_model

anphe_text_ht_model <-  paste0(
         "Models for horizontal transmission included exposed parent colony (New Orleans vs Vergel) ",
         "and sex as fixed effects. ",
         "For horizontal transmission, exposed parent sex was the only significant predictor, ",
         "with females more likely to test postive following exposure to infected fathers (p = ",
         sci_notation(anphe_ep_pval, digits = 1),
         " ; **[@tbl-anphe-model]**). ",
         "The higher prevalence in females may reflect exposure and possible transmission during mating."
)
anphe_text_ht_model

# write out to a file for inclusion in main paper qmd document
writeLines(c(anphe_text_parent, "\n", 
             anphe_text_offspring, "\n", 
             anphe_text_vt_model, "\n",
             anphe_text_horizontal, "\n", 
             anphe_text_ht_model), 
           con=paste0(text_output_dir, "anphevirus_text.txt"))


# ----------------
# Verdadero virus
# ----------------


verdadero_text_parent <- 
  paste0("Verdadero virus was at high prevalence in the Poza Rica parental population, ",
         "and undetectable in the New Orleans or Vergel mosquitoes (**[@fig-verd]**). ",
         create_prev_text(a_Sex = "Male", a_Strain = "Poza Rica", 
                          a_Virus_target = "Verdadero Virus", a_Stage = "Starting Prevalence"),
         " of Poza Rica males and ",
         create_prev_text(a_Sex = "Female", a_Strain = "Poza Rica", 
                          a_Virus_target = "Verdadero Virus", a_Stage = "Starting Prevalence"),
         " of females were positive by RT-qPCR (**[@fig-verd]**).",
         " The mean level of verdadero virus RNA 1 in infected males and females was ",
         create_levels_text(a_Sex = "Male", a_Strain = "Poza Rica", 
                            a_Virus_target = "Verdadero Virus", a_Stage = "Starting Prevalence"),
         "× and ",
         create_levels_text(a_Sex = "Female", a_Strain = "Poza Rica", 
                            a_Virus_target = "Verdadero Virus", a_Stage = "Starting Prevalence"),
         "× the level of actin mRNA, respectively. ",
         "Galbut virus, a related partitivirus that infects *Drosophila melanogaster*, also exhibits higher average RNA levels in infected males [@Cross_2020]."
  )

verdadero_text_parent

# pull out max and min verdadero virus RNA levels in offspring
verdadero_offspring <- df %>% filter(Virus_target == "Verdadero Virus", Strain == "Hybrid", Stage == "Offspring") 
max_verdadero_load <- verdadero_offspring %>% arrange(-Viral_Load) %>% head(1) %>% pull(Viral_Load)
min_verdadero_load <- verdadero_offspring %>% arrange( Viral_Load) %>% head(1) %>% pull(Viral_Load)

verdadero_text_offspring <- 
  paste0("Like anphevirus, verdadero virus exhibited ", 
         "biparental vertical transmission (**[@fig-verd]**). ",
         create_prev_text(a_Strain = "Hybrid",
                          a_Virus_target = "Verdadero Virus", a_Stage = "Offspring", 
                          a_Transmission_mode = "maternal"),
         " of offspring of Poza Rica mothers tested positive for verdadero virus RNA. ",
         "Paternal transmission was less efficient, with only ", 
         create_prev_text(a_Strain = "Hybrid",
                          a_Virus_target = "Verdadero Virus", a_Stage = "Offspring", 
                          a_Transmission_mode = "paternal"),
         " of offspring infected from Poza Rica fathers. ",
         "The mean level of anphevirus RNA in infected offspring was similar for maternal ",
         "and paternal transmission: ",
         create_levels_text(a_Strain = "Hybrid",
                            a_Virus_target = "Verdadero Virus", 
                            a_Stage = "Offspring",
                            a_Transmission_mode = "maternal"),
         "× and ",
         create_levels_text(a_Strain = "Hybrid",
                            a_Virus_target = "Verdadero Virus", 
                            a_Stage = "Offspring",
                            a_Transmission_mode = "paternal"),
         "× the level of actin mRNA, respectively. ",
         "Despite these similar averages, levels of verdadero virus RNA ",
         "in individual offspring spanned a broad range, from ",
         sci_notation(min_verdadero_load, digits = 1),
         " to ",
         sprintf("%0.0f", max_verdadero_load),
         "× the levels of actin mRNA."
  )

verdadero_text_offspring

verdadero_text_horizontal <- 
  paste0("Some New Orleans and Vergel parents tested positive for verdadero virus RNA following ",
         "cohabitation with opposite-sex Poza Rica parents (**[@fig-verd]**). ",
         create_prev_text(a_Strain_RegEx = "Orleans",
                          a_Virus_target = "Verdadero Virus", a_Stage = "After cohabitating"),
         " of New Orleans parents and ",
         create_prev_text(a_Strain_RegEx = "Vergel",
                          a_Virus_target = "Verdadero Virus", a_Stage = "After cohabitating"),
         " of vergel parents were positive following cohabitation (**[@fig-verd]**). ",
         "Verdadero virus RNA levels in these mosquitoes were low, on average ",
         create_levels_text(a_Virus_target = "Verdadero Virus", 
                            a_Strain_RegEx = "Orleans|Vergel",
                            a_Stage = "After cohabitating",
                            digits = 2),
         "× levels of actin mRNA. "
  )

verdadero_text_horizontal

# extract p-value from verdadero model
summary(verdadero_model)
coef(summary(verdadero_model))
verdadero_tm_pval <- coef(summary(verdadero_model))[4,4]

coef(summary(verdadero_ht_model))
verdadero_em_pval <- coef(summary(verdadero_ht_model))[2,4]
verdadero_ev_pval <- coef(summary(verdadero_ht_model))[3,4]

verdadero_text_model <-  
  paste0(
    "We modeled verdadero virus transmission as we had for AeAV (**[@tbl-verd-model]**). ",
    "As with anphevirus, paternal vs maternal transmission was the only ",
    "variable significantly predicting ",
    "verdadero virus prevalance in offspring (**[@tbl-verd-model]**), ",
    "with offspring more likely to be infected via maternal transmission (p = ",
    sci_notation(verdadero_tm_pval, 1),
    "). ",
    " In contrast to AeAV, male adults were more likely to test positive for verdadero virus following ",
    "exposure to infected Poza Rica females (p = ",
    sci_notation(verdadero_em_pval, 1),
    "). ",
    "Adults from the New Orleans colony were more likely to test positive ",
    "than those from the Vergel colony following cohabitation, indicating a ",
    "possible role for mosquito genotype or behavior in susceptibility to verdadero virus infection ",
    " (p = ",
    sci_notation(verdadero_ev_pval, 1),
    "; **[@fig-verd]**; **[@tbl-verd-model]**). "
)
verdadero_text_model


# write out to a file for inclusion in main paper qmd document
writeLines(c(verdadero_text_parent, "\n", 
             verdadero_text_offspring, "\n", 
             verdadero_text_horizontal, "\n", 
             verdadero_text_model), 
           con=paste0(text_output_dir, "verdadero_text.txt"))


# --------------------------
# Guadeloupe mosquito virus
# --------------------------

df_gmv_paternal <- df_prevalence_by_group %>% filter(Virus_target == "Guadeloupe Mosquito Virus" & Strain == "Hybrid" & Transmission_mode == "paternal")
df_gmv_paternal %>% summarize(total_n = sum(n),
                                    total_pos = sum(n_positive))


gmv_text_parent <- 
  paste0(
    "All tested Poza Rica adults were positive for GMV RNA, ",
    "and RNA levels were very high in individual mosquitoes: ",
    create_levels_text(a_Strain = "Poza Rica",  a_Virus_target = "Guadeloupe Mosquito Virus", a_Stage = "Starting Prevalence"),
    "× the level of actin mRNA on average ",
    "(**[@fig-gmv]**).  ",
    "Tapachula adults were also mostly infected: ",
    create_prev_text(a_Strain = "Tapachula",  a_Virus_target = "Guadeloupe Mosquito Virus", a_Stage = "Starting Prevalence"),
    " tested positive (**[@suppfig-gmv]**). ",
    "But GMV RNA levels were more variable in the Tapachula adults, ",
    "with substantially lower levels in some positive mosquitoes  (**[@suppfig-gmv]**). ",
    "Unexpectedly, some New Orleans and Vergel adults tested positive for GMV: ",
    create_prev_text(a_Strain = "New Orleans",  a_Virus_target = "Guadeloupe Mosquito Virus", a_Stage = "Starting Prevalence"),
    " and ",
    create_prev_text(a_Strain = "Vergel",  a_Virus_target = "Guadeloupe Mosquito Virus", a_Stage = "Starting Prevalence"),
    ", respectively. ",
    "This was unexpected because no GMV-mapping reads were detected by metagenomics in these colonies. ",
    "The lack of detection of GMV in these colonies by metagenomic likely reflects ",
    "the low RNA levels in infected mosquitoes (**[@fig-gmv]**). ",
    "We performed qPCR without reverse transcription to confirm that low-level detection did not ",
    "result from transcription of a GMV-like endogenized viral sequence [@Palatini_2017; @Whitfield_2017]. ",
    "We confirmed the identity of the GMV PCR product by melting temperature, ",
    "agarose gel electropheresis, and Sanger sequencing [Tillie, did you do this?]. ",
    "The presence of GMV in all parental populations meant that it was not possible to ",
    "definitively determine whether infection ",
    "in offspring derived from maternal or paternal transmission. ",
    "However, the much more highly infected Poza Rica and Tapachula parents were the likely source of infection."
  )
gmv_text_parent


gmv_text_offspring <-  paste0(
  "Transmission of GMV from infected Poza Rica parents was efficient. ",
  create_prev_text(a_Strain = "Hybrid", 
                   a_Name_RegEx = "Poza",
                   a_Virus_target = "Guadeloupe Mosquito Virus", 
                   a_Transmission_mode = "maternal",
                   a_Stage = "Offspring"),
  " and ",
  create_prev_text(a_Strain = "Hybrid", 
                   a_Name_RegEx = "Poza",
                   a_Virus_target = "Guadeloupe Mosquito Virus", 
                   a_Transmission_mode = "paternal",
                   a_Stage = "Offspring"),
  " of offspring of Poza Rica mothers and fathers tested positive for GMV RNA. ",
  "In both cases infected offspring exhibited a wide range ",
  "of GMV RNA levels (**[@fig-gmv]**). ",
  "GMV was at lower prevalence and RNA levels in parental Tapachula mosquitoes, ",
  "and, accordingly, fewer offspring of Tapachula parents were infected. ",
  create_prev_text(a_Strain = "Hybrid", 
                   a_Name_RegEx = "Tapachula",
                   a_Virus_target = "Guadeloupe Mosquito Virus", 
                   a_Transmission_mode = "maternal",
                   a_Stage = "Offspring"),
  " and ",
  create_prev_text(a_Strain = "Hybrid", 
                   a_Name_RegEx = "Tapachula",
                   a_Virus_target = "Guadeloupe Mosquito Virus", 
                   a_Transmission_mode = "paternal",
                   a_Stage = "Offspring"),
  " offspring of Tapachula mothers and fathers were positive for GMV, respectively (**[@suppfig-gmv]**). "
)

gmv_text_offspring

# GMV model p-values
coef(summary(gmv_model))
gmv_vt_ip_pval <- coef(summary(gmv_model))[3,4]
gmv_vt_tm_pval <- coef(summary(gmv_model))[5,4]

gmv_text_model <- paste0(
  "We modeled GMV transmission as we had for other viruses (**[@tbl-gmv-model]**). ",
  "GMV models included an additional term corresponding to the highly-infected parental colony: ",
  "Poza Rica vs. Tapachula. ",
  "This term captured possible differences in parental host genetics and ",
  "differences in starting prevalence and viral loads between these colonies (**[@fig-gmv]**, **[@suppfig-gmv]**). ",
  "In the vertical transmission model, transmission from highly infected mothers was more efficient (p = ",
  sci_notation(gmv_vt_ip_pval, 1),
  "), and ",
  "transmission from Poza Rica crosses was more efficient than Tapachula crosses, ",
  "likely reflecting higher GMV starting prevalence and RNA levels in the Poza Rica population (p = ",
  sci_notation(gmv_vt_tm_pval, 1),
  ")."
)
gmv_text_model

coef(summary(gmv_ht_model))
gmv_ht_es_pval <- coef(summary(gmv_ht_model))[2,4]
gmv_ht_ip_pval <- coef(summary(gmv_ht_model))[3,4]

gmv_text_ht <- paste0(
  "The impact of cohabitation and mating on GMV prevalence in New Orleans and Vergel ",
  "adults was variable (**[@fig-gmv]**, **[@suppfig-gmv]**). ",
  "The GMV horizontal transmission model that included replicate as a random effect ",
  "was better fitting ",
  "than the fixed effect only model, reflecting increased variance in ",
  "prevalence between replicate crosses (**[@fig-gmv]**, **[@suppfig-gmv]**). ",
  "New Orleans and Vergel females were more likely to test positive for GMV following ",
  "cohabitation, again reflecting sexual transmission as a candidate route of exposure ",
  "(**[@tbl-gmv-model]**; p = ",
  sci_notation(gmv_ht_es_pval, 1),
  "). ",
  "Cohabitaing mosquitoes were also more likely to test positive following exposure to ",
  "Poza Rica adults ", 
  "(**[@tbl-gmv-model]**; p = ",
  sci_notation(gmv_ht_ip_pval, 1),
  "). "
)

gmv_text_ht

# write out to a file for inclusion in main paper qmd document
writeLines(c(
  gmv_text_parent, "\n", 
  gmv_text_offspring, "\n",
  gmv_text_model, "\n",
  gmv_text_ht, "\n"),
  con=paste0(text_output_dir, "gmv_text.txt"))



# ************************************************************************************************
# ------------
# PLOTTING 
# ------------
# ************************************************************************************************

#Colors (Color corresponds to virus and shade corresponds to sex:dark = male, light = female)
#AeAv
dark_teal <- "#006566"
light_teal <- "#5BD0CF"
#VeDV
dark_maroon <- "#5E0006"
light_maroon <- "#D64A61"
#GMV
dark_purple <- "#4B4082"
light_purple <- "#B5A4EF"
#PCLV
dark_green <- "#003500"
light_green <- "#499A39"


create_prev_plot <- function (a_Name = NA, 
                              a_Strain = NA, 
                              a_Sex = NA, 
                              a_Stage = NA, 
                              a_Transmission_mode = NA, 
                              a_Virus_target = NA,
                              facet_by = NA,
                              male_color = "blue",
                              female_color = "lightblue") {
  
  df_to_plot <- df_prevalence_by_group
  
  # progressivly filter the data
  if (!is.na(a_Name))              { df_to_plot <- df_to_plot %>% filter(Name == a_Name) }
  if (!is.na(a_Strain))            { df_to_plot <- df_to_plot %>% filter(Strain == a_Strain) }
  if (!is.na(a_Sex))               { df_to_plot <- df_to_plot %>% filter(Sex == a_Sex) }
  if (!is.na(a_Stage))             { df_to_plot <- df_to_plot %>% filter(Stage == a_Stage) }
  if (!is.na(a_Transmission_mode)) { df_to_plot <- df_to_plot %>% filter(Transmission_mode == a_Transmission_mode) }
  if (!is.na(a_Virus_target))      { df_to_plot <- df_to_plot %>% filter(Virus_target == a_Virus_target) }
  
  p <- ggplot(df_to_plot) +
    geom_point(aes(x = Sex, y = percent_positive, fill = Sex), shape = 21, size = 3, stroke = 0.25) +
    geom_errorbar(aes(x = Sex, ymin = lower_conf_int, ymax = upper_conf_int, color = Sex), width = 0.1) +
    scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +
    scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +
    theme_bw(base_size = 13) +
    ylim(c(0, 100)) +
    xlab("") +
    ylab("") +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  if (!is.na(facet_by)) {
    p <- p + facet_wrap(vars(.data[[facet_by]]))
  }
  
  
  
  p
}


create_levels_plot <- function (a_Name = NA, 
                                a_Strain = NA, 
                                a_Sex = NA, 
                                a_Stage = NA, 
                                a_Transmission_mode = NA, 
                                a_Virus_target = NA,
                                facet_by = NA,
                                ymin = y_axis_min,
                                ymax = y_axis_max,
                                male_color = "blue",
                                female_color = "lightblue") {
  
  df_to_plot <- df
  
  # progressivly filter the data
  if (!is.na(a_Name))              { df_to_plot <- df_to_plot %>% filter(Name == a_Name) }
  if (!is.na(a_Strain))            { df_to_plot <- df_to_plot %>% filter(Strain == a_Strain) }
  if (!is.na(a_Sex))               { df_to_plot <- df_to_plot %>% filter(Sex == a_Sex) }
  if (!is.na(a_Stage))             { df_to_plot <- df_to_plot %>% filter(Stage == a_Stage) }
  if (!is.na(a_Transmission_mode)) { df_to_plot <- df_to_plot %>% filter(Transmission_mode == a_Transmission_mode) }
  if (!is.na(a_Virus_target))      { df_to_plot <- df_to_plot %>% filter(Virus_target == a_Virus_target) }
  
  p <- ggplot() +
    geom_jitter(data = filter(df_to_plot, Infected == "Positive"), 
                aes(x = Sex, y = Viral_Load, fill = Sex), shape = 21, size = 3, stroke = 0.25, height = 0, width = 0.1) +
    geom_jitter(data = filter(df_to_plot, Infected != "Positive"), 
                aes(x = Sex, y = Viral_Load), shape = 21, size = 3, fill = "grey90", alpha = 0.5, stroke = 0.25, height = 0, width = 0.1) +
    scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +
    theme_bw(base_size = 13) +
    scale_y_log10(limits = c(ymin, ymax)) +
    xlab("") +
    ylab("") +
    theme(legend.position = "none",
          strip.text = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y = element_blank())
  
  if (!is.na(facet_by)) {
    p <- p + facet_wrap(vars(.data[[facet_by]]))
  }
  
  p
}




make_one_big_plot <- function(Parent1 = "Poza Rica", Parent2 = "New Orleans", Virus = "Aedes Anphevirus", male_color = "blue", female_color = "lightblue") {
  
  
  # -----------------
  # Prevalence plots 
  # -----------------
  # Parent #1
  p_p1 <- create_prev_plot(paste0(Parent1, " Starting Prevalence"), Parent1, NA, "Starting Prevalence", "starting prevalence", Virus, male_color = male_color, female_color = female_color)
  # Parent  #2
  p_p2 <- create_prev_plot(paste0(Parent2, " Starting Prevalence"), Parent2, NA, "Starting Prevalence", "starting prevalence", Virus, male_color = male_color, female_color = female_color)
  # offspring maternal
  p_om <- create_prev_plot(paste0(Parent1, " F × ", Parent2, " M"), "Hybrid", NA, "Offspring", "maternal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  # offspring paternal
  p_op <- create_prev_plot(paste0(Parent1, " M × ", Parent2, " F"), "Hybrid", NA, "Offspring", "paternal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  
  # horizontal - female
  p_hf  <- create_prev_plot(paste0(Parent2, " F × ", Parent1, " M"), NA, "Female", "After cohabitating", "Horizontal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  # horizontal - male
  p_hm  <- create_prev_plot(paste0(Parent1, " F × ", Parent2, " M"), NA, "Male",   "After cohabitating", "Horizontal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  
  # -----------------
  # Viral load plots
  # -----------------
  # Parent #1
  l_p1 <- create_levels_plot(paste0(Parent1, " Starting Prevalence"), Parent1, NA, "Starting Prevalence", "starting prevalence", Virus, male_color = male_color, female_color = female_color)
  # Parent  #2
  l_p2 <- create_levels_plot(paste0(Parent2, " Starting Prevalence"), Parent2, NA, "Starting Prevalence", "starting prevalence", Virus, male_color = male_color, female_color = female_color)
  # offspring maternal
  l_om <- create_levels_plot(paste0(Parent1, " F × ", Parent2, " M"), "Hybrid", NA, "Offspring", "maternal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  # offspring paternal
  l_op <- create_levels_plot(paste0(Parent1, " M × ", Parent2, " F"), "Hybrid", NA, "Offspring", "paternal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  
  # horizontal - female
  l_hf  <- create_levels_plot(paste0(Parent2, " F × ", Parent1, " M"), NA, "Female", "After cohabitating", "Horizontal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  # horizontal - male
  l_hm  <- create_levels_plot(paste0(Parent1, " F × ", Parent2, " M"), NA, "Male",   "After cohabitating", "Horizontal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  
  # add labels to left-most plots
  p_p1 <- p_p1 + ylab("Prevalence (% Positive Individuals)") + theme(axis.text.y = element_text())
  l_p1 <- l_p1 + ylab("Viral RNA Levels in Infected Individuals\n(Viral RNA Relative to Actin RNA)") + theme(axis.text.y = element_text())
  
  # make a big plot with patchwork
  big_p <- ((p_p1 | p_p2 | p_om | p_op | p_hf | p_hm) / (l_p1 | l_p2 | l_om | l_op | l_hf | l_hm)) 
  
  # save as PDF
  plot_filename <- paste0(Parent1, "_x_", Parent2, "_", Virus, "_without_full_annotation.pdf")
  ggsave(filename = plot_filename, plot = big_p,  width=12, height=8, units="in")
  
  big_p
}


# make big plots
make_one_big_plot("Poza Rica", "New Orleans", "Aedes Anphevirus", dark_teal, light_teal)
make_one_big_plot("Poza Rica", "Vergel",      "Aedes Anphevirus", dark_teal, light_teal)

make_one_big_plot("Poza Rica", "New Orleans", "Guadeloupe Mosquito Virus", dark_purple, light_purple)
make_one_big_plot("Poza Rica", "Vergel",      "Guadeloupe Mosquito Virus", dark_purple, light_purple)

make_one_big_plot("Poza Rica", "New Orleans", "Verdadero Virus", dark_maroon, light_maroon)
make_one_big_plot("Poza Rica", "Vergel",      "Verdadero Virus", dark_maroon, light_maroon)

make_one_big_plot("Tapachula", "New Orleans", "Guadeloupe Mosquito Virus", dark_purple, light_purple)
make_one_big_plot("Tapachula", "Vergel", "Guadeloupe Mosquito Virus", dark_purple, light_purple)

make_one_big_plot("Tapachula", "New Orleans", "Phasi Charoen-like Virus", dark_green, light_green)
make_one_big_plot("Tapachula", "Vergel", "Phasi Charoen-like Virus", dark_green, light_green)


# -----------------------------------------
# are infections in offspring independent?
# -----------------------------------------


# make a wider 
df_offspring_wider <- 
  df_offspring %>% 
  pivot_wider(id_cols=c(Transmission_mode, Maternal_colony, Paternal_colony, Replicate, Sex, mosquito_number),
              names_from = Virus_target,
              values_from = c(Viral_Load)) 

# replace spaces in column names with underscores
colnames(df_offspring_wider) <- str_replace_all(colnames(df_offspring_wider), " ", "_")


# subset datasets for plotting correlation of infection in offspring of multiply-infected parents
df_offspring_pat  <-  filter(df_offspring_wider,  Transmission_mode == "paternal" & Paternal_colony == "Poza Rica")
df_offspring_mat  <-  filter(df_offspring_wider,  Transmission_mode == "maternal" & Maternal_colony == "Poza Rica")


# Paternal correlations
a_v_p_ct <-
cor.test(log10(df_offspring_pat$Aedes_Anphevirus), 
         log10(df_offspring_pat$Verdadero_Virus), 
         method="spearman", exact=F)

a_g_p_ct <-
cor.test(log10(df_offspring_pat$Aedes_Anphevirus), 
         log10(df_offspring_pat$Guadeloupe_Mosquito_Virus), 
         method="spearman", exact=F)

v_g_p_ct <-
cor.test(log10(df_offspring_pat$Verdadero_Virus), 
         log10(df_offspring_pat$Guadeloupe_Mosquito_Virus), 
         method="spearman", exact=F)

v_g_p_ct

# Maternal correlations
a_v_m_ct <-
cor.test(log10(df_offspring_mat$Aedes_Anphevirus), 
         log10(df_offspring_mat$Verdadero_Virus), 
         method="spearman", exact=F)

a_g_m_ct <-
cor.test(log10(df_offspring_mat$Aedes_Anphevirus), 
         log10(df_offspring_mat$Guadeloupe_Mosquito_Virus), 
         method="spearman", exact=F)

v_g_m_ct <-
cor.test(log10(df_offspring_mat$Verdadero_Virus), 
         log10(df_offspring_mat$Guadeloupe_Mosquito_Virus), 
         method="spearman", exact=F)


correlation_stats_table <-
  tibble(comparison = c("Anphevirus-Verdadero", "Anphevirus-GMV", "Veradero-GMV"),
       rho_paternal = c(
         a_v_p_ct$estimate,
         a_g_p_ct$estimate,
         v_g_p_ct$estimate),
       p_paternal = c( 
         a_v_p_ct$p.value,
         a_g_p_ct$p.value,
         v_g_p_ct$p.value),
       rho_maternal = c(
         a_v_m_ct$estimate,
         a_g_m_ct$estimate,
         v_g_m_ct$estimate),
       p_maternal = c( 
         a_v_m_ct$p.value,
         a_g_m_ct$p.value,
         v_g_m_ct$p.value)))

write.table(correlation_stats_table, file="correlation_stats_table.csv", 
            sep=",", row.names = F, col.names = T, quote = F)


# what about statistical independence?
# chi-squared test

# make contingency table
df_infected_by_group <-
  df_offspring %>%
  group_by(Transmission_mode, Maternal_colony, Paternal_colony, Replicate, Virus_target, Infected) %>%
  summarize(n=n()) 


df_total_infected_counts <- 
  df_infected_by_group  %>% 
  filter(Transmission_mode == "paternal" & Paternal_colony == "Poza Rica")  %>%
  group_by(Virus_target, Infected) %>%
  summarize(total_infected = sum(n), .groups="drop")

# contingency matrix
df_paternal_transmission_contingency_matrix <- 
  df_total_infected_counts %>% pivot_wider(names_from = Infected, values_from = total_infected)

rn <- df_paternal_transmission_contingency_matrix$Virus_target
df_paternal_transmission_contingency_matrix <- as.matrix(df_paternal_transmission_contingency_matrix %>% select(-Virus_target))
row.names(df_paternal_transmission_contingency_matrix) <- rn

df_paternal_transmission_contingency_matrix 

chisq.test(df_paternal_transmission_contingency_matrix[])

# make a wider 

df_offspring_wider_status <- 
  df_offspring %>% 
  pivot_wider(id_cols=c(Transmission_mode, Maternal_colony, Paternal_colony, Replicate, Sex, mosquito_number),
              names_from = Virus_target,
              values_from = c(Infected)) 

write.table(df_offspring_wider_status, file="cm.tsv", sep="\t", row.names=F, quote = F)

# replace spaces in column names with underscores
colnames(df_offspring_wider_status) <- str_replace_all(colnames(df_offspring_wider_status), " ", "_")


# this function creates a correlation plot between viral RNA levels in offspring
make_correlation_plot <- function(df_to_plot, 
                                  x_var,
                                  y_var,
                                  fill_var,
                                  x_lab, 
                                  y_lab) {
  
  x_var_sym    <- sym(x_var)
  y_var_sym    <- sym(y_var)
  fill_var_sym <- sym(fill_var)
  
  ggplot(df_to_plot) +
    geom_point(aes(x=log10(!!x_var_sym),
                   y=log10(!!y_var_sym),
                   fill=!!fill_var_sym),
               shape=21, color="black", size=2, stroke=0.25) +
    theme_bw(base_size = 8) +
    # scale_y_log10(limits=c(1e-5, 1e3)) +
    # scale_x_log10(limits=c(1e-5, 1e3)) +
    xlim(c(-5, 3)) +
    ylim(c(-5, 3)) +
    scale_fill_manual(values=c("#BB2649", "#6667AB")) +
    xlab(x_lab) +
    ylab(y_lab) + 
    coord_fixed() +
    theme(legend.position = "none")
}

# paternal transmission of Anphevirus and Verdadero virus
a_v_p_cp <-
make_correlation_plot(df_offspring_pat,
                      "Aedes_Anphevirus",
                      "Verdadero_Virus",
                      "Maternal_colony",
                      "Aedes anphevirus RNA",
                      "Verdadero virus RNA") 
a_v_p_cp 

# paternal transmission of Anphevirus and GMV 
a_g_p_cp <-
make_correlation_plot(df_offspring_pat,
                      "Aedes_Anphevirus",
                      "Guadeloupe_Mosquito_Virus",
                      "Maternal_colony",
                      "Aedes anphevirus RNA", 
                      "Guadeloupe mosq. virus RNA")
a_g_p_cp

# paternal transmission of Verdadero virus and GMV 
v_g_p_cp <-
make_correlation_plot(df_offspring_pat,
                      "Verdadero_Virus",
                      "Guadeloupe_Mosquito_Virus",
                      "Maternal_colony",
                      "Verdadero virus RNA",
                      "Guadeloupe mosq. virus RNA") 
v_g_p_cp 

# add a legend to right-most plot
v_g_p_cp  <-  v_g_p_cp  + theme(legend.position = "right")

a_v_p_cp +  a_g_p_cp + v_g_p_cp + plot_layout(nrow = 1)
ggsave("paternal_transmission_correlation_plot.pdf", units="in", width=7.5, height=5)

# ---------------------
# Maternal transmission
# ---------------------

# maternal transmission of Anphevirus and Verdadero virus
a_v_m_cp <-
make_correlation_plot(df_offspring_mat,
                      "Aedes_Anphevirus",
                      "Verdadero_Virus",
                      "Paternal_colony",
                      "Aedes anphevirus RNA",
                      "Verdadero virus RNA") 
a_v_m_cp 

# maternal transmission of Anphevirus and GMV 
a_g_m_cp <-
make_correlation_plot(df_offspring_mat,
                      "Aedes_Anphevirus",
                      "Guadeloupe_Mosquito_Virus",
                      "Paternal_colony",
                      "Aedes anphevirus RNA", 
                      "Guadeloupe mosq. virus RNA")
a_g_m_cp

# maternal transmission of Verdadero virus and GMV 
v_g_m_cp <-
make_correlation_plot(df_offspring_mat,
                      "Verdadero_Virus",
                      "Guadeloupe_Mosquito_Virus",
                      "Paternal_colony",
                      "Verdadero virus RNA",
                      "Guadeloupe mosq. virus RNA") 
v_g_m_cp 

# add a legend to right-most plot
v_g_m_cp  <-  v_g_m_cp  + theme(legend.position = "right")

a_v_m_cp +  a_g_m_cp + v_g_m_cp + plot_layout(nrow = 1)
ggsave("maternal_transmission_correlation_plot.pdf", units="in", width=7.5, height=5)
