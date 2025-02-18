library(tidyverse)
library(patchwork)
library(readxl)
library(binom)
library(lme4)
library(sjPlot)
library(ResourceSelection)


# This script process, analyzes, and plots data
# from Aedes aegypti ISV VT experiments
# 
# TJD, MDS, 2024


# create an output file and text variable to capture text output from this analysis
output_dir  <- "./"
output_file <-file(paste0(output_dir, "mismatch_text.txt"))
output_text <- ""


# ************************************************************************************************
# ------------------------
# DATE INPUT / WRANGLING
# ------------------------
# ************************************************************************************************

# read in all data
df <- read_excel("all_data.xlsx")

# make a boolean version of infected (character)
df <- df %>% mutate(infected_boolean = if_else(Infected == "Positive", T, F))

# what was infected parent genotype?
df <- df %>% mutate(infected_parent_genotype = case_when(
  Transmission_mode == "maternal" ~ Maternal_colony ,
  Transmission_mode == "paternal" ~ Paternal_colony,
  .default = NA_character_
))

# what was non-infected parent genotype?
df <- df %>% mutate(non_infected_parent_genotype = case_when(
  Transmission_mode == "maternal" ~ Paternal_colony ,
  Transmission_mode == "paternal" ~ Maternal_colony,
  .default = NA_character_
))

# number the crosses: 1 -> whatever
cross_numbers <- df %>% group_by(Name) %>% summarize() %>% mutate(cross_number = row_number())
df            <- left_join(df, cross_numbers)
# make a new grouping random effect variable: cross x replication
df            <- df %>% mutate(cross_rep = paste0(cross_number, "_", Replicate)) 

# how many crosses were there?
number_crosses <- nrow(df %>% group_by(cross_rep) %>% summarize)

output_text <-
  paste0(output_text,
         "We performed ", number_crosses, " crosses.\n\n")
  
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
df_offspring <- df %>% filter(Stage == "Offspring")

# only keep Poza Rica crosses for now
# df_offspring_poza_rica <- df_offspring %>% filter(infected_parent_genotype == "Poza Rica")

# make modeling variables factors
df_offspring$infected_parent_genotype     <- as.factor(df_offspring$infected_parent_genotype)
df_offspring$non_infected_parent_genotype <- as.factor(df_offspring$non_infected_parent_genotype)
df_offspring$Transmission_mode            <- as.factor(df_offspring$Transmission_mode)
df_offspring$Virus_target                 <- as.factor(df_offspring$Virus_target)
df_offspring$Sex                          <- as.factor(df_offspring$Sex)
df_offspring$cross_rep                    <- as.factor(df_offspring$cross_rep)

df_offspring_m <- df_offspring %>% select(infected_boolean, 
                                          Sex, 
                                          Transmission_mode, 
                                          infected_parent_genotype, 
                                          non_infected_parent_genotype, 
                                          Virus_target, 
                                          cross_rep)

# everything model
# estimate the model and store results in m
full_model <- glmer(infected_boolean ~ 
                      Virus_target + 
                      Sex + 
                      infected_parent_genotype + 
                      non_infected_parent_genotype + 
                      Transmission_mode + 
                      (1 | cross_rep), 
                    data = df_offspring_m, 
                    family = binomial) 
                    # family = binomial, 
                    # control = glmerControl(optimizer = "bobyqa"),
                    # nAGQ = 10)

summary(full_model)

# significant:
# virus 
# infected parent genotype (starting prev.?)
# transmission mode 

# what if we do it one virus at a time?

df_offspring_anphe <- filter(df_offspring_m, Virus_target == "Aedes Anphevirus")
anphe_model <- glmer(infected_boolean ~ 
                       Sex + 
                       non_infected_parent_genotype + 
                       Transmission_mode + 
                       (1 | cross_rep), 
                     data = df_offspring_anphe,
                     family = binomial) 

summary(anphe_model)

# anphe-only model, significant: 
# paternal vs. maternal 
# 
# not significant:
# sex of offspring
# non-infected paternal genotype  

# what about fixed effects only?
anphe_model_fixed_only <- glm(infected_boolean ~ 
                                  Sex + 
                                  non_infected_parent_genotype + 
                                  Transmission_mode,
                                data = df_offspring_anphe,
                                family = binomial) 

summary(anphe_model_fixed_only)

# try to identify best model by possibly dropping variables
# see: https://statsandr.com/blog/binary-logistic-regression-in-r/

# save initial model
anphe_initial <- glm(infected_boolean ~ (Sex + non_infected_parent_genotype + Transmission_mode)^2,
                     data = df_offspring_anphe,
                     family = "binomial")

# select best model according to AIC using mixed selection
anphe_final <- step(anphe_initial,
                    direction = "both", # both = mixed selection
                    trace = FALSE # do not display intermediate steps
)
summary(anphe_final)
summary(anphe_model_fixed_only)

tab_model(anphe_final, anphe_model_fixed_only, anphe_model,
          show.ci = FALSE , # show CI
          show.aic = TRUE, # display AIC
          p.style = "scientific" # display p-values and stars
)


# is model including random effect signif better?
# see: http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#testing-significance-of-random-effects
anova(anphe_model, anphe_model_fixed_only)

methods_text <- 
  paste0("We modeled transmission from parents to offspring using bimonial logistic regression and ",
         "generalized linear models using the lme4 R package [REF: doi:10.18637/jss.v067.i01. ]. ",
         "Models took the form: infected ~ sex + non_infected_parent_genotype + transmission_mode + (1|replicate). ",
         "Infected was a boolean indicating whether individual mosquitoes were qRT-PCR positive. ",
         "Fixed effects included sex of tested offspring, ",
         "the parental genotype (e.g. New Orleans or Vergel), ",
         "and paternal or maternal transmission_mode. ",
         "Models included individual replicates as a random effect. ",
         "R code for modeling is provided in the linked github repository.")

methods_text

# can use models to predict responses
# based on categorical explanatory variables
# do this to as sanity check on models
df_categories <- df_offspring %>% group_by(infected_parent_genotype, non_infected_parent_genotype, Virus_target, Transmission_mode, Sex ) %>% summarize(.groups="drop")
df_anphe_predict <- df_categories %>% filter(Virus_target == "Aedes Anphevirus")
anphe_pred <- predict(anphe_model_fixed_only, df_anphe_predict, type="response")
df_anphe_predict$predicted_response <- as.numeric(t(anphe_pred))
df_anphe_predict

# results: 
# - offspring sex not signficant
# - maternal vs paternal significant
# - vergel vs. NO doesn't matter


df_offspring_verdadero <- filter(df_offspring_m, Virus_target == "Verdadero Virus")
verdadero_model <- glmer(infected_boolean ~ 
                           Sex + 
                           non_infected_parent_genotype + 
                           Transmission_mode + 
                           (1 | cross_rep), 
                         data = df_offspring_verdadero,
                         family = binomial) 

summary(verdadero_model)

# got boundary (singular) fit which means random effects very small so omit from model
verdadero_model_fixed_only  <- glm(infected_boolean ~ 
                                       Sex + 
                                       non_infected_parent_genotype + 
                                       Transmission_mode,
                                     data = df_offspring_verdadero,
                                     family = binomial) 

summary(verdadero_model_fixed_only)

# models with and without random effect produce same log likelihood
# so including random effect doesn't improve things...
anova(verdadero_model, verdadero_model_fixed_only)

# verdadero results: same as Aedes anphevirus:
#  - offspring sex doesn't matter
#  - Vergel vs. NO doesn't matter
#  - paternal vs. maternal matters (maternal better)

# Guadaloupe mosquito virus
df_offspring_m %>% group_by(Virus_target) %>% summarize(n=n())

df_offspring_gmv <- filter(df_offspring_m, Virus_target == "Guadeloupe Mosquito Virus")
gmv_model <- glmer(infected_boolean ~ 
                     Sex + 
                     non_infected_parent_genotype + 
                     Transmission_mode + 
                     (1 | cross_rep), 
                   data = df_offspring_gmv,
                   family = binomial) 

summary(gmv_model)

# what about a model accounting for infected parent genotype (Poza Rica vs Tapachula?)
gmv_model_2 <- glmer(infected_boolean ~ 
                       Sex + 
                       infected_parent_genotype +
                       non_infected_parent_genotype + 
                       Transmission_mode + 
                       (1 | cross_rep), 
                     data = df_offspring_gmv,
                     family = binomial) 

summary(gmv_model_2)
 
# got boundary fit warning so remove random effect
gmv_model_3 <- glm(infected_boolean ~ 
                     Sex + 
                     infected_parent_genotype +
                     non_infected_parent_genotype + 
                     Transmission_mode,
                   data = df_offspring_gmv,
                   family = binomial) 

summary(gmv_model_3)

# results: 
#  - offspring sex doesn't matter
#  - Vergel vs. NO doesn't matter (though closer now)
#  - paternal vs. maternal matters (maternal better)
#  - Tapachula vs. Poza Rica matters: Poza Rica more efficient, no doubt b/c Poza Rica higher starting prevalence



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
  p_om <- create_prev_plot(paste0(Parent1, " F x ", Parent2, " M"), "Hybrid", NA, "Offspring", "maternal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  # offspring paternal
  p_op <- create_prev_plot(paste0(Parent1, " M x ", Parent2, " F"), "Hybrid", NA, "Offspring", "paternal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  
  # horizontal - female
  p_hf  <- create_prev_plot(paste0(Parent2, " F x ", Parent1, " M"), NA, "Female", "After cohabitating", "Horizontal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  # horizontal - male
  p_hm  <- create_prev_plot(paste0(Parent1, " F x ", Parent2, " M"), NA, "Male",   "After cohabitating", "Horizontal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  
  # -----------------
  # Viral load plots
  # -----------------
  # Parent #1
  l_p1 <- create_levels_plot(paste0(Parent1, " Starting Prevalence"), Parent1, NA, "Starting Prevalence", "starting prevalence", Virus, male_color = male_color, female_color = female_color)
  # Parent  #2
  l_p2 <- create_levels_plot(paste0(Parent2, " Starting Prevalence"), Parent2, NA, "Starting Prevalence", "starting prevalence", Virus, male_color = male_color, female_color = female_color)
  # offspring maternal
  l_om <- create_levels_plot(paste0(Parent1, " F x ", Parent2, " M"), "Hybrid", NA, "Offspring", "maternal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  # offspring paternal
  l_op <- create_levels_plot(paste0(Parent1, " M x ", Parent2, " F"), "Hybrid", NA, "Offspring", "paternal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  
  # horizontal - female
  l_hf  <- create_levels_plot(paste0(Parent2, " F x ", Parent1, " M"), NA, "Female", "After cohabitating", "Horizontal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  # horizontal - male
  l_hm  <- create_levels_plot(paste0(Parent1, " F x ", Parent2, " M"), NA, "Male",   "After cohabitating", "Horizontal", Virus, facet_by="Replicate", male_color = male_color, female_color = female_color)
  
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
                              a_Strain = NA, 
                              a_Sex = NA, 
                              a_Stage = NA, 
                              a_Transmission_mode = NA, 
                              a_Virus_target = NA) {
  
  df_to_use <- df_prevalence_by_group
  
  # progressivly filter the data
  if (!is.na(a_Name))              { df_to_use <- df_to_use %>% filter(Name == a_Name) }
  if (!is.na(a_Strain))            { df_to_use <- df_to_use %>% filter(Strain == a_Strain) }
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
                 sprintf("%0.1f", pct),

                                  "%)")

  text
}

create_prev_text(a_Virus_target = "Aedes Anphevirus", 
                 a_Stage = "Offspring", 
                 a_Transmission_mode = "maternal",
                 a_Sex = "Male")

# create text related to RNA levels
create_levels_text <- function (a_Name = NA, 
                                a_Strain = NA, 
                                a_Sex = NA, 
                                a_Stage = NA, 
                                a_Transmission_mode = NA, 
                                a_Virus_target = NA) {
  
  df_to_use <- df_load_per_group
  
  # progressivly filter the data
  if (!is.na(a_Name))              { df_to_use <- df_to_use %>% filter(Name == a_Name) }
  if (!is.na(a_Strain))            { df_to_use <- df_to_use %>% filter(Strain == a_Strain) }
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
 
  text <- paste0( sprintf("%0.1f", mean_load) )

  text
}

# test
create_levels_text(a_Virus_target = "Aedes Anphevirus", 
                   a_Stage = "Offspring", 
                   a_Transmission_mode = "maternal",
                   a_Sex = "Male")


# ----------------
# Aedes anphevirus
# ----------------
                            
anphe_text_parent <- paste0("Nearly all adult mosquitoes in the Poza Rica colony were anphevirus-infected: ",
                            create_prev_text(a_Sex = "Male", a_Strain = "Poza Rica", 
                                             a_Virus_target = "Aedes Anphevirus", a_Stage = "Starting Prevalence"),
                            " of males and ",
                            create_prev_text(a_Sex = "Female", a_Strain = "Poza Rica", 
                                             a_Virus_target = "Aedes Anphevirus", a_Stage = "Starting Prevalence"),
                            " of females were positive by RT-qPCR (Fig. X).",
                            " The mean level of anphevirus RNA in infected males and females was ",
                            create_levels_text(a_Sex = "Male", a_Strain = "Poza Rica", 
                                               a_Virus_target = "Aedes Anphevirus", a_Stage = "Starting Prevalence"),
                            "x and ",
                            create_levels_text(a_Sex = "Female", a_Strain = "Poza Rica", 
                                               a_Virus_target = "Aedes Anphevirus", a_Stage = "Starting Prevalence"),
                            "x the level of actin mRNA. ",
                            "No parents in the New Orleans or Vergel colonies tested positive for anphevirus."
)

anphe_text_parent



anphe_text_offspring <- paste0("Aedes anphevirus transmitted efficiently from infected mothers. ", 
                            create_prev_text(a_Strain = "Hybrid",
                                             a_Virus_target = "Aedes Anphevirus", a_Stage = "Offspring", 
                                             a_Transmission_mode = "maternal"),
                            " of offspring of infected mothers tested positive for anphevirus RNA. ",
                            "Transmission from infected fathers was less efficient. ", 
                            create_prev_text(a_Strain = "Hybrid",
                                             a_Virus_target = "Aedes Anphevirus", a_Stage = "Offspring", 
                                             a_Transmission_mode = "paternal"),
                            " of offspring of infected fathers were infected. ",
                            "The mean level of anphevirus RNA in infected offspring was similar for maternal ",
                            "and paternal transmission: ",
                            create_levels_text(a_Strain = "Hybrid",
                                               a_Virus_target = "Aedes Anphevirus", 
                                               a_Stage = "Offspring",
                                               a_Transmission_mode = "maternal"),
                            "x and ",
                            create_levels_text(a_Strain = "Hybrid",
                                               a_Virus_target = "Aedes Anphevirus", 
                                               a_Stage = "Offspring",
                                               a_Transmission_mode = "paternal"),
                            "x the level of actin mRNA, respectively. ",
                            "Offspring exhibited a bimodal distribution of anphevirus RNA levels (Fig. X). ",
                            "A subset of positive offspring had relatively high anphevirus RNA levels and a subset ",
                            "had levels ~1000x lower. "
)

anphe_text_offspring


# extract p-value from anphe model
summary(anphe_model)
coef(summary(anphe_model))
anphe_tm_pval <- coef(summary(anphe_model))[4,4]

anphe_model_text <-  
  paste0("We used generalized linear models and binomial logistic regression to model anphevirus ",
         "transmission from parents to offspring. ",
         "Models contained transmission mode, non-infected parent genotype, and offspring sex as fixed effects, ",
         "and replicate as a random effect. ",
         "Transmission mode was the only significant predictor variable, ",
         "with offspring less likely to be infected via paternal transmission (p = ",
         sprintf("%0.1e", anphe_tm_pval),
         ")."
)
anphe_model_text

# ----------------
# Verdadero virus
# ----------------

df_verdadero_paternal <- df_prevalence_by_group %>% filter(Virus_target == "Verdadero Virus" & Strain == "Hybrid" & Transmission_mode == "paternal")
df_verdadero_paternal %>% summarize(total_n = sum(n),
                                    total_pos = sum(n_positive))

# two-proportion z test for verdadero virus in Cross et al vs. this study
prop.test(c(48,38), c(92,39), alternative = "two.sided", conf.level = 0.95)
# prop.test(x, n, alternative = "two.sided", conf.level = 0.95)
# x is a vector of the number of successes in each sample.
# n is a vector of the sample sizes for each group.

verdadero_text_parent <- 
  paste0("Verdadero virus was at high prevalence in the Poza Rica population, and undetectable in the New Orleans or Vergel mosquitoes (Fig. X). ",
         create_prev_text(a_Sex = "Male", a_Strain = "Poza Rica", 
                          a_Virus_target = "Verdadero Virus", a_Stage = "Starting Prevalence"),
         " of Poza Rica males and ",
         create_prev_text(a_Sex = "Female", a_Strain = "Poza Rica", 
                          a_Virus_target = "Verdadero Virus", a_Stage = "Starting Prevalence"),
         " of females were positive by RT-qPCR (Fig. X).",
         " The mean level of verdadero virus RNA in infected males and females was ",
         create_levels_text(a_Sex = "Male", a_Strain = "Poza Rica", 
                            a_Virus_target = "Verdadero Virus", a_Stage = "Starting Prevalence"),
         "x and ",
         create_levels_text(a_Sex = "Female", a_Strain = "Poza Rica", 
                            a_Virus_target = "Verdadero Virus", a_Stage = "Starting Prevalence"),
         "x the level of actin mRNA, respectively."
  )

verdadero_text_parent


# pull out max and min verdadero virus RNA levels in offspring
head(df)
verdadero_offspring <- df %>% filter(Virus_target == "Verdadero Virus", Strain == "Hybrid", Stage == "Offspring") 
max_verdadero_load <- verdadero_offspring %>% arrange(-Viral_Load) %>% head(1) %>% pull(Viral_Load)
min_verdadero_load <- verdadero_offspring %>% arrange( Viral_Load) %>% head(1) %>% pull(Viral_Load)

verdadero_text_offspring <- 
  paste0("Like anphevirus, verdadero virus exhibited biparental vertical transmission [REF Cross](Fig. X). ",
                            create_prev_text(a_Strain = "Hybrid",
                                             a_Virus_target = "Verdadero Virus", a_Stage = "Offspring", 
                                             a_Transmission_mode = "maternal"),
                            " of offspring of infected mothers tested positive for verdadero virus RNA. ",
                            "Paternal transmission was less efficient, with only ", 
                            create_prev_text(a_Strain = "Hybrid",
                                             a_Virus_target = "Verdadero Virus", a_Stage = "Offspring", 
                                             a_Transmission_mode = "paternal"),
                            " of offspring infected from infected fathers. ",
                            "The mean level of anphevirus RNA in infected offspring was similar for maternal ",
                            "and paternal transmission: ",
                            create_levels_text(a_Strain = "Hybrid",
                                               a_Virus_target = "Verdadero Virus", 
                                               a_Stage = "Offspring",
                                               a_Transmission_mode = "maternal"),
                            "x and ",
                            create_levels_text(a_Strain = "Hybrid",
                                               a_Virus_target = "Verdadero Virus", 
                                               a_Stage = "Offspring",
                                               a_Transmission_mode = "paternal"),
                            "x the level of actin mRNA, respectively. ",
                            "Despite these similar averages, levels of verdadero virus RNA ",
                            "in individual offspring spanned a broad range, from ",
                            sprintf("%0.1e", min_verdadero_load), 
                            "x to ",
                            sprintf("%0.1e", max_verdadero_load), 
                            "x actin mRNA."
)

verdadero_text_offspring
tab_model(verdadero_model, verdadero_model_fixed_only)

# extract p-value from anphe model
summary(verdadero_model)
coef(summary(anphe_model))
anphe_tm_pval <- coef(summary(anphe_model))[4,4]

anphe_model_text <-  
  paste0("We used generalized linear models and binomial logistic regression to model anphevirus ",
         "transmission from parents to offspring. ",
         "Models contained transmission mode, non-infected parent genotype, and offspring sex as fixed effects, ",
         "and replicate as a random effect. ",
         "Transmission mode was the only significant predictor variable, ",
         "with offspring less likely to be infected via paternal transmission (p = ",
         sprintf("%0.1e", anphe_tm_pval),
         ")."
)
anphe_model_text


# write out text for paper to a file
output_text <- paste(anphe_text_parent, anphe_text_offspring, anphe_model_text, methods_text, sep="\n\n")
cat(output_text)
writeLines(output_text, output_file)

# close file 
close(output_file)

# output models in a table using sjPlot tab_model function
tab_model(anphe_model, verdadero_model,
          show.ci = FALSE , # show CI
          show.aic = TRUE, # display AIC
          p.style = "scientific"
          # p.style = "scientific",
          # file="model_table.doc"
)