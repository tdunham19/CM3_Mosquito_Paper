library(tidyverse)
library(patchwork)
library(readxl)
library(binom)


# read in all data
df <- read_excel("all_data.xlsx")


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
  plot_filename <- paste0(Parent1, " x ", Parent2, "_", Virus, ".pdf")
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

