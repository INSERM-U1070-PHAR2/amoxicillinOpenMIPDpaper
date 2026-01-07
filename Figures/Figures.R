library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(purrr)
library(tidyverse)
library(grid)
library(here)

here::i_am("Figures/Figures.R")

#### Figure 3 ####
Results_Under_overdose <- read.csv("3.csv", sep=";")
Results_Under_overdose$Method <- factor(Results_Under_overdose$Method, levels = c("Method ens", "Stacking","SVM","KNN", "RF","XGboost","FAMD", "RT informed ens", "CT informed ens",  "WME","Uninformed ens", "Meta model", "Rambaud", "Mellon", "Fournier", "Carlier","Nomogram", "Standard dose"))

dosing_summary <- Results_Under_overdose %>%
  pivot_longer(
    cols = -Method,
    names_to = "Variable",
    values_to = "Proportion"
  ) %>%
  separate(Variable, into = c("Administration", "Dosing"), sep = "_", remove = TRUE, extra = "merge") %>%
  mutate(
    Administration = recode(Administration,
                     sim = "Simulated",
                     all = "Clinical (all)",
                     con = "Clinical (ci)"),
    Dosing = recode(Dosing,
                    "On" = "On target",
                    "target" = "On target",
                    "On_target" = "On target",
                    "Underdosed" = "Underdosed",
                    "Overdosed" = "Overdosed"),
    Dosing = factor(Dosing, levels = c("Overdosed", "Underdosed", "On target")),
    Label = ifelse(Dosing == "On target", paste0(Proportion), NA)
  )

dosing_summary$Administration <- factor(dosing_summary$Administration, levels = c("Simulated", "Clinical (all)","Clinical (ci)"))

plot_3 <- ggplot(dosing_summary, aes(x = Method, y = Proportion,
                                     fill = Dosing)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            vjust = 0.5, color = "white", size = 5, na.rm = TRUE) +
  scale_fill_manual(values = c(
    "Underdosed" = "darkorange",
    "On target" = "chartreuse4",
    "Overdosed" = "#A91A27"
  )) +
  labs(y = "Correct predictions (%)",
       fill = "Dosing") +
  facet_wrap(~Administration, nrow = 1) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 16)
  ) +
  ylim(0, 100) +
  coord_flip() +
  theme(strip.text = element_text(size = 17))

print(plot_3)

ggsave("3.jpg", plot = plot_3, width = 8.27, height = 10, units = "in", dpi = 600)

#### Figure 6 ####
# Plot to represent model weights in the different clinical cohorts - to complement the ratio plot
Weights <- read.csv2(here("Figures/6.csv"), sep=";")

Weights_long <- Weights %>%
  pivot_longer(cols = c(Carlier, Fournier, Mellon, Rambaud),
               names_to = "Model",
               values_to = "Percentage")

# Put Carlier on the bottom
Weights_long$Model <- factor(Weights_long$Model, levels = c("Rambaud", "Mellon", "Fournier", "Carlier"))

model_colors <- c(
  "Carlier" = "blue",
  "Fournier" = "cyan3",
  "Mellon"   = "darkorchid1",
  "Rambaud"  = "#619cff"
)

plot_cohort <- function(cohort_name) {
  Weights_long %>%
    filter(Cohort == cohort_name) %>%
    ggplot(aes(x = Method, y = Percentage, fill = Model)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = model_colors) +
    labs(x = NULL, y = "Model weight (%)", title = cohort_name) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(size = 14, angle = 20, hjust = 1),
      legend.position = "none",
      plot.title = element_text(size = 16),
      axis.title.y = element_text(size = 15)
    )
}

# List of cohorts in order
cohorts <- unique(Weights$Cohort)

plots <- lapply(cohorts, plot_cohort)

plot_6 <- (plots[[3]] | plots[[4]]) /
  (plots[[5]] | plots[[6]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 15))

plot_6

ggsave(here("Figures/6.jpg"), plot = plot_6, width = 9, height = 7.5, units = "in", dpi = 600)

#### Figure S4 ####
Standard_dose_selection <- read.csv(here("Figures/S4.csv"), sep=";")

Standard_dose_selection$Method <- factor(Standard_dose_selection$Method, levels = c("CRCL-based", "Infection-based", "200 mg/kg", "150 mg/kg","100 mg/kg"))

dosing_summary <- Standard_dose_selection %>%
  pivot_longer(cols = c(On_target, Underdosed, Overdosed),
               names_to = "Dosing", values_to = "Proportion") %>%
  mutate(
    Dosing = factor(Dosing, levels = c("Overdosed", "On_target", "Underdosed")),
    Label = ifelse(Dosing == "On_target", paste0(Proportion, " %"), NA)
  ) %>%
  mutate(Dosing = recode(Dosing, "On_target" = "On target"))

dosing_summary$Dosing <- factor(dosing_summary$Dosing,
                                levels = c("Overdosed", "Underdosed", "On target"))

plot_S4 <- ggplot(dosing_summary, aes(x = Method, y = Proportion, fill = Dosing)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5, na.rm = TRUE) +
 
  scale_fill_manual(values = c(
    "Underdosed" = "darkorange",
    "On target" = "chartreuse4",
    "Overdosed" = "#A91A27"
  )) +
  labs(y = "%",
       fill = "Dosing") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 16)
  ) +
  guides(fill = guide_legend(title = NULL)) +
  coord_flip()

print(plot_S4)

ggsave(here("Figures/S4.jpg"), plot = plot_S4, width = 8.27, height = 3.5, units = "in", dpi = 600)

#### Figure S19 ####
Sensitivity_number_of_subjects_sim <- read.csv(here("Figures/S19.csv"), sep=";")

Sensitivity_number_of_subjects_sim_long <- Sensitivity_number_of_subjects_sim %>%
  pivot_longer(
    cols = -Method,
    names_to = c("Number", ".value"),
    names_pattern = "X(\\d+)_(mean|sd)"
  ) %>%
  mutate(Number = as.numeric(Number))

plot_S19 <- ggplot(Sensitivity_number_of_subjects_sim_long, aes(x = Number, y = mean, color = Method, fill = Method)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = Method),
              alpha = 0.15, colour = NA) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Sensitivity analysis - number of subjects (simulated data)",
    x = "Number of subjects in training data",
    y = "% of correct dose predictions",
    color = "Method",
    fill = "Method") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(angle = 20, hjust = 1)) +
  scale_x_continuous(
    breaks = seq(0, 3000, by = 300)) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    limits = c(0, 50))

plot_S19

ggsave(here("Figures/S19.jpg"), plot = plot_S19, width = 8, height = 6, units = "in", dpi = 600)

#### Figure S20 ####
Sensitivity_number_of_subjects_clin <- read.csv(here("Figures/S20.csv"), sep=";")

Sensitivity_number_of_subjects_clin_long <- Sensitivity_number_of_subjects_clin %>%
  pivot_longer(
    cols = -Method,
    names_to = c("Number", ".value"),
    names_pattern = "X(\\d+)_(mean|sd)"
  ) %>%
  mutate(Number = as.numeric(Number))

plot_S20 <- ggplot(Sensitivity_number_of_subjects_clin_long, aes(x = Number, y = mean, color = Method, fill = Method)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = Method),
              alpha = 0.15, colour = NA) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Sensitivity analysis - number of subjects (clinical data)",
    x = "Number of subjects in training data",
    y = "% of correct dose predictions",
    color = "Method",
    fill = "Method") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(angle = 20, hjust = 1)) +
  scale_x_continuous(
    breaks = seq(0, 3000, by = 300)) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    limits = c(0, 50))

  

plot_S20

ggsave(here("Figures/S20.jpg"), plot = plot_S20, width = 8, height = 6, units = "in", dpi = 600)

