# This has the scripts for the plots included in the supplement.
# Corresponding paper:  Early life adversity is associated with greater similarity in neural representations of ambiguous and threatening stimuli.
# Written by Natalie Saragosa-Harris.

library(plyr)
library(readr)
library(psych)
library(dplyr)
library(tidyr)
library(glue)
library(ggplot2)
library(lme4)
library(lmerTest)
library(sjPlot) #https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html
library(sjmisc)
library(sjlabelled)
library(stargazer) #https://zief0002.github.io/book-8252/pretty-printing-tables-in-markdown.html
library(reshape2)
library(vtable) # for summary tables (descriptive statistics).
library(gtsummary) # also for summary tables.
library(pander) # For formatting tables for t tests results.
library(patchwork)
library(ggeffects)
library(reghelper) # for simple slopes.
library(qwraps2)
options(qwraps2_markup = "markdown")
options(scipen=1, digits=2)

data <- read.csv('data/behavior_rsa_shortdata.csv',stringsAsFactors = TRUE) # Read in data.
long_behavioral_data <- read.csv('data/behavioraltask_longdata.csv',stringsAsFactors = TRUE)
tsnr_data <- read.csv('data/tSNR_estimates.csv',stringsAsFactors = TRUE) # Read in temporal signal to noise data.

# Theme for plots.
nsh_theme <- theme(text = element_text(family = "Avenir"),
                   title = element_text(size=28, vjust=2, face="bold"),
                   plot.subtitle = element_text(size=28,color="gray40", face="bold.italic"),
                   axis.title.x= element_text(size=28, vjust=-0.3),
                   axis.title.y= element_text(size=28, vjust=1.5),
                   axis.text.x= element_text(size=28, colour="black"),
                   axis.text.y= element_text(size=28, colour="black"),
                   strip.text = element_text(size=28, face="bold"),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# Color dictionary for plots, so that color matching is consistent.
fivecolors <- c("accumbens" = "#78B7C5",
                "amygdala" = "#EBCC2A",
                "anteriorinsula" = "#C5776C",
                "vmpfc" = "#9CB7B5",
                "V1" = "#A09CB0")

###############################################################################################
###### Supplemental Figure 2: Distributions of questionnaire scores and behavioral data. ######
###############################################################################################

ctq_distribution <- ggplot(data, aes(x = CTQ_Total)) +
  geom_histogram(binwidth = 1, fill = "#7fa99de6", color = "black",aes(y = ..count..)) +
  geom_density(aes(y = ..count..), color = "black", size = 1) +
  scale_x_continuous(breaks = seq(25,80,5)) + expand_limits(x = c(25,80)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # Adding this because otherwise it creates a space in between the bottom of the plot and the x axis.
  labs(title = "(A) CTQ Total Scores",
       x = "CTQ Total Score",
       y = "Number of participants") +
  nsh_theme

honos_distribution <- ggplot(data, aes(x = HONOS_total)) +
  geom_histogram(binwidth = 1, fill = "#7fa99de6", color = "black") +
  geom_density(aes(y = ..count..), color = "black", size = 1) +
  scale_x_continuous(breaks = seq(0,35,5)) +  expand_limits(x = c(-1,35)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "(B) HoNOS Total Scores",
       x = "HoNOS Total Score",
       y = "Number of participants") +
  nsh_theme

negativitybias_distribution <- ggplot(data, aes(x = Percent_Surprised_Negative)) +
  geom_histogram(binwidth = 0.05, fill = "#7fa99de6", color = "black") +
  geom_density(color = "black", size = 1) +
  scale_x_continuous(breaks = c(0,0.25,0.50,0.75,1.0)) + expand_limits(x = c(0, 1.1)) +
  scale_y_continuous(limits = c(0,10),breaks = seq(0,10,2),expand = expansion(mult = c(0, 0.05))) +
  labs(title = "(C) Negativity biases in post-scan task",
       x = "Proportion ambiguous trials categorized negatively\n",
       y = "Number of participants") +
  nsh_theme

ctq_distribution
honos_distribution
negativitybias_distribution


########################################################################################################
##### Supplemental Figure 3: Accuracy for threatening and nonthreatening trials in post-scan task. #####
########################################################################################################

# Plot accuracy for angry (percent categorized as negative) and happy (percent categorized as positive) trials in post-scan task.

accuracy_data <- data %>% select(Participant_ID, Percent_Angry_Correct, Percent_Happy_Correct) %>%
  pivot_longer(!Participant_ID, names_to = "trial_type", values_to = "accuracy")

accuracy_data$trial_type <- gsub("Percent_", "", accuracy_data$trial_type)
accuracy_data$trial_type <- gsub( "_Correct", "", accuracy_data$trial_type)


angry_happy_accuracy_plot <- ggplot(accuracy_data,aes(x = trial_type, y = accuracy)) + 
  geom_bar(stat="identity",fill="gray80",alpha=0.5) +
  geom_jitter(aes(colour = Participant_ID), size = 6, width = 0.25, alpha = 0.4) + 
  scale_y_continuous(limits = c(0.00,1.00),breaks = seq(0,1,0.25)) +
  nsh_theme + 
  theme(legend.position = "none") +
  labs(title = "Accuracy for non-ambiguous (angry and happy) trials in post-scan task",
       subtitle = "Angry = percent categorized as negative\nHappy = percent categorized as positive",
       x = "Trial type\n",
       y = "Accuracy")

angry_happy_accuracy_plot

###############################################################################################
###### Supplemental Figure 4: Average RSA values within each region. ######
###############################################################################################
longdata <- longdata %>% select(Participant_ID,ends_with("fisher_z"))

# Assuming your dataframe is named 'data'
# Use pivot_longer() function to convert to long format
longdata <- pivot_longer(longdata, 
                          cols = -Participant_ID,  # Specify columns to pivot
                          names_to = "variable",  # Name of the new 'variable' column
                          values_to = "fisher_z")    # Name of the new 'value' column

longdata <- longdata %>% mutate(region = case_when(grepl("amy", variable, ignore.case = TRUE) ~ "amygdala",
                                                   grepl("accumbens", variable, ignore.case = TRUE) ~ "accumbens",
                                                   grepl("anteriorinsula", variable, ignore.case = TRUE) ~ "anteriorinsula",
                                                   grepl("vmpfc", variable, ignore.case = TRUE) ~ "vmpfc",
                                                   grepl("V1", variable, ignore.case = TRUE) ~ "V1",
                                                   TRUE ~ NA_character_)) %>% 
                          mutate(hemisphere = case_when(grepl("_R_", variable, ignore.case = TRUE) ~ "right",
                                                        grepl("_L_", variable, ignore.case = TRUE) ~ "left",
                                                        TRUE ~ NA_character_)) %>% 
                          mutate(comparison = case_when(grepl("HO_SUR", variable, ignore.case = TRUE) ~ "nonthreatening/ambiguous",
                                                        grepl("AO_SUR", variable, ignore.case = TRUE) ~ "threatening/ambiguous",
                                                        grepl("AO_HO", variable, ignore.case = TRUE) ~ "threatening/nonthreatening",
                                                        TRUE ~ NA_character_))


supplementalfigurefour <- ggplot(longdata, aes(fill=region, color = region, y=fisher_z, x=region)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape = 1, width = 0.05, color = "black") +
  scale_y_continuous(breaks = c(0.1,0.3,0.5,0.7,0.9)) +
  scale_colour_manual(values = fivecolors) +
  scale_fill_manual(values = fivecolors) +
  facet_grid(cols = vars(hemisphere), rows = vars(comparison)) +
  xlab("Region") +
  ylab("Fisher Z Value") + 
  theme(text = element_text(family = "Avenir"), 
        panel.spacing = unit(2, "lines"),
        axis.title.x= element_text(size=22, vjust=-0.3),
        axis.title.y= element_text(size=22, vjust=1.5),
        axis.text.x= element_text(angle = 90, vjust = 0.5, hjust=1, size=22, colour="black"),
        axis.text.y= element_text(size=22, colour="black"),
        strip.text.x = element_text(size=22, face="bold"),
        strip.text.y = element_text(size=14),
        panel.background = element_blank(),
        legend.position = "none", # Get rid of legend since the regions are already labeled on the x axis.
        axis.line = element_line(colour = "black"))

supplementalfigurefour

###############################################################################################
###### Supplemental Figure 5: Temporal signal to noise ratios. ######
###############################################################################################

# These plots use the filtered_func images, which are the minimally preprocessed BOLD images that are used as  input for first level models.
# These minimally preprocessed images include include slice-timing correction and motion correction. In this case, it does not have any spatial smoothing.

amygdala_filteredfunc_plot <- ggplot(subset(tsnr_data, region == "amygdala"), 
                                     aes(fill=region, y=average_tSNR_value, x=hemisphere)) +
  geom_boxplot(outlier.shape = NA) +
  ylim(0,30) +
  geom_jitter(shape = 1, width = 0.05, color = "black") +
  scale_colour_manual(values = fivecolors) +
  scale_fill_manual(values = fivecolors) +
  #facet_grid(rows = vars(region)) +
  labs(subtitle = "Amygdala", y = "Signal to noise ratio") + 
  theme(legend.position = "none") +
  nsh_theme

accumbens_filteredfunc_plot <- ggplot(subset(tsnr_data, region == "accumbens"), 
                                      aes(fill=region, y=average_tSNR_value, x=hemisphere)) +
  geom_boxplot(outlier.shape = NA) +
  ylim(0,30) +
  geom_jitter(shape = 1, width = 0.05, color = "black") +
  scale_colour_manual(values = fivecolors) +
  scale_fill_manual(values = fivecolors) +
  #facet_grid(rows = vars(region)) +
  labs(subtitle = "Nucleus accumbens",y = " ") +
  theme(legend.position = "none") +
  nsh_theme

anteriorinsula_filteredfunc_plot <- ggplot(subset(tsnr_data, region == "anteriorinsula"), 
                                           aes(fill=region, y=average_tSNR_value, x=hemisphere)) +
  geom_boxplot(outlier.shape = NA) +
  ylim(0,40) +
  geom_jitter(shape = 1, width = 0.05, color = "black") +
  scale_colour_manual(values = fivecolors) +
  scale_fill_manual(values = fivecolors) +
  labs(subtitle = "Anterior insula", y = " ") +
  theme(legend.position = "none") +
  nsh_theme


vmpfc_filteredfunc_plot <- ggplot(subset(tsnr_data, region == "vmpfc"), 
                                  aes(fill=region, y=average_tSNR_value, x=hemisphere)) +
  geom_boxplot(outlier.shape = NA) +
  ylim(0,40) +
  geom_jitter(shape = 1, width = 0.05, color = "black") +
  scale_colour_manual(values = fivecolors) +
  scale_fill_manual(values = fivecolors) +
  labs(subtitle = "\nvmPFC",y = "Signal to noise ratio") +
  theme(legend.position = "none") +
  nsh_theme

v1_filteredfunc_plot <- ggplot(subset(tsnr_data, region == "V1"), 
                               aes(fill=region, y=average_tSNR_value, x=hemisphere)) +
  geom_boxplot(outlier.shape = NA) +
  ylim(0,50) +
  geom_jitter(shape = 1, width = 0.05, color = "black") +
  scale_colour_manual(values = fivecolors) +
  scale_fill_manual(values = fivecolors) +
  labs(subtitle = "\nV1",y = " ") +
  theme(legend.position = "none") +
  nsh_theme

amygdala_filteredfunc_plot + accumbens_filteredfunc_plot + anteriorinsula_filteredfunc_plot + vmpfc_filteredfunc_plot + v1_filteredfunc_plot +
  plot_layout(ncol = 3) +
  plot_annotation(title = "Temporal signal to noise ratio by region", subtitle = "Minimally preprocessed, unmodeled BOLD images\n") & nsh_theme
