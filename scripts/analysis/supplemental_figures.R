# Written by Natalie Saragosa-Harris.
# August 2022.

library(dplyr)
library(glue)
library(ggplot2)
library(lme4)
library(lmerTest)
library(sjPlot) #https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html
library(sjmisc)
library(sjlabelled)
library(stargazer) #https://zief0002.github.io/book-8252/pretty-printing-tables-in-markdown.html
library(reshape2)
library(patchwork)
library(ggeffects)
library(reghelper) # for simple slopes.


datadirectory <- '' # Add directory where data are saved here.
data <- read.csv(glue('{datadirectory}behavior_rsa_conditionlevel_shortdata.csv'),stringsAsFactors = TRUE) # Read in data.

# Add z scores to the data.
data$CTQ_Total_z <- as.numeric(scale(data$CTQ_Total,center=TRUE))
data$CTQ_log <- as.numeric(log(data$CTQ_Total)) # Note: in R, log function defaults to natural log.
data$CTQ_log_z <- as.numeric(scale(data$CTQ_log,center=TRUE))
data$HONOS_total_z <- scale(data$HONOS_total, center = TRUE)[,]
data$angry_rt_average_z <- scale(data$angry_rt_average, center = TRUE)[,]
data$surprised_rt_average_z <- scale(data$surprised_rt_average, center = TRUE)[,]
data$happy_rt_average_z <- scale(data$happy_rt_average, center = TRUE)[,]
data$rt_difference_all_positive_vs_negative_z <- scale(data$rt_difference_all_positive_vs_negative, center = TRUE)[,]
data$rt_difference_positive_vs_negative_surprised_z <- scale(data$rt_difference_positive_vs_negative_surprised, center = TRUE)[,]
data$rt_difference_surprised_vs_angry_z <- scale(data$rt_difference_surprised_vs_angry, center = TRUE)[,]
data$rt_difference_surprised_vs_happy_z <- scale(data$rt_difference_surprised_vs_happy, center = TRUE)[,]
data$rt_difference_surprised_vs_unambiguous_z <- scale(data$rt_difference_surprised_vs_unambiguous, center = TRUE)[,]
data$surprised_negative_rating_rt_average_z <- scale(data$surprised_negative_rating_rt_average, center = TRUE)[,]
data$surprised_positive_rating_rt_average_z <- scale(data$surprised_positive_rating_rt_average, center = TRUE)[,]
data$all_negative_rating_rt_average_z <- scale(data$all_negative_rating_rt_average, center = TRUE)[,]
data$all_positive_rating_rt_average_z <- scale(data$all_positive_rating_rt_average, center = TRUE)[,]

# Theme for plots.
nsh_theme <- theme(text = element_text(family = "Avenir"),
                   title = element_text(size=22, vjust=2, face="bold"),
                   plot.subtitle = element_text(color="gray40", size=18, face="bold.italic"),
                   axis.title.x= element_text(size=20, vjust=-0.3),
                   axis.title.y= element_text(size=20, vjust=1.5),
                   axis.text.x= element_text(size=20, colour="black"),
                   axis.text.y= element_text(size=20, colour="black"),
                   strip.text = element_text(size=20, face="bold"),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))


# Color dictionary for plots, so that color matching is consistent.
fivecolors <- c("accumbens" = "#78B7C5",
                "amygdala" = "#EBCC2A", 
                "anteriorinsula" = "#C5776C", 
                "vmpfc" = "#9CB7B5",
                "V1" = "#A09CB0")

####################################################################################################
############## Figure 2: ELA and task behavior interact to predict global functioning. #############
####################################################################################################
# Note: Do not z score these because the unstandardized 0 point is meaningful.

honos_ctq_reactiontime_surprised_vs_angry <- lm(HONOS_total_z ~ CTQ_log_z*rt_difference_surprised_vs_angry, data = data)
summary(honos_ctq_reactiontime_surprised_vs_angry)

simple_slopes(honos_ctq_reactiontime_surprised_vs_angry,
              levels=list(CTQ_log_z=c(-1.5,1.5, 'sstest')))

honos_ctq_reactiontime_surprised_vs_happy <- lm(HONOS_total_z ~ CTQ_log_z*rt_difference_surprised_vs_happy, data = data)
summary(honos_ctq_reactiontime_surprised_vs_happy)

simple_slopes(honos_ctq_reactiontime_surprised_vs_happy,
              levels=list(CTQ_log_z=c(-1.5,1.5, 'sstest')))

figure2a <- plot_model(honos_ctq_reactiontime_surprised_vs_angry, type = "pred",
                       terms = c("rt_difference_surprised_vs_angry","CTQ_log_z[-1.5,1.5]"), 
                       colors = c("gray60", "#00573D"),
                       line.size = 2,
                       alpha = 0.2) +
  geom_vline(xintercept = 0, size = 1, linetype = "dotted") +
  ylim(-6,6) +
  xlab(expression(atop("Average reaction time difference (s):","ambiguous - threatening"))) +
  ylab("Global functioning impairment") +
  nsh_theme +
  theme(legend.position = "none") # Remove legend for now.

figure2b <- plot_model(honos_ctq_reactiontime_surprised_vs_happy, type = "pred", 
                       terms = c("rt_difference_surprised_vs_happy","CTQ_log_z[-1.5,1.5]"), 
                       colors = c("gray60", "#00573D"),
                       line.size = 2,
                       alpha = 0.2)+
  ylim(-6,6) +
  geom_vline(xintercept = 0, size = 1, linetype = "dotted") +
  labs(title = "ELA x Reaction Time Difference",
       subtitle = "Reaction time difference: ambiguous - nonthreatening (higher = long spent on ambiguous)") +
  xlab(expression(atop("Average reaction time difference (s):","ambiguous - nonthreatening"))) +
  ylab("Global functioning impairment") +
  nsh_theme +
  theme(legend.position = "none") # Remove legend for now.

figure2 <- figure2a + figure2b
figure2

####################################################################################################
############ Figure 3: ELA and ambiguous/threatening overlap within regions of interest. ###########
####################################################################################################

# Right amygdala.
right_amygdala <- ggplot(data,aes(x = CTQ_log_z, y = AO_SUR_average_amy_R_Thr50_fisher_z))+
  geom_smooth(method = "lm",size=3,color = "gray28") +
  geom_point(size=4, color = fivecolors["amygdala"]) +
  labs(title = "Right Amygdala") +
  scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 2, by = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0.1, 0.9, by = 0.2)) +
  xlab("") +  ylab("") + 
  nsh_theme

# Left amygdala.
left_amygdala <- ggplot(data,aes(x = CTQ_log_z, y = AO_SUR_average_amy_L_Thr50_fisher_z))+
  geom_smooth(method = "lm",size=3,color = "gray28") +
  geom_point(size=4, color = fivecolors["amygdala"]) +
  labs(title = "Left Amygdala") +
  scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 2, by = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0.1, 0.9, by = 0.2)) +
  xlab("") +  ylab("") + 
  nsh_theme

# Right accumbens.
right_accumbens <- ggplot(data,aes(x = CTQ_log_z, y = AO_SUR_average_accumbens_R_Thr25_fisher_z))+
  geom_smooth(method = "lm",size=3,color = "gray28") +
  geom_point(size=4, color = fivecolors["accumbens"]) +
  labs(title = "Right Nucleus Accumbens") +
  scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 2, by = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0.1, 0.9, by = 0.2)) +
  xlab("") +  ylab("") + 
  nsh_theme

# Left accumbens.
left_accumbens <- ggplot(data,aes(x = CTQ_log_z, y = AO_SUR_average_accumbens_L_Thr25_fisher_z))+
  geom_smooth(method = "lm",size=3,color = "gray28") +
  geom_point(size=4, color = fivecolors["accumbens"]) +
  labs(title = "Left Nucleus Accumbens") +
  scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 2, by = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0.1, 0.9, by = 0.2)) +
  xlab("") +  ylab("") +
  nsh_theme

# Right anterior insula.
right_anteriorinsula <- ggplot(data,aes(x = CTQ_log_z, y = AO_SUR_average_anteriorinsula_R_xu2021_fisher_z))+
  geom_smooth(method = "lm",size=3,color = "gray28") +
  geom_point(size=4, color = fivecolors["anteriorinsula"]) +
  labs(title = "Right Anterior Insula") +
  scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 2, by = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0.1, 0.9, by = 0.2)) +
  xlab("") +  ylab("") + 
  nsh_theme

# Left anterior insula.
left_anteriorinsula <- ggplot(data,aes(x = CTQ_log_z, y = AO_SUR_average_anteriorinsula_L_xu2021_fisher_z))+
  geom_smooth(method = "lm",size=3,color = "gray28") +
  geom_point(size=4, color = fivecolors["anteriorinsula"]) +
  labs(title = "Left Anterior Insula") +
  scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 2, by = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0.1, 0.9, by = 0.2)) +
  xlab("") +  ylab("") + 
  nsh_theme

# Right vmPFC.
right_vmpfc <- ggplot(data,aes(x = CTQ_log_z, y = AO_SUR_average_vmpfc_R_xu2021_fisher_z))+
  geom_smooth(method = "lm",size=3,color = "gray28") +
  geom_point(size=4, color = fivecolors["vmpfc"]) +
  labs(title = "Right vmPFC") +
  scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 2, by = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0.1, 0.9, by = 0.2)) +
  xlab("") + ylab("") + 
  nsh_theme

# Left vmPFC.
left_vmpfc <- ggplot(data,aes(x = CTQ_log_z, y = AO_SUR_average_vmpfc_L_xu2021_fisher_z))+
  geom_smooth(method = "lm",size=3,color = "gray28") +
  geom_point(size=4, color = fivecolors["vmpfc"]) +
  labs(title = "Left vmPFC") +
  scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 2, by = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0.1, 0.9, by = 0.2)) +
  xlab("") + ylab("") + 
  nsh_theme

accumbens <- left_accumbens + right_accumbens + plot_layout(ncol = 2)
amygdala <- left_amygdala + right_amygdala  + plot_layout(ncol = 2)
anteriorinsula <- left_anteriorinsula + right_anteriorinsula + plot_layout(ncol = 2)
vmpfc <- left_vmpfc + right_vmpfc + plot_layout(ncol = 2)

accumbens
amygdala
anteriorinsula
vmpfc
####################################################################################################
################## Supplemental Figure 3: Average RSA values within each region. ###################
####################################################################################################

longdata <- data[,grepl(glob2rx('Participant_ID|*average_*fisher_z'), names(data))] # Only keep these columns for the descriptive statistics table.
longdata <- melt(longdata,id.vars = 'Participant_ID', value.name = 'fisher_z')

longdata <- longdata %>%
  mutate(region = 
           case_when(grepl("_amy_",longdata$variable) ~ "amygdala",
                     grepl("_accumbens_",longdata$variable) ~ "accumbens",
                     grepl("_anteriorinsula_",longdata$variable) ~ "anteriorinsula",
                     grepl("_vmpfc_",longdata$variable) ~ "vmpfc",
                     grepl("_V1_",longdata$variable) ~ "V1"),
         hemisphere =
           case_when(grepl("_R_",longdata$variable) ~ "right",
                     grepl("_L_",longdata$variable) ~ "left"),
         comparison = 
           case_when(grepl("AO_HO",longdata$variable) ~ "threatening/nonthreatening",
                     grepl("AO_SUR",longdata$variable) ~ "threatening/ambiguous",
                     grepl("HO_SUR",longdata$variable) ~ "nonthreatening/ambiguous"))
longdata$regionname <- paste(longdata$hemisphere,"_",longdata$region,sep="")
factor_columns <- names(longdata)[!(names(longdata) %in% 'fisher_z')] # Make every column except fisher_z column a factor.
longdata[,factor_columns] <- lapply(longdata[,factor_columns], as.factor)

figureS3 <- ggplot(longdata, aes(fill=region, color = region, y=fisher_z, x=region)) + 
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

figureS3


# Bar plot.
longdata_toplot <- longdata %>% 
  dplyr::group_by(region,comparison,hemisphere) %>% 
  dplyr::summarise(mean = mean(fisher_z), sd = sd(fisher_z))

descriptive_statistics_barplot <- ggplot(longdata_toplot, aes(fill=region, y=mean, x=region)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  facet_grid(cols = vars(hemisphere), rows = vars(comparison)) +
  xlab("Region") +
  ylab("Fisher Z Value") + 
  theme(text = element_text(family = "Avenir"),        
        axis.title.y= element_text(size=10, vjust=1.5),
        axis.text.x= element_text(angle = 90, vjust = 0.5, hjust=1, size=10, colour="black"),
        axis.text.y= element_text(size=10, colour="black"),
        strip.text = element_text(size=10, face="bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

####################################################################################################
######## Supplemental Figure 4: ELA and ambiguous/threatening overlap within control region. #######
####################################################################################################

# Right V1.
right_v1 <- ggplot(data,aes(x = CTQ_log_z, y = AO_SUR_average_V1_R_Thr75_fisher_z))+
  geom_smooth(method = "lm",size=3,color = "gray28") +
  geom_point(size=4, color = fivecolors["V1"]) +
  labs(title = "Right V1") +
  scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 2, by = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0.1, 0.9, by = 0.2)) +
  xlab("") + ylab("") + 
  nsh_theme

# Left V1.
left_v1 <- ggplot(data,aes(x = CTQ_log_z, y = AO_SUR_average_V1_L_Thr75_fisher_z))+
  geom_smooth(method = "lm",size=3,color = "gray28") +
  geom_point(size=4, color = fivecolors["V1"]) +
  labs(title = "Left V1") +
  scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 2, by = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0.1, 0.9, by = 0.2)) +
  xlab("") + ylab("") + 
  nsh_theme

v1 <- left_v1 + right_v1 + plot_layout(ncol = 2)

####################################################################################################
######### Supplemental Figure 5: Average univariate values by condition within each region. ########
####################################################################################################

# Descriptive statistics: Average and standard deviations of univariate values for each region.
# First, create new data frame that is long format.

longdata_univariate <- data[,grepl(glob2rx('Participant_ID|*univariate_*'), names(data))] # Only keep these columns for the descriptive statistics table.
longdata_univariate <- melt(longdata_univariate,id.vars = 'Participant_ID', value.name = 'univariate_average')

longdata_univariate <- longdata_univariate %>%
  mutate(region = 
           case_when(grepl("_amy_",longdata_univariate$variable) ~ "amygdala",
                     grepl("_accumbens_",longdata_univariate$variable) ~ "accumbens",
                     grepl("_anteriorinsula_",longdata_univariate$variable) ~ "anteriorinsula",
                     grepl("_vmpfc_",longdata_univariate$variable) ~ "vmpfc",
                     grepl("_V1_",longdata_univariate$variable) ~ "V1"),
         hemisphere =
           case_when(grepl("_R_",longdata_univariate$variable) ~ "right",
                     grepl("_L_",longdata_univariate$variable) ~ "left"),
         emotion = 
           case_when(grepl("angry",longdata_univariate$variable) ~ "threatening",
                     grepl("happy",longdata_univariate$variable) ~ "nonthreatening",
                     grepl("surprised",longdata_univariate$variable) ~ "ambiguous"))

longdata_univariate$regionname <- paste(longdata_univariate$hemisphere,"_",longdata_univariate$region,sep="")

factor_columns <- names(longdata_univariate)[!(names(longdata_univariate) %in% 'univariate_average')] # Make every column except univariate_average column a factor.
longdata_univariate[,factor_columns] <- lapply(longdata_univariate[,factor_columns], as.factor)

# To make each region have a bigger plot, plot each one separately, with each emotion condition on the x axis.

univariate_plot_theme <- theme(text = element_text(family = "Avenir"), 
                                panel.spacing = unit(2, "lines"),
                                #axis.title.x= element_text(size=30, vjust=-0.3),
                                axis.title.x= element_blank(),# Remove since labels are clear.
                                axis.title.y= element_text(size=30, vjust=1.5),
                                axis.text.x= element_text(angle = 90, vjust = 0.5, hjust=1, size=30, colour="black"),
                                axis.text.y= element_text(size=30, colour="black"),
                                strip.text = element_text(size=30, face="bold"),
                                panel.background = element_blank(),
                                legend.position = "none", # Remove legend for now since we are only plotting one region at a time.
                                axis.line = element_line(colour = "black"))

amygdala_univariate_plot <- ggplot(subset(longdata_univariate, region == "amygdala"), aes(fill=region, y=univariate_average, x=emotion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape = 1, width = 0.05, color = "black") +
  scale_colour_manual(values = fivecolors) +
  scale_fill_manual(values = fivecolors) +
  facet_grid(cols = vars(hemisphere), rows = vars(region)) +
  xlab("Stimulus type") +
  ylab("Average univariate response") + 
  univariate_plot_theme

accumbens_univariate_plot <- ggplot(subset(longdata_univariate, region == "accumbens"), aes(fill=region, y=univariate_average, x=emotion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape = 1, width = 0.05, color = "black") +
  scale_colour_manual(values = fivecolors) +
  scale_fill_manual(values = fivecolors) +
  facet_grid(cols = vars(hemisphere), rows = vars(region)) +
  xlab("Stimulus type") +
  ylab("Average univariate response") + 
  univariate_plot_theme

anteriorinsula_univariate_plot<- ggplot(subset(longdata_univariate, region == "anteriorinsula"), aes(fill=region, y=univariate_average, x=emotion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape = 1, width = 0.05, color = "black") +
  scale_colour_manual(values = fivecolors) +
  scale_fill_manual(values = fivecolors) +
  facet_grid(cols = vars(hemisphere), rows = vars(region)) +
  xlab("Stimulus type") +
  ylab("Average univariate response") + 
  univariate_plot_theme

vmpfc_univariate_plot<- ggplot(subset(longdata_univariate, region == "vmpfc"), aes(fill=region, y=univariate_average, x=emotion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape = 1, width = 0.05, color = "black") +
  scale_colour_manual(values = fivecolors) +
  scale_fill_manual(values = fivecolors) +
  facet_grid(cols = vars(hemisphere), rows = vars(region)) +
  xlab("Stimulus type") +
  ylab("Average univariate response") + 
  univariate_plot_theme

V1_univariate_plot <- ggplot(subset(longdata_univariate, region == "V1"), aes(fill=region, y=univariate_average, x=emotion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape = 1, width = 0.05, color = "black") +
  scale_colour_manual(values = fivecolors) +
  scale_fill_manual(values = fivecolors) +
  facet_grid(cols = vars(hemisphere), rows = vars(region)) +
  xlab("Stimulus type") +
  ylab("Average univariate response") + 
  univariate_plot_theme



amygdala_univariate_plot + ylim(-1,2)

accumbens_univariate_plot + ylim(-3,2)

anteriorinsula_univariate_plot + ylim(-2,2)

vmpfc_univariate_plot + ylim(-3,2)

V1_univariate_plot + ylim(-3,5)
