# clear the workspace
rm(list=ls())

# libraries 
library(glue) 
library(grid)
library(dplyr)
library(tidyr)
library(ggh4x)
library(ggpubr)
library(scales)
library(readxl)
library(writexl)
library(stringr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(reshape2)
library(graphics)
library(latex2exp)
library(gridExtra)
library(gtsummary)
library(flextable)
library(hrbrthemes)

theme_gtsummary_compact()
set_flextable_defaults(background.color = "white")

scientific_10 <- function(x) {parse(text=gsub("e\\+*", " %*% 10^", scales::scientific_format()(x))) }

human.cov.study.data <- read_excel("human_coronavirus_study_data.xlsx")  # the human corona-virus study data

colnames(human.cov.study.data) # column names

# ---------- BAR RELATIONSHIP BETWEEN VARIABLES ----------
## ---------- vaccination and rural/urban settlement ----------
evervaccinated.area <- human.cov.study.data |> count(ever_vaccinated_COVID, area, sort = FALSE)

evervaccinated.area <- evervaccinated.area[complete.cases(evervaccinated.area), ]
evervaccinated.area$vaccinated <- ifelse(evervaccinated.area$ever_vaccinated_COVID == "1", "Yes", "No")

graph1 <- ggplot(evervaccinated.area, aes(factor(x = vaccinated, level = c("Yes", "No")), y = n, fill = area)) +
  geom_bar(position="dodge", stat = "identity", color = "black") +
  geom_text(aes(label = n), size = 4, vjust=-0.4, position = position_dodge(0.7)) +
  labs(title = "COVID-19 Vaccination Status", y = "Number of participants", x = "", fill = "Area:", face = 'bold') +
  theme_classic() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
                          plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,350))
graph1
#ggsave("new img/graph1.png", graph1, dpi = 1200, width = 3, height = 4)

# Classifying the ages into groups using the function (age_group_func) below
age_group_func <- function(df) {
  age_groups <- c()
  
  for (age in df$age) {
    if (age <= 10) {
      age_groups <- c(age_groups, "<10 years")
    }
    if (11 <= age & age <= 20) {
      age_groups <- c(age_groups, "11-20 years")
    }
    if (21 <= age & age <= 40) {
      age_groups <- c(age_groups, "21-40 years")
    }
    if (41 <= age & age <= 60) {
      age_groups <- c(age_groups, "41-60 years")
    }
    if (age >= 61) {
      age_groups <- c(age_groups, ">61 years")
    }
  }
  return(age_groups)
}

# adding a new column named 'age_group'
human.cov.study.data$age_group <- age_group_func(human.cov.study.data)

## ---------- age groups and rural/urban settlements ----------
agegroup.area <- human.cov.study.data |> count(age_group, area, sort = FALSE)

agegroup.area <- agegroup.area[complete.cases(agegroup.area), ]

age_group_order <- c("<10 years", "11-20 years", "21-40 years", "41-60 years", ">61 years") 

graph2 <- ggplot(agegroup.area, aes(x = factor(age_group, level = age_group_order), y = n, fill = area)) +
  geom_bar(position="dodge", stat = "identity", color = "black") +
  geom_text(aes(label = n), size = 4, vjust=-0.4, position = position_dodge(0.7))+
  labs(y = "Number of participants", x = "", fill = "Area: ", title = "Age group", face = "bold") +
  theme_classic() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1),
                          plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,250))
graph2
#ggsave("new new img/graph2a.png", graph2, dpi = 1200, width = 4, height = 4)

## ----------- sex and rural/urban settlements ----------
sex.area <- human.cov.study.data |> count(sex, area, sort = FALSE)

sex.area <- sex.area[complete.cases(sex.area), ]

graph3 <- ggplot(sex.area, aes(x = sex, y = n, fill = area)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_text(aes(label = n), size = 4, vjust=-0.4, hjust =0.5, position = position_dodge(0.75))+
  labs(y = "Number of participants", x = "", fill = "Area: ", title = "Sex", face = "bold") +
  theme_classic() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
                          plot.title = element_text(hjust = 0.5, face = "bold"))  +
  scale_y_continuous(limits = c(0,370))
graph3
#ggsave("new img/graph3a.png", graph3, dpi = 1200, width = 3, height = 4)

## ---------- combined graph of graph1, graph2 and graph3 ----------
combined_graph <- ggarrange(graph2,
                            ggarrange(graph1, graph3, ncol = 2, labels = c("B", "C"), common.legend = TRUE, legend = "bottom"), 
                            nrow = 2, labels = "A") 
combined_graph
ggsave("new img/combined_plotb.png", combined_graph, dpi = 600, width = 6, height = 6, bg = "white")

# ----------- Variable labels -----------

# the "cov_signal_spikes" is used to access their columns in the study data
cov_signal_spikes <- c("SARS-CoV-2 Spike signal", "SARS-CoV-2 RBD signal", "HCoV-229E Spike signal", "HCoV-HKU1 Spike signal", "HCoV-NL63 Spike signal", "HCoV-OC43 Spike signal")

# the "cov_signal_labels" contains renamed values of the signal spikes and is be used to reassign labels in the plots
cov_signal_labels <- c("SARS-CoV-2 Spike signal" = "SARS-CoV-2 Spike", "SARS-CoV-2 RBD signal" = "SARS-CoV-2 RBD", "HCoV-229E Spike signal" = "229E Spike",
                       "HCoV-HKU1 Spike signal" = "HKU1 Spike", "HCoV-NL63 Spike signal" = "NL63 Spike", "HCoV-OC43 Spike signal" = "OC43 Spike")

# vaccinated labels
vaccinated_labels <- c("0" = "Non-vaccinated", "1" = "Vaccinated")
vaccine_dose_labels <- c("1" = "Vaccine Dose 1", "2" = "Vaccine Dose 2", "3" = "Vaccine Dose 3")

town_labels_order <- c("Amanfrom", "Doblo", "Kraboa", "Obom", "Panpaso", "Amasaman", "Medie", "Nsawam", "Ofankor", "Pokuase")
#town_labels <- c("AF" = "Amanfrom", "DO" = "Doblo", "KR" = "Kraboa","OB" = "Obom", "PA" = "Pampamso",
                 #"AM" = "Amansaman", "ME" = "Medie",  "NS" = "Nsawam", "OF" = "Ofankor", "PO" = "Pokuase")

age_group_order <- c("<10 years", "11-20 years", "21-40 years", "41-60 years", ">61 years")

# ---------- SIGNAL SPIKES PLOTS ----------
## ----------- area and signals ----------
area.signals_spike.data <- human.cov.study.data[,c("area", cov_signal_spikes)]
combined.area.signals_spike.data <- reshape2::melt(area.signals_spike.data, id = "area")

graph4 <- ggplot(data = combined.area.signals_spike.data, aes(x = variable, y = value, fill = area)) +
  geom_boxplot(outlier.shape = NA) + theme_bw() + 
  geom_point(aes(fill=area), size=0.5,position = position_jitterdodge(jitter.width=0.1)) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5)) +
  scale_y_continuous(breaks = c(1e3,1e4,1e5,1e6), labels = scientific_10, limits = c(1e3,1e7)) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "", fill = "Legend:") + coord_trans(y = "log10") +
  scale_x_discrete(labels = c("SARS-CoV-2 Spike","SARS-CoV-2 RBD","HCoV-229E Spike","HCoV-HKU1 Spike","HCoV-NL63 Spike", "HCoV-OC43 Spike")) +
  stat_compare_means(label.y = c(5e6), label = "p.signif") 
graph4
ggsave("new img/graph4.png", graph4, dpi = 1200, width = 6, height = 6)

## ---------- town, area and signal spikes ----------

town.area.signals_spike.data <- human.cov.study.data[,c("town", "area", cov_signal_spikes)]
combined.town.area.signals_spike.data <- melt(town.area.signals_spike.data, id = c("town", "area"))

graph5 <- ggplot(data = combined.town.area.signals_spike.data, aes(factor(x = town, level = town_labels_order), y=value, fill=area)) +
  geom_boxplot(outlier.shape = NA) + theme_bw() +
  geom_point(aes(fill=area), size=0.5, position = position_jitterdodge(jitter.width=0.1)) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = c(1e3,1e4,1e5,1e6), labels = scientific_10) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "", fill = "Legend:") + coord_trans(y = "log10") +
  facet_nested_wrap(~variable,nrow = 1, labeller = as_labeller(cov_signal_labels)) 
graph5 
ggsave("new img/graph5.png", graph5, dpi = 1200, width = 10, height = 5)

## ----------  age groups, area and signal spikes ----------

agegroup.area.signals_spike.data <- human.cov.study.data[,c("age_group", "area", cov_signal_spikes)]
combined.agegroup.area.signals_spike.data <- melt(agegroup.area.signals_spike.data, id = c("age_group", "area"))

graph6 <- ggplot(data = combined.agegroup.area.signals_spike.data, aes(factor(x = age_group, level = age_group_order), y=value, fill=area)) +
  geom_boxplot(outlier.shape = NA) + theme_bw() +
  geom_point(aes(fill=area), size=0.5, position = position_jitterdodge(jitter.width=0.1)) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1), plot.title = element_text(hjust=0.5), 
        axis.title.y = element_text(face="bold"), strip.text.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = c(1e3,1e4,1e5,1e6), labels = scientific_10) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "", fill = "Legend:") + coord_trans(y = "log10") +
  facet_nested_wrap(~variable,nrow = 1, labeller = as_labeller(cov_signal_labels)) 
graph6
ggsave("new img/graph6.png", graph6, dpi = 1200, width = 10, height = 5)

## ---------- combined graph of graph5 and graph6 ----------
combined_graph2 <- ggarrange(graph5, graph6, ncol = 1, labels = c("A", "B"), common.legend = TRUE, legend = "bottom")
combined_graph2
ggsave("new img/combined_plot2.png", combined_graph2, dpi = 1200, width = 10, height = 7, bg = "white")

## ---------- sex, area and signals ----------

sex.area.signals_spike.data <- human.cov.study.data[,c("sex", "area", cov_signal_spikes)]
combined.sex.area.signals_spike.data <- melt(sex.area.signals_spike.data, id = c("sex", "area"))

graph7 <- ggplot(data = combined.sex.area.signals_spike.data, aes(factor(x = sex), y=value, fill=area)) +
  geom_boxplot(outlier.shape = NA) + theme_bw() +
  geom_point(aes(fill=area), size=0.5, position = position_jitterdodge(jitter.width=0.1)) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = c(1e3,1e4,1e4,1e5,1e6), labels = scientific_10, limits = c(1e2, 1e7)) +
  #scale_x_discrete(breaks = c(1,2), labels = c("Male", "Female")) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "", fill = "Legend:") + coord_trans(y = "log10") +
  facet_nested_wrap(~variable,nrow = 1, labeller = as_labeller(cov_signal_labels)) +
  stat_compare_means(label.y = c(5e6), label = "p.signif") 
graph7
ggsave("new img/graph7.png", graph7, dpi = 1200, width = 10, height = 5)

## ---------- sex, vaccinated, area and signals ----------

sex.area.vaccinated.data <- human.cov.study.data[,c("sex", "area", "ever_vaccinated_COVID", cov_signal_spikes)]
combined.sex.area.vaccinated.data <- melt(sex.area.vaccinated.data, id = c("sex", "area", "ever_vaccinated_COVID"))

graph8 <- ggplot(data = combined.sex.area.vaccinated.data, aes(factor(x = sex), y = value, fill = area)) +
  geom_boxplot(outlier.shape = NA) + theme_bw() +
  geom_point(aes(fill=area), size=0.5, position = position_jitterdodge(jitter.width=0.1)) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = c(1e3,1e4,1e5,1e6), labels = scientific_10, limits = c(1e2, 1e7)) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "", fill = "Legend:") + coord_trans(y = "log10") +
  facet_nested_wrap(~ever_vaccinated_COVID + variable, nrow = 2, labeller = as_labeller(c(vaccinated_labels, cov_signal_labels))) +
  stat_compare_means(label = "p.signif", label.y = c(5e6))
graph8
ggsave("new img/graph8.png", graph8, dpi = 1200, width = 9, height = 7)


## ---------- town, vaccinated, area and signals ----------

town.area.vaccinated.data <- human.cov.study.data[,c("town", "area", "ever_vaccinated_COVID", cov_signal_spikes)]
combined.town.area.vaccinated.data <- melt(town.area.vaccinated.data, id = c("town", "area", "ever_vaccinated_COVID"))

graph9 <- ggplot(data = combined.town.area.vaccinated.data, aes(factor(x = town, level = town_labels_order), y=value, fill=area)) +
  geom_boxplot(outlier.shape = NA) + theme_bw() +
  geom_point(aes(fill=area), size=0.5, position = position_jitterdodge(jitter.width=0.1)) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = c(1e3,1e4,1e5,1e6), labels = scientific_10) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "", fill = "Legend:") + coord_trans(y = "log10") +
  facet_nested_wrap(~ever_vaccinated_COVID + variable,nrow = 2,labeller = as_labeller(c(vaccinated_labels, cov_signal_labels))) 
graph9
ggsave("new img/graph9.png", graph9, dpi = 1200, width = 10, height = 7)

## ---------- age group, vaccinated, area and signals --------------------

agegroup.area.vaccinated.data <- human.cov.study.data[,c("age_group", "area", "ever_vaccinated_COVID", cov_signal_spikes)]
combined.agegroup.area.vaccinated.data <- melt(agegroup.area.vaccinated.data, id = c("age_group", "area", "ever_vaccinated_COVID"))

graph10 <- ggplot(data = combined.agegroup.area.vaccinated.data, aes(factor(x = age_group, level = age_group_order), y = value, fill = area)) +
  geom_boxplot(outlier.shape = NA) + theme_bw() +
  geom_point(aes(fill=area), size=0.5, position = position_jitterdodge(jitter.width=0.1)) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = c(1e3,1e4,1e5,1e6), labels = scientific_10) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "", fill = "Legend:") + coord_trans(y = "log10") +
  facet_nested_wrap(~ever_vaccinated_COVID + variable,nrow = 2,labeller = as_labeller(c(vaccinated_labels, cov_signal_labels))) 
graph10
ggsave("new img/graph10.png", graph10, dpi = 1200, width = 10, height = 7)


# ------------- SIGNALS GROUPED BY VACCINE DOSES ---------------
## ----------- town, area, vaccine doses and signals ----------
town.area.vaccinedoses.data <- human.cov.study.data[,c("town", "area", "vaccine_doses", cov_signal_spikes)]

combined.town.area.vaccinedoses.data <- melt(town.area.vaccinedoses.data, id = c("town", "area", "vaccine_doses"))
combined.town.area.vaccinedoses.data <- na.omit(combined.town.area.vaccinedoses.data)

graph11 <- ggplot(combined.town.area.vaccinedoses.data, aes(factor(x = town, level = town_labels_order), y=value, fill=area)) +
  geom_boxplot(outlier.shape = NA) + theme_bw() +
  geom_point(aes(fill=area), size=0.2, position = position_jitterdodge(jitter.width=0.1)) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = c(1e3,1e4,1e5,1e6), labels = scientific_10) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "", fill = "Legend:") + coord_trans(y = "log10") +
  facet_nested_wrap(~vaccine_doses + variable,nrow = 3,labeller = as_labeller(c(vaccine_dose_labels, cov_signal_labels))) 
graph11
ggsave("new img/graph11.png", graph11, dpi = 1200, width = 8, height = 7)

## ---------- sex, area, vaccine doses and signals ---------------------

sex.area.vaccinedoses.data <- human.cov.study.data[,c("sex", "area", "vaccine_doses", cov_signal_spikes)]

combined.sex.area.vaccinedoses.data <- melt(sex.area.vaccinedoses.data, id = c("sex", "area", "vaccine_doses"))
combined.sex.area.vaccinedoses.data <- na.omit(combined.sex.area.vaccinedoses.data)

graph12 <- ggplot(combined.sex.area.vaccinedoses.data, aes(factor(x = sex), y=value, fill=area)) +
  geom_boxplot(outlier.shape = NA) + theme_bw() +
  geom_point(aes(fill = area), size = 0.2, position = position_jitterdodge(jitter.width=0.1)) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = c(1e3,1e4,1e5,1e6), labels = scientific_10) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "", fill = "Legend:") + coord_trans(y = "log10") +
  facet_nested_wrap(~vaccine_doses + variable, nrow = 3,labeller = as_labeller(c(vaccine_dose_labels, cov_signal_labels))) 
graph12
ggsave("new img/graph12.png", graph12, dpi = 1200, width = 8, height = 8)

## ----------- age group, area, vaccine doses and signals --------------

agegroup.area.vaccinedoses.data <- human.cov.study.data[,c("age_group", "area", "vaccine_doses", cov_signal_spikes)]

combined.agegroup.area.vaccinedoses.data <- melt(agegroup.area.vaccinedoses.data, id = c("age_group", "area", "vaccine_doses"))
combined.agegroup.area.vaccinedoses.data <- na.omit(combined.agegroup.area.vaccinedoses.data)

graph13 <- ggplot(combined.agegroup.area.vaccinedoses.data, aes(factor(x = age_group, level = age_group_order), y = value, fill = area)) +
  geom_boxplot(outlier.shape = NA) + theme_bw() +
  geom_point(aes(fill = area), size = 0.2, position = position_jitterdodge(jitter.width=0.1)) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = c(1e3,1e4,1e5,1e6), labels = scientific_10) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "", fill = "Legend:") + coord_trans(y = "log10") +
  facet_nested_wrap(~vaccine_doses + variable, nrow = 3,labeller = as_labeller(c(vaccine_dose_labels, cov_signal_labels))) 
graph13
ggsave("new img/graph13.png", graph13, dpi = 1200, width = 8, height = 7)

# ---------- STATUS PLOT ----------
## ------------- town, area, and signals ------------------

combined.town.area.signals_spike.data$status <- ifelse(combined.town.area.signals_spike.data$value >= 9630.405, "High", "Low")

graph14 <- ggplot(data = combined.town.area.signals_spike.data, aes(factor(x = town), y=value, fill=status)) +
  theme_bw() +
  geom_point(aes(color = status), size=0.5, position = position_jitter(width = 0.2)) + 
  geom_hline(aes(yintercept=c(9630.405)), lty = "dashed", lwd = 1) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) + coord_trans(y = "log10") +
  scale_y_continuous(breaks = c(1e3,1e4,1e5,1e6), labels = scientific_10) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "") +
  facet_nested_wrap(~variable + area, nrow = 2, scales="free_x",
                    labeller = as_labeller(c(cov_signal_labels, c("Rural" = "Rural", "Urban" = "Urban")))) +
  scale_fill_discrete(labels = c("Positive signal", "Below cutoff for positive", "Cutoff line")) +
  scale_color_discrete(labels = c("Positive signal", "Below cutoff for positive", "Cutoff line"))
graph14
ggsave("new img/graph14.png", graph14, dpi = 1200, width = 9, height = 7)


## ----------------- age group, area and signals -------------------
combined.agegroup.area.signals_spike.data$status <- ifelse(combined.agegroup.area.signals_spike.data$value >= 9630.405, "High", "Low")

graph15 <- ggplot(data = combined.agegroup.area.signals_spike.data, aes(factor(x = age_group, level = age_group_order), y=value, fill=status)) +
  theme_bw() +
  geom_point(aes(color = status), size=0.5, position = position_jitter(width = 0.2)) + 
  geom_hline(aes(yintercept=c(9630.405)), lty = "dashed", lwd = 1) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) + coord_trans(y = "log10") +
  scale_y_continuous(breaks = c(1e3,1e4,1e5,1e6), labels = scientific_10) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "") +
  facet_nested_wrap(~variable + area, nrow = 2, 
                    labeller = as_labeller(c(cov_signal_labels, c("Rural" = "Rural", "Urban" = "Urban")))) +
  scale_fill_discrete(labels=c("Positive signal", "Below cutoff for positive", "Cutoff line")) +
  scale_color_discrete(labels=c("Positive signal", "Below cutoff for positive", "Cutoff line"))
graph15
ggsave("new img/graph15.png", graph15, dpi = 1200, width = 9, height = 7)

## ---------- sex, area and signals -----------

combined.sex.area.signals_spike.data$status <- ifelse(combined.sex.area.signals_spike.data$value >= 9630.405, "High", "Low")

graph16 <- ggplot(data = combined.sex.area.signals_spike.data, aes(factor(x = sex), y = value, fill = status)) +
  theme_bw() +
  geom_point(aes(color = status), size=0.5, position = position_jitter(width = 0.2)) + 
  geom_hline(aes(yintercept=c(9630.405)), lty = "dashed", lwd = 1) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) + coord_trans(y = "log10") +
  scale_y_continuous(breaks = c(1e3,1e4,1e5,1e6), labels = scientific_10) +
  labs(y = "Anti-HCoV IgG (MSD AU/mL)", x = "") +
  facet_nested_wrap(~variable + area, nrow = 2, scales="free_x",
                    labeller = as_labeller(c(cov_signal_labels, c("Rural" = "Rural", "Urban" = "Urban")))) +
  scale_fill_discrete(labels=c("Positive signal", "Below cutoff for positive", "Cutoff line")) +
  scale_color_discrete(labels=c("Positive signal", "Below cutoff for positive", "Cutoff line"))
graph16
ggsave("new img/graph16.png", graph16, dpi = 1200, width = 9, height = 7)

# ---------- NEUTRALIZATION PLOTS ----------
## ---------- between vaccination and area ----------

vaccinated.area.neutralization.data <- human.cov.study.data[,c("area", "ever_vaccinated_COVID", "Neutralization_Titres")]
combined.vaccinated.area.neutralization.data <- melt(vaccinated.area.neutralization.data, id = c("area", "ever_vaccinated_COVID"))

graph17 <- ggplot(data = combined.vaccinated.area.neutralization.data, aes(x = area, y=value, fill=area)) +
  geom_violin(draw_quantiles = c(0.5)) + 
  geom_violin(fill = "transparent", draw_quantiles = c(0.25), linetype = "twodash") +
  geom_violin(fill = "transparent", draw_quantiles = 0.75, linetype = "dashed") + theme_classic() +
  geom_hline(aes(yintercept = c(50)),  color = "red", lty = "dashed", show.legend = FALSE) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold")) + 
  labs(y = "Neutralizing antibody titre", x = "", fill = "Legend:") + 
  facet_nested_wrap(~ever_vaccinated_COVID, nrow = 1, scales = "free", labeller = as_labeller(vaccinated_labels)) +
  scale_x_discrete(labels = c("Rural" = "Rural", "Urban" = "Urban")) +
  scale_y_continuous(breaks = c(10,20,40,80,160,320,640,1280,2560), limits = c(10,3000), trans = "log2") + 
  #scale_y_log10() + coord_trans(y = "log10") + 
  stat_compare_means(label = "p.signif", comparisons = list(c("Rural","Urban"))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank())
graph17
ggsave("new img/graph17.png", graph17, dpi = 1200, width = 5, height = 4)

## ---------- between vaccination and age group ----------

vaccinated.agegroup.neutralization.data <- human.cov.study.data[,c("age_group", "ever_vaccinated_COVID", "Neutralization_Titres")]
combined.vaccinated.agegroup.neutralization.data <- melt(vaccinated.agegroup.neutralization.data, id = c("age_group", "ever_vaccinated_COVID"))

graph18 <- ggplot(data = combined.vaccinated.agegroup.neutralization.data, aes(factor(x = age_group, level = age_group_order), y=value, fill=age_group)) +
  geom_violin(draw_quantiles = c(0.5)) + 
  geom_violin(fill = "transparent", draw_quantiles = c(0.25), linetype = "twodash") +
  geom_violin(fill = "transparent", draw_quantiles = 0.75, linetype = "dashed") + theme_classic() +
  geom_hline(aes(yintercept = c(50)),  color = "red", lty = "dashed", show.legend = FALSE) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust = 0.5), axis.title.y = element_text(face = "bold")) +
  labs(y = "Neutralizing antibody titre", x = "", fill = "Legend:") + 
  facet_nested_wrap(~ever_vaccinated_COVID, nrow = 1, scales = "free_y",
                    labeller = as_labeller(vaccinated_labels)) +
  scale_y_continuous(breaks = c(10,20,40,80,160,320,640,1280,2560), limits = c(10,2560), trans = "log2") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank())
graph18
ggsave("new img/graph18.png", graph18, dpi = 1200, width = 6, height = 4)

## ---------- between vaccination, age group and area ----------

vaccinated.agegroup.area.neutralization.data <- human.cov.study.data[,c("age_group", "ever_vaccinated_COVID", "area", "Neutralization_Titres")]
combined.vaccinated.agegroup.area.neutralization.data <- melt(vaccinated.agegroup.area.neutralization.data, id = c("age_group", "ever_vaccinated_COVID", "area"))

graph19 <- ggplot(data = combined.vaccinated.agegroup.area.neutralization.data, aes(factor(x = age_group, level = age_group_order), y=value, fill=age_group)) +
  geom_violin(draw_quantiles = c(0.5)) + 
  geom_violin(fill = "transparent", draw_quantiles = c(0.25), linetype = "twodash") +
  geom_violin(fill = "transparent", draw_quantiles = 0.75, linetype = "dashed") + theme_classic() +
  geom_hline(aes(yintercept = c(50)),  color = "red", lty = "dashed", show.legend = FALSE) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) + 
  labs(y = "Neutralizing antibody titre", x = "", fill = "Legend:") + 
  facet_nested_wrap(~ever_vaccinated_COVID + area, nrow = 1, scales = "free_y",
                    labeller = as_labeller(c(vaccinated_labels, c("Rural" = "Rural", "Urban" = "Urban")))) +
  scale_y_continuous(breaks = c(10,20,40,80,160,320,640,1280,2560), limits = c(10,2560), trans = "log2") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank())
graph19
ggsave("new img/graph19.png", graph19, dpi = 1200, width = 10, height = 6)

## ---------- between town, age group, area and vaccination ----------

vaccinated.area.town.neutralization.data <- human.cov.study.data[,c("area", "town", "ever_vaccinated_COVID", "Neutralization_Titres")]
combined.vaccinated.area.town.neutralization.data <- melt(vaccinated.area.town.neutralization.data, id = c("area", "town", "ever_vaccinated_COVID"))

graph20 <- ggplot(data = combined.vaccinated.area.town.neutralization.data, aes(factor(x = town, level = town_labels_order), y=value, fill=area)) +
  geom_violin(draw_quantiles = c(0.5)) + 
  geom_violin(fill = "transparent", draw_quantiles = c(0.25), linetype = "twodash") +
  geom_violin(fill = "transparent", draw_quantiles = 0.75, linetype = "dashed") + theme_bw() +
  geom_hline(aes(yintercept = c(50)),  color = "red", lty = "dashed", show.legend = FALSE) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face = "bold")) + 
  labs(y = "Neutralizing antibody titre", x = "", fill = "Legend:") + 
  facet_nested_wrap(~ever_vaccinated_COVID + area, nrow = 1, scales = "free_x",
                    labeller = as_labeller(c(c("Rural" = "Rural", "Urban" = "Urban"), vaccinated_labels))) +
  scale_y_continuous(breaks = c(10,20,40,80,160,320,640,1280,2560), limits = c(10,2560), trans = "log2") 
graph20
ggsave("new img/graph20.png", graph20, dpi = 1200, width = 10, height = 6)


## ---------- Pan CT mean plot ----------
pancov_CTmean.data <- filter(human.cov.study.data, panCoV_CTMean != "Undetermined")
pancov_CTmean.data$panCoV_CTMean <- as.numeric(pancov_CTmean.data$panCoV_CTMean)

graph21 <- ggplot(pancov_CTmean.data, aes(x = panCoV_CTMean)) + 
  geom_histogram(color="black", fill="lightblue", binwidth = 1) + theme_classic() +
  geom_vline(aes(xintercept=c(36)), color = "red", lty = "dashed", lwd = 1) +
  labs(x = "panCoV CT Mean", y = "Count")
graph21
ggsave("new img/graph21.png", graph21, dpi = 1200, width = 4, height = 4)

# ---------- PanCoV PLOTS -----------
## -------------- for towns -------------------------

panCoV_mean <- c("SARS.CoV2_CTMean", "NL63_CTMean", "OC43_CTMean", "229E_CTMean", "HKU1_CTMean")
panCov_statuses <- c("SARS.CoV2_STATUS", "NL63_STATUS", "OC43_STATUS", "229E_STATUS", "HKU1_STATUS")

town.area.pancov_mean.data <- human.cov.study.data[, c("town", "area", panCoV_mean)]

combined.town.area.pancov_mean.data <- melt(town.area.pancov_mean.data, id = c("town", "area"))
combined.town.area.pancov_mean.data <- filter(combined.town.area.pancov_mean.data, value != "Undetermined")
combined.town.area.pancov_mean.data$value <- as.numeric(combined.town.area.pancov_mean.data$value)
combined.town.area.pancov_mean.data$status <- ifelse(combined.town.area.pancov_mean.data$value >= 36, "Negative", "Positive")

graph22 <- ggplot(data = combined.town.area.pancov_mean.data, aes(factor(x = town, town_labels_order), y=value, fill=status)) +
  theme_bw() +
  #geom_point(color='black', fill="orangered", shape = 21, size = 1.5, position = position_jitter(width = 0.2)) +
  geom_point(aes(colour = status), position = position_jitter(width = 0.2)) +
  geom_hline(aes(yintercept=c(36), colour = "red"), lty = "dashed", lwd = 1) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold")) + coord_trans(y = "log10") +
  scale_y_continuous(breaks = c(15,25,35,40), limits = c(15, 45)) +
  labs(y = "Mean CT Value", x = "") +
  facet_nested_wrap(~variable + area, nrow = 1, scales = "free_x",
                    labeller = as_labeller(c("SARS.CoV2_CTMean" = "SARS-CoV2", "OC43_CTMean" = "OC43", "229E_CTMean" = "229E",
                                             "HKU1_CTMean" = "HKU1", "Rural" = "Rural", "Urban" = "Urban"))) +
  scale_fill_discrete(labels=c("Positive", "Below cutoff for positive", "Cutoff line")) +
  scale_color_discrete(labels = c("Positive", "Below cutoff for positive", "Cutoff line"))
graph22
ggsave("new img/graph22.png", graph22, dpi = 1200, width = 9, height = 6)

## ----------------- for age groups ------------------------------

agegroup.area.pancov_mean.data <- human.cov.study.data[, c("age_group", "area", panCoV_mean)]

combined.agegroup.area.pancov_mean.data <- melt(agegroup.area.pancov_mean.data, id = c("age_group", "area"))
combined.agegroup.area.pancov_mean.data <- filter(combined.agegroup.area.pancov_mean.data, value != "Undetermined")
combined.agegroup.area.pancov_mean.data$value <- as.numeric(combined.agegroup.area.pancov_mean.data$value)
combined.agegroup.area.pancov_mean.data$status <- ifelse(combined.agegroup.area.pancov_mean.data$value >= 36, "Negative", "Positive")

graph23 <- ggplot(data = combined.agegroup.area.pancov_mean.data, aes(factor(x = age_group, age_group_order), y=value, fill=status)) +
  theme_bw() +
  #geom_point(color='black', fill="orangered", shape = 21, size = 1.5, position = position_jitter(width = 0.2)) +
  geom_point(aes(color = status, fill = "black"), size=1.5, position = position_jitter(width = 0.2)) + 
  geom_hline(aes(yintercept=c(36), colour = "red"), lty = "dashed", lwd = 1) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold")) + coord_trans(y = "log10") +
  scale_y_continuous(breaks = c(15,25,35,40), limits = c(15, 45)) +
  labs(y = "Mean CT Value", x = "") +
  facet_nested_wrap(~variable, nrow = 1,
                    labeller = as_labeller(c("SARS.CoV2_CTMean" = "SARS-CoV-2", "OC43_CTMean" = "OC43", "229E_CTMean" = "229E",
                                             "HKU1_CTMean" = "HKU1"))) +
  scale_fill_discrete(labels=c("Positive", "Below cutoff for positive", "Cutoff line")) +
  scale_color_discrete(labels=c("Positive", "Below cutoff for positive", "Cutoff line"))
graph23
ggsave("new img/graph23.png", graph23, dpi = 1200, width = 7, height = 6)

## ------------ samples ------------------------ 

samples.pancov_status.data <-filter(human.cov.study.data, panCoV_STATUS == "Positive")[, c("id", panCov_statuses, "Coinfection_STATUS")]
combined.samples.pancov_status.data <- melt(samples.pancov_status.data, id = c("id"))

graph24 <- ggplot(data = combined.samples.pancov_status.data, mapping = aes(factor(x = id), y = variable, fill = value)) +
  geom_tile(color = "white") + theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1, size = 7),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold")) +
  labs(x = "Samples", y = "Coronavirus", fill = "Status:") +
  scale_y_discrete(labels=c("HKU1_STATUS"="HKU1","229E_STATUS"="229E", "OC43_STATUS"="OC43", "NL63_STATUS" = "NL63", "SARS.CoV2_STATUS" = "SARS-CoV-2")) +
  scale_fill_manual(values = c("lightgray", "red"))
graph24
ggsave("new img/graph24.png", graph24, dpi = 1200, width = 10, height = 4, bg = "white")

## ----------- distribution of positives ----------

positive.dist <- filter(combined.samples.pancov_status.data, value == "Positive") |> count(variable, value, sort = FALSE)
positive.dist
positive.dist <- positive.dist |> add_row(variable = "NL63_STATUS", value = "Positive", n = 0)

positive.plot <- ggplot(positive.dist, aes(x = variable, y = n))+
  geom_bar(aes(color = "Positive"), stat = "identity", width = 0.7, position = position_dodge(width = .8), fill = "red") +
  geom_text(aes(label = n), position = position_dodge(width = .8), hjust = -0.5, vjust = 0, size = 3) +
  theme_minimal() + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"), legend.title=element_blank()) + 
  labs(y = "Counts", fill = "") + coord_flip() + 
  scale_x_discrete(limits = rev(c("HKU1_STATUS", "229E_STATUS", "OC43_STATUS", "NL63_STATUS", "SARS.CoV2_STATUS"))) +
  scale_y_continuous(limits = c(0, 30)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())
positive.plot

combined_graph3 <- grid.arrange(graph24, positive.plot, nrow = 1, ncol = 2, widths=c(0.9, 0.1))
combined_graph3
ggsave("new img/combined_plot3.png", combined_graph3, dpi = 1200, width = 10, height = 3, bg = "white")

# -------------------- VACCINATION TYPES -------------------------
# By counts 

vaccination_types <- c("vaccine_astrazeneca", "vaccine_pfizer", "vaccine_johnsonjohnson", "vaccine_moderna", "vaccine_unknown", "vaccine_other")

combined.vaccinations.data <- melt(human.cov.study.data[,vaccination_types])
combined.vaccinations.data$value <- as.factor(combined.vaccinations.data$value)

graph25 <- ggplot(combined.vaccinations.data, aes(x = variable, fill = value)) + 
  geom_bar(stat = "count", position = position_dodge()) + theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face = "bold")) +
  labs(x = "", y = "Frequency", title = "Vaccine taken", fill = "") +
  scale_x_discrete(labels = c("Astrazeneca", "Pfizer", "Johnsonjohnson", "Moderna", "Unknown", "Other")) +
  scale_fill_discrete(labels = c("0" = "No", "1" = "Yes"))
graph25
ggsave("new img/graph25.png", graph25, dpi = 1200, width = 4, height = 4, bg = "white")

# By rural and urban

vaccination_types <- c("vaccine_astrazeneca", "vaccine_pfizer", "vaccine_johnsonjohnson", "vaccine_moderna", "vaccine_unknown", "vaccine_other")

combined.area.vaccinetype.data <- melt(human.cov.study.data[,c("area",vaccination_types)], id = c("area"))
combined.area.vaccinetype.data <- filter(combined.area.vaccinetype.data, value == 1)

graph26 <- ggplot(combined.area.vaccinetype.data, aes(x = variable, fill = area)) + 
  geom_bar(stat = "count", position = position_dodge(preserve = "single"), color = "black") + theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5), axis.title.y = element_text(face = "bold")) +
  labs(x = "", y = "Frequency", title = "Vaccine taken", fill = "Area: ") +
  scale_x_discrete(labels = c("Astrazeneca", "Pfizer", "Johnsonjohnson", "Moderna", "Don't Know", "Other")) +
  scale_fill_discrete(labels = c("rural" = "Rural", "urban" = "Urban"))
graph26
ggsave("new img/graph26.png", graph26, dpi = 1200, width = 4, height = 5, bg = "white")


# ---------- Pie plot ---------

percentage.positive.values <- filter(combined.samples.pancov_status.data, value == "Positive") |> 
  count(variable = factor(variable), value = factor(value))

percentage.positive.values[1,c("n")] <- percentage.positive.values[1,c("n")] - 2 
percentage.positive.values[3,c("n")] <- percentage.positive.values[3,c("n")] - 2

percentage.positive.values <- percentage.positive.values |> add_row(variable = "NL63", value = "Positive", n = 0) |>
  add_row(variable = "unknown", value = "Positive", n = 32)
percentage.positive <- percentage.positive.values |> mutate(pct = prop.table(n))
overall.percentage <- percentage.positive.values |> mutate(pct = n/1000) 

graph27 <- ggplot(percentage.positive, aes(as.factor(1), y = pct, fill = variable, label = scales::percent(pct))) +
  geom_bar(stat = "identity", width = 1) + theme_void() +
  geom_label_repel(aes(x = 1, label = scales::percent(pct)), position = position_stack(vjust = .5), show.legend = FALSE,
                   min.segment.length = unit(0, 'lines')) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face = "bold")) +
  labs(x = "", y = "", title = "Percentage positive") +
  scale_fill_discrete(name = "", labels=c("HKU1_STATUS"="HKU1","229E_STATUS"="229E", "OC43_STATUS"="OC43", "NL63_STATUS" = "NL63", "SARS.CoV2_STATUS" = "SARS-CoV-2",
                                          "unknown" = "Unknown PanCov Positive", "Coinfection_STATUS" = "Co-infection")) +
  coord_polar("y") 
graph27
ggsave("new img/graph27.png", graph27, dpi = 1200, width = 6, height = 4, bg = "white")

# ----------- Corresponding bar plot -------------------------
# with percentage labels
pancov_order <- c("SARS.CoV2_STATUS", "NL63", "229E_STATUS", "HKU1_STATUS", "OC43_STATUS", "Coinfection_STATUS", "unknown")

graph28 <- ggplot(percentage.positive, aes(factor(x = variable, level = pancov_order), y = pct*100, fill = variable, label = scales::percent(pct))) +
  geom_bar(stat = "identity", width = 0.5) + theme_bw() +
  geom_text(position = position_dodge(preserve = "single", width = 0), vjust = -1, hjust = 0.5) +
  annotate(geom = "text", x = 4, y = 25, label = "% PanCoV positive: 6.7%", color = "black") +
  theme(legend.position = "none", legend.direction = "horizontal",
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face = "bold")) + 
  labs(x = "", y = "Percentage positive") +
  scale_y_continuous(limits = c(0,35)) +
  scale_x_discrete(labels=c("HKU1_STATUS" = "HKU1","229E_STATUS"="229E", "OC43_STATUS"="OC43", "NL63_STATUS" = "NL63", "SARS.CoV2_STATUS" = "SARS-CoV-2", 
                            "unknown" = "Unknown\n PanCov\n Positive", "Coinfection_STATUS" = "Co-infection")) 
#scale_fill_discrete(name = "", labels=c("HKU1_STATUS"="HKU1","X229E_STATUS"="X229E", "OC43_STATUS"="OC43", "NL63_STATUS" = "NL63", "SARS.CoV2_STATUS" = "SARS.CoV2")) 
graph28
ggsave("new img/graph28.png", graph28, dpi = 1200, width = 5, height = 5, bg = "white")

# with text labels
pancov_order <- c("SARS.CoV2_STATUS", "NL63", "229E_STATUS", "HKU1_STATUS", "OC43_STATUS", "Coinfection_STATUS", "unknown")

graph29 <- ggplot(percentage.positive, aes(factor(x = variable, level = pancov_order), y = n, fill = variable, label = n)) +
  geom_bar(stat = "identity", width = 0.5) + theme_bw() +
  geom_text(position = position_dodge(preserve = "single", width = 0), vjust = -1, hjust = 0.5) +
  annotate(geom = "text", x = 4, y = 21, label = "PanCoV positive: 67", color = "black") +
  theme(legend.position = "none", legend.direction = "horizontal",
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face = "bold")) + 
  labs(x = "", y = "No. of positive cases") +
  scale_y_continuous(limits = c(0,35)) +
  scale_x_discrete(labels=c("HKU1_STATUS" = "HKU1","229E_STATUS"="229E", "OC43_STATUS"="OC43", "NL63_STATUS" = "NL63", "SARS.CoV2_STATUS" = "SARS-CoV-2", 
                            "unknown" = "Unknown\n PanCov\n Positive", "Coinfection_STATUS" = "Co-infection")) 
#scale_fill_discrete(name = "", labels=c("HKU1_STATUS"="HKU1","X229E_STATUS"="X229E", "OC43_STATUS"="OC43", "NL63_STATUS" = "NL63", "SARS.CoV2_STATUS" = "SARS.CoV2")) 
graph29
ggsave("new img/graph29.png", graph29, dpi = 1200, width = 5, height = 5, bg = "white")


graph30 <- ggplot(overall.percentage, aes(factor(x = variable, level = pancov_order), y = pct*100, fill = variable, label = scales::percent(pct))) +
  geom_bar(stat = "identity", width = 0.5) + theme_classic() +
  geom_text(position = position_dodge(preserve = "single", width = 0), vjust = -1, hjust = 0.5) +
  annotate(geom = "text", x = 4, y = 3, label = "% PanCoV positive: 6.7%", color = "black") +
  theme(legend.position = "none", legend.direction = "horizontal",
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face = "bold")) + 
  labs(x = "", y = "Percentage positive") +
  scale_y_continuous(limits = c(0,4), expand = c(0,0)) +
  scale_x_discrete(labels=c("HKU1_STATUS" = "HKU1","229E_STATUS"="229E", "OC43_STATUS"="OC43", "NL63_STATUS" = "NL63", "SARS.CoV2_STATUS" = "SARS-CoV-2", 
                            "unknown" = "Unknown\n PanCov\n Positive", "Coinfection_STATUS" = "Co-infection")) 
#scale_fill_discrete(name = "", labels=c("HKU1_STATUS"="HKU1","X229E_STATUS"="X229E", "OC43_STATUS"="OC43", "NL63_STATUS" = "NL63", "SARS.CoV2_STATUS" = "SARS.CoV2")) 
graph30
ggsave("new img/graph30.png", graph30, dpi = 1200, width = 5, height = 5, bg = "white")

# ----------- Symptoms -----------

symptoms.data <- filter(human.cov.study.data, panCoV_STATUS == "Positive") |> 
  select(colnames(human.cov.study.data[24:33]))
all.symptoms.counts.data <- symptoms.data |> 
  pivot_longer(col = colnames(human.cov.study.data[24:33])) |> 
  count(name, value)
all.symptoms.counts.data$value <- ifelse(all.symptoms.counts.data$value == "1", "Yes", "No")

graph31 <- ggplot(all.symptoms.counts.data) +
  geom_col(aes(x = n, y = name, fill = value), width = 0.6) +
  theme_bw()+
  #geom_bar(stat = "identity", color = "black", position = position_dodge(preserve = "single")) + theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold")) +
  labs(x = "No. of cases", y = "", title = "Symptoms of PanCoV Positive Individuals", fill = "Legend:") +
  scale_y_discrete(breaks = colnames(human.cov.study.data[24:33]), 
                   labels = c("Cough", "Fever chills", "Sore throat", "Shortness \n of breath", "Muscle/body \n aches", "Headaches", "Taste/smell \n loss", "Congestion \n runnynose",
                              "Nausea/ \n Vomitting", "Diarrhea"))
graph31
ggsave("new img/graph31.png", graph31, dpi = 1200, width = 6, height = 6, bg = "white")


# ------------ New plots -----------

graph32 <- ggplot(human.cov.study.data |> count(area, panCoV_STATUS, sort = FALSE), aes(x = area, y = n, fill = panCoV_STATUS)) +
  geom_bar(position="dodge", stat = "identity", color = "black") +
  #geom_text(aes(label = n), size = 2.5, vjust=-0.4, position = position_dodge(0.7))+
  labs(y = "Number of participants", x = "", fill = "PanCoV Status: ", face = "bold") +
  theme_bw() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
                     plot.title = element_text(hjust = 0.5, face = "bold")) 
graph32
ggsave("new img/graph32.png", graph32, dpi = 1200, width = 5, height = 5, bg = "white")


graph33 <- ggplot(human.cov.study.data |> count(age_group, panCoV_STATUS, sort = FALSE), aes(x = age_group, y = n, fill = panCoV_STATUS)) +
  geom_bar(position="dodge", stat = "identity", color = "black") +
  #geom_text(aes(label = n), size = 2.5, vjust=-0.4, position = position_dodge(0.7))+
  labs(y = "Number of participants", x = "", fill = "PanCoV Status: ", face = "bold") +
  theme_classic() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
                          plot.title = element_text(hjust = 0.5, face = "bold"))
graph33
ggsave("new img/graph33.png", graph33, dpi = 1200, width = 5, height = 5, bg = "white")


graph34 <- ggplot(human.cov.study.data |> select(town, panCoV_STATUS, area), aes(factor(x = town, level = town_labels_order), fill = panCoV_STATUS)) +
  geom_bar(stat = "count", position = position_dodge(preserve = "single"), color = "black") +
  #geom_text(aes(label = n), size = 2.5, vjust=-0.4, position = position_dodge(0.7))+
  labs(y = "Number of participants", x = "", fill = "PanCoV Status: ", face = "bold") +
  theme_classic() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
                          plot.title = element_text(hjust = 0.5, face = "bold")) +
  facet_nested_wrap(~area, scales = "free_x") 
graph34
ggsave("new img/graph34.png", graph34, dpi = 1200, width = 5, height = 5, bg = "white")


pancov.neutralization.data <- human.cov.study.data[,c("panCoV_STATUS", "Neutralization_Titres")]

graph35 <- ggplot(data = pancov.neutralization.data, aes(x = panCoV_STATUS, y=Neutralization_Titres, fill=panCoV_STATUS)) +
  geom_violin(draw_quantiles = c(0.5)) + 
  geom_violin(fill = "transparent", draw_quantiles = c(0.25), linetype = "twodash") +
  geom_violin(fill = "transparent", draw_quantiles = 0.75, linetype = "dashed") + theme_classic() +
  geom_point(aes(fill=panCoV_STATUS), size=0.5,position = position_jitterdodge(jitter.width=0.5)) +
  geom_hline(aes(yintercept = c(50)),  color = "red", lty = "dashed", show.legend = FALSE) +
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust=0.5), axis.title.y = element_text(face="bold")) + 
  labs(y = "Neutralizing antibody titre", x = "PanCoV Status", fill = "Legend:") + 
  scale_x_discrete(labels = c("rural" = "Rural", "urban" = "Urban")) +
  scale_y_continuous(breaks = c(10,20,40,80,160,320,640,1280,2560), limits = c(10,3000), trans = "log2")  
#scale_y_log10() + coord_trans(y = "log10") +
graph35
ggsave("new img/graph35.png", graph35, dpi = 1200, width = 5, height = 5, bg = "white")


human.cov.study.data$neu_cutoff <- ifelse(human.cov.study.data$Neutralization_Titres >= 50, "High", "Low")

graph36 <- ggplot(na.omit(human.cov.study.data |> count(panCoV_STATUS, neu_cutoff, sort = FALSE)), 
                  aes(x = neu_cutoff, y = n, fill = panCoV_STATUS)) +
  geom_bar(position="dodge", stat = "identity", color = "black") +
  annotate(geom = "text", x = 1.5, y = 600, label = TeX("$chi^{2}$ = 4.2807, $p$ = 0.0386"), color = "black") +
  labs(y = "Frequency", x = "Neutralization Titre", fill = "PanCoV Status: ", face = "bold") +
  theme_classic() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
                          plot.title = element_text(hjust = 0.5, face = "bold"))
graph36
ggsave("new img/graph36.png", graph36, dpi = 1200, width = 5, height = 5, bg = "white")


ggplot(pancov.neutralization.data, aes(x = Neutralization_Titres)) +
  geom_density(fill = "skyblue", alpha = 0.7)



# -------- Regression --------------------------

human.cov.study.data$panCoV_STATUS <- as.factor(human.cov.study.data$panCoV_STATUS)

human.cov.study.data |> select(panCoV_STATUS, `fever/chills`, headaches, cough)

tbl_uvregression(human.cov.study.data |> select(panCoV_STATUS, `fever/chills`, headaches, cough, sore_throat, runny_nose,
                                                body_ache, sob),
                 method = glm,
                 y = panCoV_STATUS,
                 method.args = list(family = binomial),
                 exponentiate = TRUE,
                 include = c( `fever/chills`, headaches, cough, sore_throat, runny_nose, body_ache, sob) )


tbl_summary(human.cov.study.data |> select(colnames(human.cov.study.data[24:33]), panCoV_STATUS), 
            by = panCoV_STATUS,
            percent = "cell",
            type = all_dichotomous() ~ "categorical") |> add_p() |> bold_labels() -> table1
table1


table1 |> as_flex_table() |>
  set_table_properties(width = 1, layout = "autofit") |>
  save_as_docx(path = "table1.docx") 

table1 |> as_flex_table() |>
  save_as_image(path = "table1.png", width = 6, height = 6, units = "in", res = 200) 
