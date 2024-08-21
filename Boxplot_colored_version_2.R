
# Gene_expression_analysis_box plots for Cyp6p3, Cyp6m2 and Cyp6aa1
# 28/07/2024
# box plots



#pacman::p_load(rio,here,tidyverse,patchwork)


#load packages
library(readxl)
library(ggplot2)
library(ggsignif)
library(patchwork)

#working directory
setwd("C:/Users/justi/OneDrive/Desktop/Re_ Research project results")

library(readxl)
age_mosq <- read_excel("gene_expression_data.xlsx",
                       sheet = "Age of the mosquito condition")
View(gene_expression_data)
###################
# Eric Suggestion #
###################
# The object gene_expression _data hasn't been created yet, so the above line doesn't work
###################

age_mosq<- read_excel("gene_expression_data.xlsx",
                      sheet = "Age of the mosquito condition")


blood_feeding <- read_excel("gene_expression_data.xlsx",
                            sheet = "Blood feeding status condition")

time_conditon <- read_excel("gene_expression_data.xlsx",
                            sheet = "Time of the day condition")


# create box plot for Cyp6p3--------------------------------------------------


age_mosq$Sample_Time <- paste(age_mosq$`Sample Name`, age_mosq$`Time of the day`, sep = " ")
# Ensure the order of levels for Sample_Time is set as desired
age_mosq$Sample_Time <- factor(age_mosq$Sample_Time, levels = c("Kisumu 2 Days", "Tiassale 2 Days", "Kisumu 5 Days", "Tiassale 5 Days"),
                               labels = c("Kis 2", "Tia 2", "Kis 5", "Tia 5"))
###################
# Eric suggestion #
###################
# Your code is good and correst. Just as a suggestion, you made find it useful and neater to use
# Magritt pipes to chain together commands on the same object. In the situation above, where you 
# create "Sample_Time" and then modify "Sample_Time", you can use the %>% symbol to chain those
# commands together. This is called piping, and looks like this (and give the exact same result
# as your code):
#library(magrittr)
#age_mosq$Sample_Time <- paste(age_mosq$`Sample Name`, age_mosq$`Time of the day`, sep = " ") %>%
#                        factor(levels = c("Kisumu 2 Days", "Tiassale 2 Days", "Kisumu 5 Days", "Tiassale 5 Days"))
###################



# Plot for age_mosq dataset with multiple comparisons
age_cyp6p3 <- ggplot(data = age_mosq,
                     aes(x = Sample_Time, y = Cyp6p3, fill = Sample_Time)) + # Added fill aesthetic
  geom_boxplot() +
  labs(x = "", y = 'Cyp6p3 ΔCt', title = "Age of mosquito") +
  geom_signif(comparisons = list(
    c("Kis 2", "Tia 2"),
    c("Kis 5", "Tia 5"),
    c("Tia 2", "Kis 5"),
    c("Kis 2", "Kis 5"),
    c("Tia 2", "Tia 5"),
    c("Kis 2", "Tia 5")),
    map_signif_level = TRUE, step_increase = 0.1) +  scale_fill_manual(values = c("Kis 2" = "lightblue", "Kis 5" = "lightblue",
                                                                                  "Tia 2" = "cornflowerblue", "Tia 5" = "cornflowerblue")) +
  theme_minimal() + theme(legend.position = "none")

###################
# Eric suggestion #
###################
# If you use the "geom_signif" function as above, it will do its own statistical test for significance
# (by default, it performs a Wilcoxon test), which might not produce the same result (and significance
# levels) as your own analysis. Have you checked that all the significance levels are the same as your
# own ones?
###################

ggsave(filename = "age_cyp6p3.png", plot = age_cyp6p3, device = "png", dpi = 300, width = 10, height = 7)

# Print the plot
print(age_cyp6p3)




#blood feeding status v gene expression


blood_feeding$Sample_Blood <- paste(blood_feeding$`Sample Name`, blood_feeding$`Blood-feeding status`, sep = " ")

# Ensure the order of levels for Sample_Blood is set as desired
blood_feeding$Sample_Blood <- factor(blood_feeding$Sample_Blood, levels = c("Kisumu Blood-fed", "Tiassale Blood-fed", "Kisumu Non-blood-fed", "Tiassale Non-blood-fed"),
                                     labels = c("Kis BF", "Tia BF", "Kis NBF", "Tia NBF"))

# Plot for blood_feeding dataset with multiple comparisons
blood_cyp6p3 <- ggplot(data = blood_feeding,
                       aes(x = Sample_Blood, y = Cyp6p3, fill = Sample_Blood)) + # Added fill aesthetic
  geom_boxplot() +
  labs(x = "", y = 'Cyp6p3 ΔCt', title = "Blood feeding status") +
  geom_signif(comparisons = list(
    c("Kis BF", "Tia BF"),
    c("Kis NBF", "Tia NBF"),
    c("Tia BF", "Kis NBF"),
    c("Kis BF", "Kis NBF"),
    c("Tia BF", "Tia NBF"),
    c("Kis BF", "Tia NBF")),
    map_signif_level = TRUE, step_increase = 0.1) +  scale_fill_manual(values = c("Kis BF" = "red", "Kis NBF" = "red",
                                                                                  "Tia BF" = "darkred", "Tia NBF" = "darkred")) +
  theme_minimal() + theme(legend.position = "none")

# Print the plot
print(blood_cyp6p3)


time_conditon$Sample_Time <- paste(time_conditon$`Sample Name`, time_conditon$`Time of the day`, sep = " ")
# Ensure the order of levels for Sample_Time is set as desired
time_conditon$Sample_Time <- factor(time_conditon$Sample_Time, levels = c("Kisumu 6AM", "Tiassale 6AM", "Kisumu 6PM", "Tiassale 6PM"),
                                    labels = c("Kis 6AM", "Tia 6AM", "Kis 6PM", "Tia 6PM"))


# Plot for time_conditon dataset with multiple comparisons
time_cyp6p3 <- ggplot(data = time_conditon,
                      aes(x = Sample_Time, y = Cyp6p3, fill = Sample_Time)) + # Added fill aesthetic
  geom_boxplot() +
  labs(x = "", y = 'Cyp6p3 ΔCt', title = "Time of the day") +
  geom_signif(comparisons = list(
    c("Kis 6AM", "Tia 6AM"),
    c("Kis 6PM", "Tia 6PM"),
    c("Tia 6AM", "Kis 6PM"),
    c("Kis 6AM", "Kis 6PM"),
    c("Tia 6AM", "Tia 6PM"),
    c("Kis 6AM", "Tia 6PM")),
    map_signif_level = TRUE, step_increase = 0.1) +  scale_fill_manual(values = c("Kis 6AM" = "chartreuse1", "Kis 6PM" = "chartreuse1",
                                                                                  "Tia 6AM" = "chartreuse4", "Tia 6PM" = "chartreuse4")) +
  theme_minimal() + theme(legend.position = "none")

# Print the plot
print(time_cyp6p3)


combined_cyp6p3 <- age_cyp6p3 + blood_cyp6p3 + time_cyp6p3

print(combined_cyp6p3)





# the mosquito v gene expression Cyp6m2 boxplots-----------------------------------------------------


age_mosq$Sample_Time <- paste(age_mosq$`Sample Name`, age_mosq$`Time of the day`, sep = " ")

# Ensure the order of levels for Sample_Time is set as desired
age_mosq$Sample_Time <- factor(age_mosq$Sample_Time, levels = c("Kisumu 2 Days", "Tiassale 2 Days", "Kisumu 5 Days", "Tiassale 5 Days"),
                               labels = c("Kis 2", "Tia 2", "Kis 5", "Tia 5"))


# Plot for age_mosq dataset with multiple comparisons
age_cyp6m2 <- ggplot(data = age_mosq,
                     aes(x = Sample_Time, y = Cyp6m2, fill = Sample_Time)) + # Added fill aesthetic
  geom_boxplot() +
  labs(x = "", y = 'Cyp6m2 ΔCt', title = " Age of the mosquito") +
  geom_signif(comparisons = list(
    c("Kis 2", "Tia 2"),
    c("Kis 5", "Tia 5"),
    c("Tia 2", "Kis 5"),
    c("Kis 2", "Kis 5"),
    c("Tia 2", "Tia 5"),
    c("Kis 2", "Tia 5")),
    map_signif_level = TRUE, step_increase = 0.1) +  scale_fill_manual(values = c("Kis 2" = "lightblue", "Kis 5" = "lightblue",
                                                                                  "Tia 2" = "cornflowerblue", "Tia 5" = "cornflowerblue")) +
  theme_minimal() + theme(legend.position = "none")

# Print the plot
print(age_cyp6m2)


#Blood feeding status v gene expression Cyp6m2


blood_feeding$Sample_Blood <- paste(blood_feeding$`Sample Name`, blood_feeding$`Blood-feeding status`, sep = " ")
# Ensure the order of levels for Sample_Blood is set as desired
blood_feeding$Sample_Blood <- factor(blood_feeding$Sample_Blood, levels = c("Kisumu Blood-fed", "Tiassale Blood-fed", "Kisumu Non-blood-fed", "Tiassale Non-blood-fed"),
                                     labels = c("Kis BF", "Tia BF", "Kis NBF", "Tia NBF"))

# Plot for blood_feeding dataset with multiple comparisons
blood_cyp6m2 <- ggplot(data = blood_feeding,
                       aes(x = Sample_Blood, y = Cyp6m2, fill = Sample_Blood)) + # Added fill aesthetic
  geom_boxplot() +
  labs(x = "", y = 'Cyp6m2 ΔCt', title = "Blood feeding status") +
  geom_signif(comparisons = list(
    c("Kis BF", "Tia BF"),
    c("Kis NBF", "Tia NBF"),
    c("Tia BF", "Kis NBF"),
    c("Kis BF", "Kis NBF"),
    c("Tia BF", "Tia NBF"),
    c("Kis BF", "Tia NBF")),
    map_signif_level = TRUE, step_increase = 0.1) +  scale_fill_manual(values = c("Kis BF" = "red", "Kis NBF" = "red",
                                                                                  "Tia BF" = "darkred", "Tia NBF" = "darkred")) +
  theme_minimal() + theme(legend.position = "none")


# Print the plot
print(blood_cyp6m2)



#Time of the day v gene expression Cyp6m2


time_conditon$Sample_Time <- paste(time_conditon$`Sample Name`, time_conditon$`Time of the day`, sep = " ")
# Ensure the order of levels for Sample_Time is set as desired
time_conditon$Sample_Time <- factor(time_conditon$Sample_Time, levels = c("Kisumu 6AM", "Tiassale 6AM", "Kisumu 6PM", "Tiassale 6PM"),
                                    labels = c("Kis 6AM", "Tia 6AM", "Kis 6PM", "Tia 6PM"))


# Plot for time_conditon dataset with multiple comparisons
time_cyp6m2 <- ggplot(data = time_conditon,
                      aes(x = Sample_Time, y = Cyp6m2, fill = Sample_Time)) + # Added fill aesthetic
  geom_boxplot() +
  labs(x = "", y = 'Cyp6m2 ΔCt', title = "Time of the day") +
  geom_signif(comparisons = list(
    c("Kis 6AM", "Tia 6AM"),
    c("Kis 6PM", "Tia 6PM"),
    c("Tia 6AM", "Kis 6PM"),
    c("Kis 6AM", "Kis 6PM"),
    c("Tia 6AM", "Tia 6PM"),
    c("Kis 6AM", "Tia 6PM")),
    map_signif_level = TRUE, step_increase = 0.1) +  scale_fill_manual(values = c("Kis 6AM" = "chartreuse1", "Kis 6PM" = "chartreuse1",
                                                                                  "Tia 6AM" = "chartreuse4", "Tia 6PM" = "chartreuse4")) +
  theme_minimal() + theme(legend.position = "none")

# Print the plot
print(time_cyp6m2)


combined_cyp6m2 <- age_cyp6m2 + blood_cyp6m2 + time_cyp6m2

print(combined_cyp6m2)



# the mosquito v gene expression Cyp6aa1--------------------------------------------------------


age_mosq$Sample_Time <- paste(age_mosq$`Sample Name`, age_mosq$`Time of the day`, sep = " ")
# Ensure the order of levels for Sample_Time is set as desired
age_mosq$Sample_Time <- factor(age_mosq$Sample_Time, levels = c("Kisumu 2 Days", "Tiassale 2 Days", "Kisumu 5 Days", "Tiassale 5 Days"),
                               labels = c("Kis 2", "Tia 2", "Kis 5", "Tia 5"))

# Plot for age_mosq dataset with multiple comparisons
age_cyp6aa1 <- ggplot(data = age_mosq,
                      aes(x = Sample_Time, y = Cyp6aa1, fill = Sample_Time)) + # Added fill aesthetic
  geom_boxplot() +
  labs(x = "", y = 'Cyp6aa1 ΔCt', title = "Age of the mosquito") +
  geom_signif(comparisons = list(
    c("Kis 2", "Tia 2"),
    c("Kis 5", "Tia 5"),
    c("Tia 2", "Kis 5"),
    c("Kis 2", "Kis 5"),
    c("Tia 2", "Tia 5"),
    c("Kis 2", "Tia 5")),
    map_signif_level = TRUE, step_increase = 0.1) +  scale_fill_manual(values = c("Kis 2" = "lightblue", "Kis 5" = "lightblue",
                                                                                  "Tia 2" = "cornflowerblue", "Tia 5" = "cornflowerblue")) +
  theme_minimal() + theme(legend.position = "none")

# Print the plot
print(age_cyp6aa1)



# the mosquito v gene expression Cyp6aa1


blood_feeding$Sample_Blood <- paste(blood_feeding$`Sample Name`, blood_feeding$`Blood-feeding status`, sep = " ")
# Ensure the order of levels for Sample_Blood is set as desired
blood_feeding$Sample_Blood <- factor(blood_feeding$Sample_Blood, levels = c("Kisumu Blood-fed", "Tiassale Blood-fed", "Kisumu Non-blood-fed", "Tiassale Non-blood-fed"),
                                     labels = c("Kis BF", "Tia BF", "Kis NBF", "Tia NBF"))

# Plot for blood_feeding dataset with multiple comparisons
blood_cyp6aa1 <- ggplot(data = blood_feeding,
                        aes(x = Sample_Blood, y = Cyp6aa1, fill = Sample_Blood)) + # Added fill aesthetic
  geom_boxplot() +
  labs(x = "", y = 'Cyp6aa1 ΔCt', title = "Blood feeding status") +
  geom_signif(comparisons = list(
    c("Kis BF", "Tia BF"),
    c("Kis NBF", "Tia NBF"),
    c("Tia BF", "Kis NBF"),
    c("Kis BF", "Kis NBF"),
    c("Tia BF", "Tia NBF"),
    c("Kis BF", "Tia NBF")),
    map_signif_level = TRUE, step_increase = 0.1) + scale_fill_manual(values = c("Kis BF" = "red", "Kis NBF" = "red",
                                                                                 "Tia BF" = "darkred", "Tia NBF" = "darkred")) +
  theme_minimal() + theme(legend.position = "none")

# Print the plot
print(blood_cyp6aa1)



#Time of the day v gene expression Cyp6aa1


time_conditon$Sample_Time <- paste(time_conditon$`Sample Name`, time_conditon$`Time of the day`, sep = " ")
# Ensure the order of levels for Sample_Time is set as desired
time_conditon$Sample_Time <- factor(time_conditon$Sample_Time, levels = c("Kisumu 6AM", "Tiassale 6AM", "Kisumu 6PM", "Tiassale 6PM"),
                                    labels = c("Kis 6AM", "Tia 6AM", "Kis 6PM", "Tia 6PM"))

# Plot for time_conditon dataset with multiple comparisons
time_cyp6aa1 <- ggplot(data = time_conditon,
                       aes(x = Sample_Time, y = Cyp6aa1, fill = Sample_Time)) + # Added fill aesthetic
  geom_boxplot() +
  labs(x = "", y = 'Cyp6aa1 ΔCt', title = "Time of the day") +
  geom_signif(comparisons = list(
    c("Kis 6AM", "Tia 6AM"),
    c("Kis 6PM", "Tia 6PM"),
    c("Tia 6AM", "Kis 6PM"),
    c("Kis 6AM", "Kis 6PM"),
    c("Tia 6AM", "Tia 6PM"),
    c("Kis 6AM", "Tia 6PM")),
    map_signif_level = TRUE, step_increase = 0.1) + scale_fill_manual(values = c("Kis 6AM" = "chartreuse1", "Kis 6PM" = "chartreuse1",
                                                                                  "Tia 6AM" = "chartreuse4", "Tia 6PM" = "chartreuse4")) +
  theme_minimal() + theme(legend.position = "none")

# Print the plot
print(time_cyp6aa1)


combined_cyp6aa1 <- age_cyp6aa1 + blood_cyp6aa1 + time_cyp6aa1

# final combined plots

combined_cyp6p3/combined_cyp6m2/combined_cyp6aa1






