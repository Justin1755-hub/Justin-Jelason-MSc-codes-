# gene_expression_analysis
# 28/07/2024
# box



#pacman::p_load(rio,here,tidyverse,patchwork)


#load packages
library(readxl)
library(ggplot2)
library(ggsignif)
library(patchwork)

#working directory
setwd("C:/Users/justi/OneDrive/Desktop/Re_ Research project results")


age_mosq<- read_excel("gene_expression_data.xlsx",
                      sheet = "Age of the mosquito condition")


blood_feeding <- read_excel("gene_expression_data.xlsx",
                                   sheet = "Blood feeding status condition")

time_conditon <- read_excel("gene_expression_data.xlsx",
                                   sheet = "Time of the day condition")


# create box plot

# creates separate plots
# # age versus the gene expresion plot
# age_cyp6p3 <- ggplot(data = age_mosq,
#        aes(
#        x = `Sample Name`,
#        y = Cyp6p3)) +
#   geom_boxplot(color = "red") +
#   facet_wrap(~`Time of the day`) +
#   labs(x = "", y = 'Cyp6p3 ΔCt',
#        title = "Age of mosquito") +
#   geom_signif(comparisons = list(c("Kisumu", "Tiassale")),
#               map_signif_level = TRUE)
#
# print(age_cyp6p3)


age_mosq$Sample_Time <- paste(age_mosq$`Sample Name`, age_mosq$`Time of the day`, sep = " ")
# Ensure the order of levels for Sample_Time is set as desired
age_mosq$Sample_Time <- factor(age_mosq$Sample_Time, levels = c("Kisumu 2 Days", "Tiassale 2 Days", "Kisumu 5 Days", "Tiassale 5 Days"))

# Plot for age_mosq dataset with multiple comparisons
age_cyp6p3 <- ggplot(data = age_mosq,
                     aes(x = Sample_Time, y = Cyp6p3)) +
  geom_boxplot(color = "red") +
  labs(x = "", y = 'Cyp6p3 ΔCt', title = "Age of mosquito") +
  geom_signif(comparisons = list(
    c("Kisumu 2 Days", "Tiassale 2 Days"),
    c("Kisumu 5 Days", "Tiassale 5 Days"),
    c("Tiassale 2 Days", "Kisumu 5 Days"),
    c("Tiassale 2 Days", "Tiassale 5 Days"),
    c("Kisumu 2 Days", "Kisumu 5 Days"),
    c("Kisumu 2 Days", "Tiassale 5 Days")),
    map_signif_level = TRUE, step_increase = 0.1)

# Print the plot
print(age_cyp6p3)



#blood feeding status v gene expression
blood_cyp6p3 <- ggplot(data = blood_feeding,
       aes(
         x = `Sample Name`,
         y = Cyp6p3)) +
  geom_boxplot(color = "blue") +
  facet_wrap(~`Blood-feeding status`) +
  labs(x = "Mosquito Name",y = 'Cyp6p3 ΔCt',
       title = "Blood feeding status") +
  geom_signif(comparisons = list(c("Kisumu", "Tiassale")),
              map_signif_level = TRUE)

print(blood_cyp6p3)



#time of the day v gene expression
time_cyp6p3 <- ggplot(data = time_conditon,
       aes(
         x = `Sample Name`,
         y = Cyp6p3)) +
  geom_boxplot(color = "chocolate4") +
  facet_wrap(~`Time of the day`) +

  labs(x = "Mosquito Name",y = 'Cyp6p3 ΔCt',
       title = "Time of the day") +
  geom_signif(comparisons = list(c("Kisumu", "Tiassale")),
              map_signif_level = TRUE)

print(time_cyp6p3)



combined_cyp6p3 <- age_cyp6p3 + blood_cyp6p3 + time_cyp6p3


# the mosquito v gene expression Cyp6m2-----------------------------------------------------
age_cyp6m2 <- ggplot(data = age_mosq,
              aes(
                x = `Sample Name`,
                y = Cyp6m2)) +
  geom_boxplot(color = "red") +
  facet_wrap(~`Time of the day`) +
  labs(x = "Mosquito Name",y = 'Cyp6m2 ΔCt',)+
  geom_signif(comparisons = list(c("Kisumu", "Tiassale")),
              map_signif_level = TRUE)
print(age_cyp6m2)

#Blood feeding status v gene expression Cyp6m2
blood_cyp6m2 <- ggplot(data = blood_feeding,
                aes(
                  x = `Sample Name`,
                  y = Cyp6m2)) +
  geom_boxplot(color = "blue") +
  facet_wrap(~`Blood-feeding status`) +

  labs(x = "Mosquito Name", y = 'Cyp6m2 ΔCt',)+
  geom_signif(comparisons = list(c("Kisumu", "Tiassale")),
              map_signif_level = TRUE)
print(blood_cyp6m2)


#Time of the day v gene expression Cyp6m2
time_cyp6m2 <- ggplot(data = time_conditon,
               aes(
                 x = `Sample Name`,
                 y = Cyp6m2)) +
  geom_boxplot(color = "chocolate4") +
  facet_wrap(~`Time of the day`) +

  labs(x = "", y = 'Cyp6m2 ΔCt',)+
    geom_signif(comparisons = list(c("Kisumu", "Tiassale")),
                map_signif_level = TRUE)
print(time_cyp6m2)

combined_cyp6m2 <- age_cyp6m2 + blood_cyp6m2 + time_cyp6m2
print(combined_cyp6m2)

# the mosquito v gene expression Cyp6aa1--------------------------------------------------------
age_cyp6aa1 <- ggplot(data = age_mosq,
                     aes(
                       x = `Sample Name`,
                       y = Cyp6aa1)) +
  geom_boxplot(color = "red") +
  facet_wrap(~`Time of the day`) +

  labs(x = "",y = "Cyp6aa1 ΔCt",)

# the mosquito v gene expression Cyp6aa1
blood_cyp6aa1 <- ggplot(data = blood_feeding,
                       aes(
                         x = `Sample Name`,
                         y = Cyp6aa1)) +
  geom_boxplot(color = "blue") +
  facet_wrap(~`Blood-feeding status`) +

  labs(x = "", y = "Cyp6aa1 ΔCt,") +
  geom_signif(comparisons = list(c("Kisumu", "Tiassale")),
              map_signif_level = TRUE)

print(blood_cyp6aa1)




age_cyp6aa1 <- ggplot(data = age_mosq,
                      aes(x = `Sample Name`, y = Cyp6aa1)) +
  geom_boxplot(color = "red") +
  facet_wrap(~`Time of the day`) +
  labs(x = "", y = "Cyp6aa1 ΔCt") +
  geom_signif(comparisons = list(c("Kisumu", "Tiassale")),
              map_signif_level = TRUE)

print(age_cyp6aa1)




#Time of the day v gene expression Cyp6aa1
time_cyp6aa1 <- ggplot(data = time_conditon,
                      aes(
                        x = `Sample Name`,
                        y = Cyp6aa1)) +
  geom_boxplot(color = "chocolate4") +
  facet_wrap(~`Time of the day`) +
  labs(x = "", y = "Cyp6aa1 ΔCt",) +
  geom_signif(comparisons = list(c("Kisumu", "Tiassale")),
              map_signif_level = TRUE)

print(time_cyp6aa1)




combined_cyp6aa1 <- age_cyp6aa1 + blood_cyp6aa1 + time_cyp6aa1

# final combined plots

combined_cyp6p3/combined_cyp6m2/combined_cyp6aa1



# creates separate plots
# # age versus the gene expresion plot
# age_cyp6p3 <- ggplot(data = age_mosq,
#        aes(
#        x = `Sample Name`,
#        y = Cyp6p3)) +
#   geom_boxplot(color = "red") +
#   facet_wrap(~`Time of the day`) +
#   labs(x = "", y = 'Cyp6p3 ΔCt',
#        title = "Age of mosquito") +
#   geom_signif(comparisons = list(c("Kisumu", "Tiassale")),
#               map_signif_level = TRUE)
#
# print(age_cyp6p3)

age_cyp6m2 <- ggplot(data = age_mosq,
                     aes(
                       x = `Sample Name`,
                       y = Cyp6m2)) +
  geom_boxplot(color = "red") +
  facet_wrap(~`Time of the day`) +
  labs(x = "Mosquito Name",y = 'Cyp6m2 ΔCt',)+
  geom_signif(comparisons = list(c("Kisumu", "Tiassale")),
              map_signif_level = TRUE)
print(age_cyp6m2)
