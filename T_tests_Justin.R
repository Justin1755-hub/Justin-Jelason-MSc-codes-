
#t_tests 13/08/2024

#pacman::p_load(rio,here,tidyverse,patchwork)


#load packages
library(readxl)
library(ggplot2)
library(ggsignif)
install.packages('gmodels')
# Install the gmodels package if it's not already installed
if (!require("gmodels")) {
  install.packages("gmodels")}


# Load the gmodels package
library("gmodels")



age_mosq <- read_excel("gene_expression_data.xlsx",
                       sheet = "Age of the mosquito condition")



blood_feeding <- read_excel("gene_expression_data.xlsx",
                            sheet = "Blood feeding status condition")



time_conditon <- read_excel("gene_expression_data.xlsx",
                            sheet = "Time of the day condition")

# Install required package if not already installed
install.packages("readxl")

# Ensure categorical variables are factors


#t_test for the age of mosquito--------------------------------------------------------

# Create a combined factor for `Sample Name` and `Time of the day`
age_mosq$Sample_Time <- paste(age_mosq$`Sample Name`, age_mosq$`Time of the day`, sep = " ")

# Function to calculate t-test and mean difference for Cyp6p3------

perform_t_test <- function(data, group1, group2) {
  test <- t.test(Cyp6p3 ~ Sample_Time, data = data,
                 subset = Sample_Time %in% c(group1, group2))
  mean_diff <- diff(test$estimate)
  return(list(test = test, mean_diff = mean_diff))}

# Perform t-tests and calculate mean differences
t_test_1 <- perform_t_test(age_mosq, "Kisumu 2 Days", "Tiassale 2 Days")
t_test_2 <- perform_t_test(age_mosq, "Kisumu 2 Days", "Tiassale 5 Days")
t_test_3 <- perform_t_test(age_mosq, "Kisumu 5 Days", "Tiassale 2 Days")
t_test_4 <- perform_t_test(age_mosq, "Kisumu 2 Days", "Kisumu 5 Days")
t_test_5 <- perform_t_test(age_mosq, "Tiassale 2 Days", "Tiassale 5 Days")
t_test_6 <- perform_t_test(age_mosq, "Kisumu 5 Days", "Tiassale 5 Days")

# Print t-test results
print(t_test_1)
print(t_test_2)
print(t_test_3)
print(t_test_4)
print(t_test_5)
print(t_test_6)

# Display the t-test results and mean differences
t_test_1$mean_diff  # Difference in means for Kisumu 2 Days vs Tiassale 2 Days
t_test_2$mean_diff  # Difference in means for Kisumu 2 Days vs Tiassale 5 Days
t_test_3$mean_diff  # Difference in means for Kisumu 5 Days vs Tiassale 2 Days
t_test_4$mean_diff  # Difference in means for Kisumu 2 Days vs Kisumu 5 Days
t_test_5$mean_diff  # Difference in means for Tiassale 2 Days vs Tiassale 5 Days
t_test_6$mean_diff  # Difference in means for Kisumu 5 Days vs Tiassale 5 Days



# Function to calculate t-test and mean difference for Cyp6m2------

perform_t_test <- function(data, group1, group2) {
  test <- t.test(Cyp6m2 ~ Sample_Time, data = data,
                 subset = Sample_Time %in% c(group1, group2))
  mean_diff <- diff(test$estimate)
  return(list(test = test, mean_diff = mean_diff))}

# Perform t-tests and calculate mean differences
t_test_1 <- perform_t_test(age_mosq, "Kisumu 2 Days", "Tiassale 2 Days")
t_test_2 <- perform_t_test(age_mosq, "Kisumu 2 Days", "Tiassale 5 Days")
t_test_3 <- perform_t_test(age_mosq, "Kisumu 5 Days", "Tiassale 2 Days")
t_test_4 <- perform_t_test(age_mosq, "Kisumu 2 Days", "Kisumu 5 Days")
t_test_5 <- perform_t_test(age_mosq, "Tiassale 2 Days", "Tiassale 5 Days")
t_test_6 <- perform_t_test(age_mosq, "Kisumu 5 Days", "Tiassale 5 Days")

# Print t-test results
print(t_test_1)
print(t_test_2)
print(t_test_3)
print(t_test_4)
print(t_test_5)
print(t_test_6)

# Display the t-test results and mean differences
t_test_1$mean_diff  # Difference in means for Kisumu 2 Days vs Tiassale 2 Days
t_test_2$mean_diff  # Difference in means for Kisumu 2 Days vs Tiassale 5 Days
t_test_3$mean_diff  # Difference in means for Kisumu 5 Days vs Tiassale 2 Days
t_test_4$mean_diff  # Difference in means for Kisumu 2 Days vs Kisumu 5 Days
t_test_5$mean_diff  # Difference in means for Tiassale 2 Days vs Tiassale 5 Days
t_test_6$mean_diff  # Difference in means for Kisumu 5 Days vs Tiassale 5 Days



# Function to calculate t-test and mean difference for Cyp6aa1------

perform_t_test <- function(data, group1, group2) {
  test <- t.test(Cyp6aa1 ~ Sample_Time, data = data,
                 subset = Sample_Time %in% c(group1, group2))
  mean_diff <- diff(test$estimate)
  return(list(test = test, mean_diff = mean_diff))}

# Perform t-tests and calculate mean differences
t_test_1 <- perform_t_test(age_mosq, "Kisumu 2 Days", "Tiassale 2 Days")
t_test_2 <- perform_t_test(age_mosq, "Kisumu 2 Days", "Tiassale 5 Days")
t_test_3 <- perform_t_test(age_mosq, "Kisumu 5 Days", "Tiassale 2 Days")
t_test_4 <- perform_t_test(age_mosq, "Kisumu 2 Days", "Kisumu 5 Days")
t_test_5 <- perform_t_test(age_mosq, "Tiassale 2 Days", "Tiassale 5 Days")
t_test_6 <- perform_t_test(age_mosq, "Kisumu 5 Days", "Tiassale 5 Days")

# Print t-test results
print(t_test_1)
print(t_test_2)
print(t_test_3)
print(t_test_4)
print(t_test_5)
print(t_test_6)

# Display the t-test results and mean differences
t_test_1$mean_diff  # Difference in means for Kisumu 2 Days vs Tiassale 2 Days
t_test_2$mean_diff  # Difference in means for Kisumu 2 Days vs Tiassale 5 Days
t_test_3$mean_diff  # Difference in means for Kisumu 5 Days vs Tiassale 2 Days
t_test_4$mean_diff  # Difference in means for Kisumu 2 Days vs Kisumu 5 Days
t_test_5$mean_diff  # Difference in means for Tiassale 2 Days vs Tiassale 5 Days
t_test_6$mean_diff  # Difference in means for Kisumu 5 Days vs Tiassale 5 Days




#t_test for the Blood-feeding status------------------------------------------------------------------

# Create a combined factor for `Sample Name` and `Blood-feeding status`
blood_feeding$Sample_Blood <- paste(blood_feeding$`Sample Name`, blood_feeding$`Blood-feeding status`, sep = " ")

# Function to calculate t-test and mean difference for Cyp6p3------

perform_t_test <- function(data, group1, group2) {
  test <- t.test(Cyp6p3 ~ Sample_Blood, data = data,
                 subset = Sample_Blood %in% c(group1, group2))
  mean_diff <- diff(test$estimate)
  return(list(test = test, mean_diff = mean_diff))}

# Perform t-tests and calculate mean differences
t_test_1 <- perform_t_test(blood_feeding, "Kisumu Blood-fed", "Tiassale Blood-fed")
t_test_2 <- perform_t_test(blood_feeding, "Kisumu Blood-fed", "Tiassale Non-blood-fed")
t_test_3 <- perform_t_test(blood_feeding, "Kisumu Non-blood-fed", "Tiassale Blood-fed")
t_test_4 <- perform_t_test(blood_feeding, "Kisumu Blood-fed", "Kisumu Non-blood-fed")
t_test_5 <- perform_t_test(blood_feeding, "Tiassale Blood-fed", "Tiassale Non-blood-fed")
t_test_6 <- perform_t_test(blood_feeding, "Kisumu Non-blood-fed", "Tiassale Non-blood-fed")

# Print t-test results
print(t_test_1)
print(t_test_2)
print(t_test_3)
print(t_test_4)
print(t_test_5)
print(t_test_6)


# Function to calculate t-test and mean difference for Cyp6m2------

perform_t_test <- function(data, group1, group2) {
  test <- t.test(Cyp6m2 ~ Sample_Blood, data = data,
                 subset = Sample_Blood %in% c(group1, group2))
  mean_diff <- diff(test$estimate)
  return(list(test = test, mean_diff = mean_diff))}

# Perform t-tests and calculate mean differences
t_test_1 <- perform_t_test(blood_feeding, "Kisumu Blood-fed", "Tiassale Blood-fed")
t_test_2 <- perform_t_test(blood_feeding, "Kisumu Blood-fed", "Tiassale Non-blood-fed")
t_test_3 <- perform_t_test(blood_feeding, "Kisumu Non-blood-fed", "Tiassale Blood-fed")
t_test_4 <- perform_t_test(blood_feeding, "Kisumu Blood-fed", "Kisumu Non-blood-fed")
t_test_5 <- perform_t_test(blood_feeding, "Tiassale Blood-fed", "Tiassale Non-blood-fed")
t_test_6 <- perform_t_test(blood_feeding, "Kisumu Non-blood-fed", "Tiassale Non-blood-fed")

# Print t-test results
print(t_test_1)
print(t_test_2)
print(t_test_3)
print(t_test_4)
print(t_test_5)
print(t_test_6)


# Function to calculate t-test and mean difference for Cyp6aa1------

perform_t_test <- function(data, group1, group2) {
  test <- t.test(Cyp6aa1 ~ Sample_Blood, data = data,
                 subset = Sample_Blood %in% c(group1, group2))
  mean_diff <- diff(test$estimate)
  return(list(test = test, mean_diff = mean_diff))}

# Perform t-tests and calculate mean differences
t_test_1 <- perform_t_test(blood_feeding, "Kisumu Blood-fed", "Tiassale Blood-fed")
t_test_2 <- perform_t_test(blood_feeding, "Kisumu Blood-fed", "Tiassale Non-blood-fed")
t_test_3 <- perform_t_test(blood_feeding, "Kisumu Non-blood-fed", "Tiassale Blood-fed")
t_test_4 <- perform_t_test(blood_feeding, "Kisumu Blood-fed", "Kisumu Non-blood-fed")
t_test_5 <- perform_t_test(blood_feeding, "Tiassale Blood-fed", "Tiassale Non-blood-fed")
t_test_6 <- perform_t_test(blood_feeding, "Kisumu Non-blood-fed", "Tiassale Non-blood-fed")

# Print t-test results
print(t_test_1)
print(t_test_2)
print(t_test_3)
print(t_test_4)
print(t_test_5)
print(t_test_6)



#t_test for the Time of the day condition--------------------------------------------------------

# Create a combined factor for `Sample Name` and `Time of the day`
time_conditon$Sample_Time <- paste(time_conditon$`Sample Name`, time_conditon$`Time of the day`, sep = " ")

# Function to calculate t-test and mean difference for Cyp6p3------

perform_t_test <- function(data, group1, group2) {
  test <- t.test(Cyp6p3 ~ Sample_Time, data = data,
                 subset = Sample_Time %in% c(group1, group2))
  mean_diff <- diff(test$estimate)
  return(list(test = test, mean_diff = mean_diff))}

# Perform t-tests and calculate mean differences
t_test_1 <- perform_t_test(time_conditon, "Kisumu 6AM", "Tiassale 6AM")
t_test_2 <- perform_t_test(time_conditon, "Kisumu 6AM", "Tiassale 6PM")
t_test_3 <- perform_t_test(time_conditon, "Kisumu 6PM", "Tiassale 6AM")
t_test_4 <- perform_t_test(time_conditon, "Kisumu 6AM", "Kisumu 6PM")
t_test_5 <- perform_t_test(time_conditon, "Tiassale 6AM", "Tiassale 6PM")
t_test_6 <- perform_t_test(time_conditon, "Kisumu 6PM", "Tiassale 6PM")

# Print t-test results
print(t_test_1)
print(t_test_2)
print(t_test_3)
print(t_test_4)
print(t_test_5)
print(t_test_6)


# Function to calculate t-test and mean difference for Cyp6m2------

perform_t_test <- function(data, group1, group2) {
  test <- t.test(Cyp6m2 ~ Sample_Time, data = data,
                 subset = Sample_Time %in% c(group1, group2))
  mean_diff <- diff(test$estimate)
  return(list(test = test, mean_diff = mean_diff))}

# Perform t-tests and calculate mean differences
t_test_1 <- perform_t_test(time_conditon, "Kisumu 6AM", "Tiassale 6AM")
t_test_2 <- perform_t_test(time_conditon, "Kisumu 6AM", "Tiassale 6PM")
t_test_3 <- perform_t_test(time_conditon, "Kisumu 6PM", "Tiassale 6AM")
t_test_4 <- perform_t_test(time_conditon, "Kisumu 6AM", "Kisumu 6PM")
t_test_5 <- perform_t_test(time_conditon, "Tiassale 6AM", "Tiassale 6PM")
t_test_6 <- perform_t_test(time_conditon, "Kisumu 6PM", "Tiassale 6PM")

# Print t-test results
print(t_test_1)
print(t_test_2)
print(t_test_3)
print(t_test_4)
print(t_test_5)
print(t_test_6)

# Function to calculate t-test and mean difference for Cyp6aa1------

perform_t_test <- function(data, group1, group2) {
  test <- t.test(Cyp6aa1 ~ Sample_Time, data = data,
                 subset = Sample_Time %in% c(group1, group2))
  mean_diff <- diff(test$estimate)
  return(list(test = test, mean_diff = mean_diff))}

# Perform t-tests and calculate mean differences
t_test_1 <- perform_t_test(time_conditon, "Kisumu 6AM", "Tiassale 6AM")
t_test_2 <- perform_t_test(time_conditon, "Kisumu 6AM", "Tiassale 6PM")
t_test_3 <- perform_t_test(time_conditon, "Kisumu 6PM", "Tiassale 6AM")
t_test_4 <- perform_t_test(time_conditon, "Kisumu 6AM", "Kisumu 6PM")
t_test_5 <- perform_t_test(time_conditon, "Tiassale 6AM", "Tiassale 6PM")
t_test_6 <- perform_t_test(time_conditon, "Kisumu 6PM", "Tiassale 6PM")

# Print t-test results
print(t_test_1)
print(t_test_2)
print(t_test_3)
print(t_test_4)
print(t_test_5)
print(t_test_6)

