
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

###################
# Eric suggestion #
###################
# Tidy up the lines above, no need for the first install.packages('gmodels') line since you 
# then have it again inside the if clause. Also no need for the library("gmodels") line since
# you already ran require. 
# Also, I'm not sure what the "gmodels" package is for. You don't seem to use it. 
###################

age_mosq <- read_excel("gene_expression_data.xlsx",
                       sheet = "Age of the mosquito condition")



blood_feeding <- read_excel("gene_expression_data.xlsx",
                            sheet = "Blood feeding status condition")



time_conditon <- read_excel("gene_expression_data.xlsx",
                            sheet = "Time of the day condition")

# Install required package if not already installed
install.packages("readxl")
###################
# Eric suggestion #
###################
# Remove this or put it above the line where you load the readxl library
###################

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

###################
# Eric suggestion #
###################
# The above is fine. What would be neater would be to store all of your results in a table, 
# which you can then save as a .csv file So you are not then copying out your results from the 
# screen output. You could do something like:
#age_mean_diff <- matrix(NA, 3, 3, dimnames = list(c('K2D', 'T2D', 'K5D'), c('T2D', 'K5D', 'T5D')))
#age_mean_diff['K2D', 'T2D'] <- t_test_1$mean_diff
#age_mean_diff['K2D', 'T5D'] <- t_test_2$mean_diff
#age_mean_diff['T2D', 'K5D'] <- t_test_3$mean_diff
#age_mean_diff['K2D', 'K5D'] <- t_test_4$mean_diff
#age_mean_diff['T2D', 'T5D'] <- t_test_5$mean_diff
#age_mean_diff['K5D', 'T5D'] <- t_test_6$mean_diff
# Or, if you want to be even more fancy, you could include all your calculations in a single loop:
#age.treatments <- c("Kisumu 2 Days", "Tiassale 2 Days", "Kisumu 5 Days", "Tiassale 5 Days")
#cyp6p3.age.mean.diff <- matrix(NA, 4, 4, dimnames = list(age.treatments, age.treatments))
#cyp6p3.age.pval <- matrix(NA, 4, 4, dimnames = list(age.treatments, age.treatments))
#cyp6p3.age.tval <- matrix(NA, 4, 4, dimnames = list(age.treatments, age.treatments))
#for (i in 1:(length(age.treatments)-1)){
#	for (j in (i+1):length(age.treatments)){
#		t_test <- perform_t_test(age_mosq, age.treatments[i], age.treatments[j])
#		cyp6p3.age.mean.diff[i,j] <- t_test$mean_diff
#		cyp6p3.age.pval[i,j] <- t_test$test$p.value
#		cyp6p3.age.mean.diff[i,j] <- t_test$test$statistic
#	}
#}
# Then you could save those matrices to a csv file, and you would have the results of your analysis
# in a file. Less room for error due to typos, etc...
# Just to emphasise, there is nothing wrong with your approach, these are just recommendations to
# make things clearer and more streamline. In general, if you repeat a process many times (like your
# t-test here), it's good practice to try and write your code in such a way that you only code the
# process once (like in the for-loop above, so I only actually have one line of code where the 
# perform_t_test function gets run), because this means that if you later want to change how you
# do things, you only have to change it in one place. 
###################


# Function to calculate t-test and mean difference for Cyp6m2------

perform_t_test <- function(data, group1, group2) {
  test <- t.test(Cyp6m2 ~ Sample_Time, data = data,
                 subset = Sample_Time %in% c(group1, group2))
  mean_diff <- diff(test$estimate)
  return(list(test = test, mean_diff = mean_diff))}

###################
# Eric suggestion #
###################
# Again, this is fine and won't give you any wrong results, but just in terms of good practice, it's 
# best not to replace a function with another one of the same name. It can lead to accidental errors.
# Better to give each function a different name, eg: perform_t_test_cyp6m2_time. 
# Similarly, below, you are re-using the names t_test_1, etc... This could also cause confusion and 
# accidental errors down the line. 
###################

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

