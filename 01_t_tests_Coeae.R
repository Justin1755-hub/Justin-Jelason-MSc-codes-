#t_tests for the carboxylesterase genes Coeae1f and Coeae6g
# same analysis as the t-tests for Cyp6p3, Cyp6m2 and Cyp6aa1 (13/08/2024)

#load packages
library(readxl)

age_mosq <- read_excel("gene_expression_data_5genes.xlsx",
                       sheet = "Age of the mosquito condition")

blood_feeding <- read_excel("gene_expression_data_5genes.xlsx",
                            sheet = "Blood feeding status condition")

time_conditon <- read_excel("gene_expression_data_5genes.xlsx",
                            sheet = "Time of the day condition")

# Function to calculate the t-test (Welch) and mean difference for any gene
# mean_diff = log2 FC = mean of the second group minus mean of the first group
# (groups are ordered alphabetically by R, as in the Cyp6 analysis)
perform_t_test <- function(data, gene, group_var, group1, group2) {
  sub_data <- data[data[[group_var]] %in% c(group1, group2), ]
  test <- t.test(sub_data[[gene]] ~ sub_data[[group_var]])
  mean_diff <- diff(test$estimate)
  return(list(test = test, mean_diff = mean_diff))}

# Function to run the six comparisons of one experiment and print a summary table
run_comparisons <- function(data, gene, group_var, comparisons) {
  results <- data.frame()
  for (cmp in comparisons) {
    res <- perform_t_test(data, gene, group_var, cmp[1], cmp[2])
    print(res$test)
    results <- rbind(results, data.frame(Gene = gene,
                                         Group1 = cmp[1], Group2 = cmp[2],
                                         log2FC = round(unname(res$mean_diff), 2),
                                         T = round(unname(res$test$statistic), 2),
                                         P = signif(res$test$p.value, 3)))}
  print(results)
  results}


#t_test for the age of mosquito--------------------------------------------------------

# Create a combined factor for `Sample Name` and `Time of the day`
age_mosq$Sample_Time <- paste(age_mosq$`Sample Name`, trimws(age_mosq$`Time of the day`), sep = " ")

age_comparisons <- list(c("Kisumu 2 Days", "Tiassale 2 Days"),
                        c("Kisumu 2 Days", "Tiassale 5 Days"),
                        c("Kisumu 5 Days", "Tiassale 2 Days"),
                        c("Kisumu 2 Days", "Kisumu 5 Days"),
                        c("Tiassale 2 Days", "Tiassale 5 Days"),
                        c("Kisumu 5 Days", "Tiassale 5 Days"))

# Coeae1f------
age_coeae1f <- run_comparisons(age_mosq, "Coeae1f", "Sample_Time", age_comparisons)
# Coeae6g------
age_coeae6g <- run_comparisons(age_mosq, "Coeae6g", "Sample_Time", age_comparisons)


#t_test for the Blood-feeding status------------------------------------------------------------------

# Create a combined factor for `Sample Name` and `Blood-feeding status`
blood_feeding$Sample_Blood <- paste(blood_feeding$`Sample Name`, blood_feeding$`Blood-feeding status`, sep = " ")

blood_comparisons <- list(c("Kisumu Blood-fed", "Tiassale Blood-fed"),
                          c("Kisumu Blood-fed", "Tiassale Non-blood-fed"),
                          c("Kisumu Non-blood-fed", "Tiassale Blood-fed"),
                          c("Kisumu Blood-fed", "Kisumu Non-blood-fed"),
                          c("Tiassale Blood-fed", "Tiassale Non-blood-fed"),
                          c("Kisumu Non-blood-fed", "Tiassale Non-blood-fed"))

# Coeae1f------
blood_coeae1f <- run_comparisons(blood_feeding, "Coeae1f", "Sample_Blood", blood_comparisons)
# Coeae6g------
blood_coeae6g <- run_comparisons(blood_feeding, "Coeae6g", "Sample_Blood", blood_comparisons)


#t_test for the Time of the day condition--------------------------------------------------------

# Create a combined factor for `Sample Name` and `Time of the day`
time_conditon$Sample_Time <- paste(time_conditon$`Sample Name`, time_conditon$`Time of the day`, sep = " ")

time_comparisons <- list(c("Kisumu 6AM", "Tiassale 6AM"),
                         c("Kisumu 6AM", "Tiassale 6PM"),
                         c("Kisumu 6PM", "Tiassale 6AM"),
                         c("Kisumu 6AM", "Kisumu 6PM"),
                         c("Tiassale 6AM", "Tiassale 6PM"),
                         c("Kisumu 6PM", "Tiassale 6PM"))

# Coeae1f------
time_coeae1f <- run_comparisons(time_conditon, "Coeae1f", "Sample_Time", time_comparisons)
# Coeae6g------
time_coeae6g <- run_comparisons(time_conditon, "Coeae6g", "Sample_Time", time_comparisons)

# all results in one table
all_t_tests <- rbind(age_coeae1f, age_coeae6g, blood_coeae1f, blood_coeae6g, time_coeae1f, time_coeae6g)
write.csv(all_t_tests, "t_tests_Coeae.csv", row.names = FALSE)
