# Normality of Coeae1f and Coeae6g delta Ct values
# same tests as for Cyp6p3, Cyp6m2 and Cyp6aa1 (19/08/2024)

# Load necessary libraries
library(readxl)

age_mosq <- read_excel("gene_expression_data_5genes.xlsx",
                       sheet = "Age of the mosquito condition")

blood_feeding <- read_excel("gene_expression_data_5genes.xlsx",
                            sheet = "Blood feeding status condition")

time_conditon <- read_excel("gene_expression_data_5genes.xlsx",
                            sheet = "Time of the day condition")

# Shapiro-Wilk test for normality ----------------------------------------------
datasets <- list("Age of the mosquito" = age_mosq,
                 "Blood-feeding status" = blood_feeding,
                 "Time of the day" = time_conditon)

for (condition in names(datasets)) {
  for (gene in c("Coeae1f", "Coeae6g")) {
    shapiro_test_result <- shapiro.test(datasets[[condition]][[gene]])
    cat(condition, "-", gene, ": W =", round(shapiro_test_result$statistic, 3),
        ", P =", signif(shapiro_test_result$p.value, 3), "\n")}}

# Q-Q plots ------------------------------------------------------------------------
# 3 rows (conditions) x 2 columns (genes)
par(mfrow = c(3, 2))
for (condition in names(datasets)) {
  for (gene in c("Coeae1f", "Coeae6g")) {
    qqnorm(datasets[[condition]][[gene]], main = paste("Q-Q Plot for", gene, "-", condition))
    qqline(datasets[[condition]][[gene]], col = ifelse(gene == "Coeae1f", "purple", "orange"))}}

# Reset plotting area to default
par(mfrow = c(1, 1))
