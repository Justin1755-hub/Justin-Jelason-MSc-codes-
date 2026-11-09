# General Linear Model_Analysis of Variance for Coeae1f and Coeae6g
# same models as for Cyp6p3, Cyp6m2 and Cyp6aa1 (14/08/2024)

# For the glm and drop1 functions
library(stats)
# for excel files
library(readxl)

# Load the datasets as below------
age_mosq <- read_excel("gene_expression_data_5genes.xlsx",
                       sheet = "Age of the mosquito condition")
age_mosq$`Time of the day` <- trimws(age_mosq$`Time of the day`)

blood_feeding <- read_excel("gene_expression_data_5genes.xlsx",
                            sheet = "Blood feeding status condition")

time_conditon <- read_excel("gene_expression_data_5genes.xlsx",
                            sheet = "Time of the day condition")

# Function: 1) full model with interaction -> is the interaction significant?
#           2) model with main effects only -> log2 FC (coefficients), F and P (drop1)
fit_glm <- function(gene, condition, data) {
  cat("\n=====", gene, "~ colony *", condition, "=====\n")
  f_int  <- as.formula(paste0(gene, " ~ `Sample Name` * `", condition, "`"))
  f_main <- as.formula(paste0(gene, " ~ `Sample Name` + `", condition, "`"))
  glm_int <- glm(f_int, data = data)
  print(drop1(glm_int, test = "F"))      # F test of the interaction term
  print(summary(glm_int))
  glm_main <- glm(f_main, data = data)
  print(drop1(glm_main, test = "F"))     # F and P for colony and condition
  print(summary(glm_main))               # coefficients = log2 FC
  print(anova(glm_main, test = "F"))
  invisible(list(interaction = glm_int, main = glm_main))}

# Age of the mosquito------------------------------------------------
glm_age_coeae1f <- fit_glm("Coeae1f", "Time of the day", age_mosq)
glm_age_coeae6g <- fit_glm("Coeae6g", "Time of the day", age_mosq)

# Blood-feeding status------------------------------------------------
glm_bf_coeae1f <- fit_glm("Coeae1f", "Blood-feeding status", blood_feeding)
glm_bf_coeae6g <- fit_glm("Coeae6g", "Blood-feeding status", blood_feeding)

# Time of the day condition------------------------------------------------
glm_time_coeae1f <- fit_glm("Coeae1f", "Time of the day", time_conditon)
glm_time_coeae6g <- fit_glm("Coeae6g", "Time of the day", time_conditon)
