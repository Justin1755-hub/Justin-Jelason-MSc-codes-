
# General Linear Model_Analysis of Variance
# 14/08/2024



#working directory
install.packages("car")
# For the glm and drop1 functions
library(stats) 
library(car)
# for excel files
library(readxl)
# Load the datasets as below------

age_mosq <- read_excel("gene_expression_data.xlsx", 
                       sheet = "Age of the mosquito condition")


blood_feeding <- read_excel("gene_expression_data.xlsx",
                            sheet = "Blood feeding status condition")

time_conditon <- read_excel("gene_expression_data.xlsx",
                            sheet = "Time of the day condition")

# View the first few rows of the data set
head(age_mosq)

# Check the structure of the data set
str(age_mosq)

# Fit the General Linear Model for the Age of the mosquito------------------------------------------------

# for the Cyp6p3-------------
glm_model <- glm(Cyp6p3 ~ `Sample Name` + `Time of the day`, data = age_mosq)
glm_model <- glm(Cyp6p3 ~ `Sample Name` * `Time of the day`, data = age_mosq)
# Perform model selection using drop1
drop1(glm_model, test = "F")

# Summarize the fitted model
summary(glm_model)
anova(glm_model)


# for the Cyp6m2-------------
glm_model <- glm(Cyp6m2 ~ `Sample Name` + `Time of the day`, data = age_mosq)
glm_model <- glm(Cyp6m2 ~ `Sample Name` * `Time of the day`, data = age_mosq)
# Perform model selection using drop1
drop1(glm_model, test = "F")

# Summarize the fitted model
summary(glm_model)
anova(glm_model)

# for the Cyp6aa1-------------
glm_model <- glm(Cyp6aa1 ~ `Sample Name` + `Time of the day`, data = age_mosq)
glm_model <- glm(Cyp6aa1 ~ `Sample Name` * `Time of the day`, data = age_mosq)
# Perform model selection using drop1
drop1(glm_model, test = "F")

# Summarize the fitted model
summary(glm_model)
anova(glm_model)

# Fit the General Linear Model for Blood-feeding status------------------------------------------------

# for the Cyp6p3-------------
glm_model <- glm(Cyp6p3 ~ `Sample Name` + `Blood-feeding status`, data = blood_feeding)
glm_model <- glm(Cyp6p3 ~ `Sample Name` * `Blood-feeding status`, data = blood_feeding)
# Perform model selection using drop1
drop1(glm_model, test = "F")

# Summarize the fitted model
summary(glm_model)
anova(glm_model)

# for the Cyp6m2-------------
glm_model <- glm(Cyp6m2 ~ `Sample Name` + `Blood-feeding status`, data = blood_feeding)
glm_model <- glm(Cyp6m2 ~ `Sample Name` * `Blood-feeding status`, data = blood_feeding)
# Perform model selection using drop1
drop1(glm_model, test = "F")

# Summarize the fitted model
summary(glm_model)
anova(glm_model)

# for the Cyp6aa1-------------
glm_model <- glm(Cyp6aa1 ~ `Sample Name` + `Blood-feeding status`, data = blood_feeding)
glm_model <- glm(Cyp6aa1 ~ `Sample Name` * `Blood-feeding status`, data = blood_feeding)
# Perform model selection using drop1
drop1(glm_model, test = "F")

# Summarize the fitted model
summary(glm_model)
anova(glm_model)


# Fit the General Linear Model for the Time of the day condition------------------------------------------------

# for the Cyp6p3-------------
glm_model <- glm(Cyp6p3 ~ `Sample Name` + `Time of the day`, data = time_conditon)
glm_model <- glm(Cyp6p3 ~ `Sample Name` * `Time of the day`, data = time_conditon)
# Perform model selection using drop1
drop1(glm_model, test = "F")

# Summarize the fitted model
summary(glm_model)
anova(glm_model)


# for the Cyp6m2-------------
glm_model <- glm(Cyp6m2 ~ `Sample Name` + `Time of the day`, data = time_conditon)
glm_model <- glm(Cyp6m2 ~ `Sample Name` * `Time of the day`, data = time_conditon)
# Perform model selection using drop1
drop1(glm_model, test = "F")

# Summarize the fitted model
summary(glm_model)
anova(glm_model)


# for the Cyp6aa1-------------
glm_model <- glm(Cyp6aa1 ~ `Sample Name` + `Time of the day`, data = time_conditon)
glm_model <- glm(Cyp6aa1 ~ `Sample Name` * `Time of the day`, data = time_conditon)
# Perform model selection using drop1
drop1(glm_model, test = "F")

# Summarize the fitted model
summary(glm_model)
anova(glm_model)

