
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

###################
# Eric suggestion #
###################
# The first of the two lines above is redundant, since it gets replaced by the second
# line. That's confusing code. I guess what you did was first run the first of the 
# two lines, and do "drop1", then go back and run the second version, and do "drop1" 
# again? It's better to write your code in a way that doesn't require interactive 
# use, where you can run the whole script from start to finish and get what you need.
# Also, better to avoid name re-use if possible. So rather than calling all your
# models glm_model, call them glm_model_cyp6p3_age, or something like that. This 
# reduces the risk of accidental errors, especially if you are running the code 
# interactively. 
###################

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

