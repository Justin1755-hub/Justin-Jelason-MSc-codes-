

#19/08/2024

# working space 

# Load necessary libraries
library(readxl)

# Load the data from the Excel sheet
age_mosq <- read_excel("gene_expression_data.xlsx", 
                       sheet = "Age of the mosquito condition")


blood_feeding <- read_excel("gene_expression_data.xlsx",
                            sheet = "Blood feeding status condition")

time_conditon <- read_excel("gene_expression_data.xlsx",
                            sheet = "Time of the day condition")

####################
# Eric suggestions #
####################
# When you test for normality, you shouldn't be testing the raw data, you should be testing the
# residuals of your data. This means that for each treatment (eg: Kisumu 2 days) you subtract the
# mean of the treatment from each value. This gives you the residuals. Then you combine all the 
# residuals and test those for normality. I will explain the reason for this when I see you.
# Here is a manual way to calculate residuals for Cyp6aa1 by mosquito age, which will hopefully 
# explain to you what a residual is
#cyp6aa1.K2d <- subset(age_mosq, `Sample Name` == 'Kisumu' & `Time of the day` == '2 Days')
#residuals.cyp6aa1.K2d <- cyp6aa1.K2d$Cyp6aa1 - mean(cyp6aa1.K2d$Cyp6aa1)
#cyp6aa1.K5d <- subset(age_mosq, `Sample Name` == 'Kisumu' & `Time of the day` == '5 Days')
#residuals.cyp6aa1.K5d <- cyp6aa1.K5d$Cyp6aa1 - mean(cyp6aa1.K5d$Cyp6aa1)
#cyp6aa1.T2d <- subset(age_mosq, `Sample Name` == 'Tiassale' & `Time of the day` == '2 Days')
#residuals.cyp6aa1.T2d <- cyp6aa1.T2d$Cyp6aa1 - mean(cyp6aa1.T2d$Cyp6aa1)
#cyp6aa1.T5d <- subset(age_mosq, `Sample Name` == 'Tiassale' & `Time of the day` == '5 Days')
#residuals.cyp6aa1.T5d <- cyp6aa1.T5d$Cyp6aa1 - mean(cyp6aa1.T5d$Cyp6aa1)
#cyp6aa1.age.residuals <- c(residuals.cyp6aa1.K2d, residuals.cyp6aa1.K5d, residuals.cyp6aa1.T2d, residuals.cyp6aa1.T5d) 
# However, you can also extract the residuals directly from the glm:
#age_mosq$Sample_Time <- paste(age_mosq$`Sample Name`, age_mosq$`Time of the day`, sep = " ")
#glm_model <- glm(Cyp6aa1 ~ Sample_Time, data = age_mosq)
#glm_model$residuals
# The values in cyp6aa1.age.residuals and glm_model$residuals aren't in the same order, but they 
# are the same. 
# You can then check this for normality
#shapiro.test(glm_model$residuals)
####################



# To test the normality

# Perform the Shapiro-Wilk test for normality for age of mosquito 

#Cyp6p3--------------
shapiro_test_result <- shapiro.test(age_mosq$Cyp6p3)
# Print the results
print(shapiro_test_result)

#Cyp6m2--------------
shapiro_test_result <- shapiro.test(age_mosq$Cyp6m2)
# Print the results
print(shapiro_test_result)

#Cyp6aa1--------------
shapiro_test_result <- shapiro.test(age_mosq$Cyp6aa1)
# Print the results
print(shapiro_test_result)

---------------------------------------------------
  
# Perform the Shapiro-Wilk test for normality for blood-feeding status 
  
#Cyp6p3--------------
shapiro_test_result <- shapiro.test(blood_feeding$Cyp6p3)
# Print the results
print(shapiro_test_result)

#Cyp6m2--------------
shapiro_test_result <- shapiro.test(blood_feeding$Cyp6m2)
# Print the results
print(shapiro_test_result)

#Cyp6aa1--------------
shapiro_test_result <- shapiro.test(blood_feeding$Cyp6aa1)
# Print the results
print(shapiro_test_result)

---------------------------------------------------
# Perform the Shapiro-Wilk test for normality for blood-feeding status 
  
#Cyp6p3--------------
shapiro_test_result <- shapiro.test(time_conditon$Cyp6p3)
# Print the results
print(shapiro_test_result)
  
#Cyp6m2--------------
shapiro_test_result <- shapiro.test(time_conditon$Cyp6m2)
# Print the results
print(shapiro_test_result)  
  
#Cyp6aa1--------------
shapiro_test_result <- shapiro.test(time_conditon$Cyp6aa1)
# Print the results
print(shapiro_test_result)  



--------------------------------------------------------------------------------------------------------------
  
# normality uisng   Q-Q plots
# Age of the mossquito conditon 
  

# Q-Q plot for Cyp6p3
qqnorm(age_mosq$Cyp6p3, main = "Q-Q Plot for Cyp6p3")
qqline(age_mosq$Cyp6p3, col = "red")

# Q-Q plot for Cyp6m2
qqnorm(age_mosq$Cyp6m2, main = "Q-Q Plot for Cyp6m2")
qqline(age_mosq$Cyp6m2, col = "blue")

# Q-Q plot for Cyp6aa1
qqnorm(age_mosq$Cyp6aa1, main = "Q-Q Plot for Cyp6aa1")
qqline(age_mosq$Cyp6aa1, col = "green")

---------------------
  
# Blood-feeding status  

# Q-Q plot for Cyp6p3
qqnorm(blood_feeding$Cyp6p3, main = "Q-Q Plot for Cyp6p3")
qqline(blood_feeding$Cyp6p3, col = "red")

# Q-Q plot for Cyp6m2
qqnorm(blood_feeding$Cyp6m2, main = "Q-Q Plot for Cyp6m2")
qqline(blood_feeding$Cyp6m2, col = "blue")

# Q-Q plot for Cyp6aa1
qqnorm(blood_feeding$Cyp6aa1, main = "Q-Q Plot for Cyp6aa1")
qqline(blood_feeding$Cyp6aa1, col = "green")
 
 
--------------------
# Time of the day condtion  
  
# Q-Q plot for Cyp6p3
  qqnorm(time_conditon$Cyp6p3, main = "Q-Q Plot for Cyp6p3")
qqline(time_conditon$Cyp6p3, col = "red")

# Q-Q plot for Cyp6m2
qqnorm(time_conditon$Cyp6m2, main = "Q-Q Plot for Cyp6m2")
qqline(time_conditon$Cyp6m2, col = "blue")

# Q-Q plot for Cyp6aa1
qqnorm(time_conditon$Cyp6aa1, main = "Q-Q Plot for Cyp6aa1")
qqline(time_conditon$Cyp6aa1, col = "green")

  
--------------------------------------------------------------------------------
  
# Set up the plotting area to have 3 rows and 1 column
par(mfrow = c(3, 1))  # 3 rows, 1 column layout

# Q-Q plot for Cyp6p3
qqnorm(age_mosq$Cyp6p3, main = "Q-Q Plot for Cyp6p3")
qqline(age_mosq$Cyp6p3, col = "red")

# Q-Q plot for Cyp6m2
qqnorm(age_mosq$Cyp6m2, main = "Q-Q Plot for Cyp6m2")
qqline(age_mosq$Cyp6m2, col = "blue")

# Q-Q plot for Cyp6aa1
qqnorm(age_mosq$Cyp6aa1, main = "Q-Q Plot for Cyp6aa1")
qqline(age_mosq$Cyp6aa1, col = "green")

# Reset plotting area to default
par(mfrow = c(1, 1))
  
  
