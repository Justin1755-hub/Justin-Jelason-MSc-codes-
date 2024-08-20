

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
  
  