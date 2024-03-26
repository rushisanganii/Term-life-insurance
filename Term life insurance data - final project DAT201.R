library(readxl)
TermLife <- read_excel("C:/Users/jasmi/Desktop/MCE DATA SCIENCE/DATA 201 DATA ANALYTICS AND MODELLING/final project/TermLife.xlsx")
data <- TermLife
summary(data)
library(ggplot2)



pairs(~ FACE + AGE + INCOME, data = TermLife, 
      main = "Scatterplot Matrix")

###1
 # Scatter plot for AGE vs. FACE
 ggplot(data, aes(x = AGE, y = FACE, color = GENDER)) +
   geom_point() +
   labs(title = "Scatter Plot of AGE vs. FACE by GENDER")
 
 
 # Scatter plot for INCOME vs. FACE
 ggplot(data, aes(x = INCOME, y = FACE)) +
   geom_point(aes(color = GENDER)) +
   labs(title = "Scatter Plot of INCOME vs. FACE by GENDER")
 
 # Boxplot for GENDER vs. FACE
 
 data$GENDER <- factor(data$GENDER)
 
 #0 = male , 1 = female
 ggplot(data, aes(x = GENDER, y = FACE)) +
   geom_boxplot() +
   labs(title = "Boxplot of GENDER vs. FACE")
 
 cor(data[, c("AGE", "INCOME", "FACE")])
 

 
 ###2
 
 ###a. Run a Multiple Linear Regression
 
 model <- lm(FACE ~ GENDER + AGE + INCOME, data = data)
 summary(model)
 
 # R-squared
 rsq <- summary(model)$r.squared
 

 rsq_adj <- summary(model)$adj.r.squared
 
 # Mean Squared Error (MSE)
 mse <- mean(model$residuals^2)
 

 cat("R-squared:", rsq, "\n")
 cat("Adjusted R-squared:", rsq_adj, "\n")
 cat("Mean Squared Error:", mse, "\n")
 
 
 # T-test for coefficients
 coefficients_test <- coef(summary(model))
 t_test <- coefficients_test[, "t value"]
 

 print(t_test)
 
 
 
 
 