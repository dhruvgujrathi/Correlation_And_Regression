#ALY6015_Dhruv_Gujrathi_Assignment1_R
install.packages("tidyverse") #Data Manipulation & Visualization
install.packages("janitor") #Examining & Cleaning Data
install.packages("broom") #Converts messy data to interpretable data
install.packages("Hmisc") #Imputing missing values, high level graphics, etc.
install.packages("dplyr") #Manipulating the Data
install.packages("ggplot2") #Creating Graphics

library(tidyverse) # data manipulation and cleaning
library(janitor) # cleaning up column names
library(broom) # tidying analysis results
library(Hmisc) # exploratory data analysis
library(dplyr) #Data manipulation
library(ggplot2) #Creating Graphics

#Loading AmesHousing.csv Dataset
data1<-read.csv("AmesHousing.csv") 
#Cleaning the dataset for easing the use with R
clean_names(data1)   
#Displaying the data characteristics
glimpse(data1)
#Descriptive Data Analysis
describe(data1 %>% select(SalePrice, Street, Yr.Sold))
#Selecting top values
head(map(data1, ~sum(is.na(.))))
#Exploratory data analysis
glimpse(data1)
frequency(data1) 
#Creating an easy to read data frame
describe(data1)
#Prepare dataset for modelling by imputing missing values with the variable mean value
data1$SalePrice <- as.numeric(data1$SalePrice) #Converting to numeric class
data1_SalePrice_mean <- data1 %>%
  mutate(Street = replace(SalePrice,
                          is.na(SalePrice),
                          mean(SalePrice, na.rm = T))) #Replacing NA/Nan Values
data1_SalePrice_mean

data1$Street <- as.numeric(data1$Street) #Converting to numeric class
data1_Street_mean <- data1 %>%
  mutate(Street = replace(Street,
                          is.na(Street),
                          mean(Street, na.rm = T))) #Replacing NA/Nan Values
data1_Street_mean

data1$Yr.Sold <- as.numeric(data1$Yr.Sold) #Converting to numeric class
data1_Yrsold_mean <- data1 %>%
  mutate(Yr.Sold = replace(Yr.Sold,
                           is.na(Yr.Sold),
                           mean(Yr.Sold, na.rm = T))) #Replacing NA/Nan Values
data1_Yrsold_mean

#Using the cor function for creating the correlation matrix 
x<-data1 %>%
  select(SalePrice, Yr.Sold, Lot.Area, Bedroom.AbvGr) %>%
  cor() %>%
  round(3)
x
#Producing a cor relation plot of the correlation matrix
library(corrplot)
corrplot(x, method = "circle")
#Comparing the correlation plots and determing the patterns 
plot(data1$SalePrice,data1$`Lot.Area`,main="Highest correlation with Sales Price",xlab="SalesPrice", ylab="Lot of Size in square foot")
plot(data1$SalePrice, data1$`1st Flr SF`, main = "lowest correlation with salesprice",
     xlab="salesprice", ylab="first floor square feet")
plot(data1$SalePrice,data1$`Gr.Liv.Area`,main="Correlation with salesprice", xlab = "SalePrice", ylab ="size of garage in square feet" )
#plotting ggplot
ggplot(data1,aes(x=`Lot.Area`,y=SalePrice, main="Correlation with salesprice with regression line"))+
  geom_point(alpha=.5)+
  geom_smooth(method = "lm", se=F) +
  theme_bw()
# Fitting Regression Model
mod <- lm(SalePrice ~ Yr.Sold+ Lot.Area + Street , data = data1)
print(mod)
summary(mod)
#Rounding the data
tidy(mod) %>%
  mutate(estimate = round(estimate, 3))
# Writing this in equation form
#hat(Price) = 2756000-1356.777 \times Yr.Sold + 2.828 \times Lot.Area + 120662.749 Street$$
#Plotting Regression model 
plot(mod)
#Checking for multicolinearity
library(car)
vif(mod)
#Checking for outliers
hist(mod$coefficients,
     xlab = "Co-efficients",
     main = "Histogram to check for Outliers",)
#Subsets regression method to identify best model
summary(mod)
res.sum <- summary(mod)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic))
