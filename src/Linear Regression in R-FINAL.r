library(stats)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(broom)
library(ggpubr)
library(MASS)

df_features = read.csv("data/Walmart Retail Data Analytics - Features data-set.csv", sep = ",")
head(df_features)

df_sales = read.csv("data/Walmart Retail Data Analytics - sales data-set.csv", sep = ",")
head(df_sales)

df_stores = read.csv("data/Walmart Retail Data Analytics - stores data-set.csv", sep = ",")
head(df_stores)

convert_to_knowns <- function(df, date_format) {
    
    # Factorizating
    df$Store <- factor(df$Store)
    df$Dept <- factor(df$Dept)
    df$Type <- factor(df$Type)
    df$Size <- as.numeric(df$Size)

    # Converting timestamp to date format
    df$Date = as.Date(df$Date, format = date_format)
    df = arrange(df, Date, decreasing=TRUE)
    return(df)
}

df = merge(df_features, df_sales[-c(5)], by = c("Date", "Store"))
df = merge(df, df_stores, by = c("Store"))

df = convert_to_knowns(df, date_format="%d/%m/%Y")
head(df, 5)

numerical_cols = c('Temperature', 'Fuel_Price', 'MarkDown1', 'MarkDown2', 'MarkDown3', 'MarkDown4', 'MarkDown5', 
                   'CPI', 'Unemployment', 'Weekly_Sales', 'Size')

original_rows = nrow(df)
summary(df)

# Count missing values:

missing_values <- sapply(df, function(x) sum(is.na(x)))
print(missing_values)

##### Missing Value Treatment ######

# If there were missing values, imputation with MEAN is a good option for LR!
df$MarkDown1[is.na(df$MarkDown1)] = 0
df$MarkDown2[is.na(df$MarkDown2)] = 0
df$MarkDown3[is.na(df$MarkDown3)] = 0
df$MarkDown4[is.na(df$MarkDown4)] = 0
df$MarkDown5[is.na(df$MarkDown5)] = 0

# Dropping rows where Weakly Sales are negative
df = subset(df, df$Weekly_Sales>=0)

cat("Number of Records:\n\nOriginal Dataset = ", original_rows, "\nProcessed Dataset = ", nrow(df))

options(repr.plot.width = 20, repr.plot.height = 10)
par(mfrow = c(2, 5))

hist(df$Temperature, col="cyan", main = "Histogram for Temperature")
hist(df$Fuel_Price , col="cyan", main = "Histogram for Fuel_Price")
hist(df$MarkDown1 , col="cyan", main = "Histogram for MarkDown1")
hist(df$MarkDown2 , col="cyan", main = "Histogram for MarkDown2")
hist(df$MarkDown3 , col="cyan", main = "Histogram for MarkDown3")
hist(df$MarkDown4 , col="blue", main = "Histogram for MarkDown4")
hist(df$MarkDown5 , col="blue", main = "Histogram for MarkDown5")
hist(df$CPI , col="blue", main = "Histogram for CPI")
hist(df$Unemployment, col="blue", main = "Histogram for Unemployment")
hist(df$Weekly_Sales, col="blue", main = "Histogram for Weekly_Sales")

options(repr.plot.width = 20, repr.plot.height = 18)
par(mfrow = c(2, 5))

boxplot(df$Temperature, col="cyan", main = "Temperature")
boxplot(df$Fuel_Price , col="cyan", main = "Fuel_Price")
boxplot(df$MarkDown1 , col="cyan", main = "MarkDown1")
boxplot(df$MarkDown2 , col="cyan", main = "MarkDown2")
boxplot(df$MarkDown3 , col="cyan", main = "MarkDown3")
boxplot(df$MarkDown4 , col="blue", main = "MarkDown4")
boxplot(df$MarkDown5 , col="blue", main = "MarkDown5")
boxplot(df$CPI , col="blue", main = "CPI")
boxplot(df$Unemployment, col="blue", main = "Unemployment")
boxplot(df$Weekly_Sales, col="blue", main = "Weekly_Sales")

# Weakly Sales
Weekly_Sales_zscores <- scale(df$Weekly_Sales)
outliers <- which(abs(Weekly_Sales_zscores) > 3)
df$Weekly_Sales[outliers] <- mean(df$Weekly_Sales)

# Temperature
Temperature_zscores <- scale(df$Temperature)
outliers <- which(abs(Temperature_zscores) > 3)
df$Temperature[outliers] <- mean(df$Temperature)

# Fuel_Price
Fuel_Price_zscores <- scale(df$Fuel_Price)
outliers <- which(abs(Fuel_Price_zscores) > 3)
df$Fuel_Price[outliers] <- mean(df$Fuel_Price)

# MarkDown
MarkDown1_zscores <- scale(df$MarkDown1)
outliers <- which(abs(MarkDown1_zscores) > 3)
df$MarkDown1[outliers] <- mean(df$MarkDown1)

MarkDown2_zscores <- scale(df$MarkDown2)
outliers <- which(abs(MarkDown2_zscores) > 3)
df$MarkDown2[outliers] <- mean(df$MarkDown2)

MarkDown3_zscores <- scale(df$MarkDown3)
outliers <- which(abs(MarkDown3_zscores) > 3)
df$MarkDown3[outliers] <- mean(df$MarkDown3)

MarkDown4_zscores <- scale(df$MarkDown4)
outliers <- which(abs(MarkDown4_zscores) > 3)
df$MarkDown4[outliers] <- mean(df$MarkDown4)

MarkDown5_zscores <- scale(df$MarkDown5)
outliers <- which(abs(MarkDown5_zscores) > 3)
df$MarkDown5[outliers] <- mean(df$MarkDown5)

# CPI
CPI_zscores <- scale(df$CPI)
outliers <- which(abs(CPI_zscores) > 3)
df$CPI[outliers] <- mean(df$CPI)

# Unemployment
Unemployment_zscores <- scale(df$Unemployment)
outliers <- which(abs(Unemployment_zscores) > 3)
df$Unemployment[outliers] <- mean(df$Unemployment)

# Export csv to local
write.csv(df, "data/Preprocessed_Dataset.csv", row.names=FALSE)

# # # LOAD
df = read.csv("data/Preprocessed_Dataset.csv", sep = ",")
# df = convert_to_knowns(df, date_format="%Y-%m-%d")
# df

col4 = colorRampPalette(c("black", "darkgrey", "grey","#CFB87C"))
corrplot(cor(df[numerical_cols]), method = "ellipse", col = col4(100),  addCoef.col = "black", tl.col = "black")

#Plotting the Stores with the highest Average Store Sales, to select the one of the top 5 stores
ggplot(data=dum_dataf, aes(x=reorder(factor(Store), -Avg_Store_Sales), y=Avg_Store_Sales)) +
  geom_bar(stat="identity", fill="brown") +
  xlab('Store') +
  ylab('Average Store Sales')





pairs(df[numerical_cols], main="Weekly Sales Data", pch=21,  bg=c("#CFB87C"))







set.seed(7)

sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.75,0.2))
train  <- df[sample, ]
test   <- df[!sample, ]

cat("Number of records in Training set:", nrow(train))
cat("\nNumber of records in Testing set:", nrow(test))
