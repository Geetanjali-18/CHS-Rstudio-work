
# Packages I load every time;
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr)

data <- read.csv('E:/Geet College Work/CHC/R programming rstudio/Decision tree/diabetes.csv')
View(data)


# Feature selection

pc <- prcomp(data, center = TRUE, scale. = TRUE)
summary(pc)
typeof(pc)



# divide the data into train and test data 

set.seed(3)
id <- sample(2, nrow(data), prob = c(0.7, 0.3), replace = TRUE)
data_train <- data[id==1,]
data_test <- data[id==2,]


# Building decision tree

library(rpart)

colnames(data)
data_mod <- rpart(Outcome~. , data = data_train)

# Here we are using columns for the mode
data_mod
plot(data_mod)

text(data_mod, use.n = TRUE, pretty = TRUE, cex = 0.8)

pred_dia <- predict(data_mod, newdata = data_test,type = "class")
pred_dia

# Correlation Matrix
cor_mat <- cor(data, use = "complete.obs")
cor_mat

# Visualise the data using heat map 
library(corrplot)

corrplot(cor_mat, method = "color")

# Scatter Plot

plot(data$Glucose, data$outcome)

hist(data$Outcome)
