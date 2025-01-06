
# Week26 - 1st Part of Question
#
#LINEAR REGRESSION
install.packages(c("tidyverse", "ggpubr"))
library(tidyverse)
library(ggpubr)
theme_set(theme_pubr())

#Set the working directory - The location of the .txt file
setwd("/Users/imac/Desktop/M2_Activity")
getwd()
dir()

# Read the text file into R as a data frame
data<- read.table("Data.txt", header = TRUE, sep = "\t")
head(data)
# The header = TRUE argument tells R that the first row of the text file 
# contains the column's names. If the text file does not have column's name, set
# header = FALSE. 
# The sep = "\t" argument tells R that the file is tab-separated. If the file 
# is comma-separated, use sep="," instead. 

####

## VISUALIZATION
# Create a scatter plot displying deaths against vaccination and add a smoothed 
# line "X" would be the independent variable, and "Y" the dependent one. 

ggplot(data, aes(x=Doses_100, y=Deaths_100000)) +
  geom_point() +
  stat_smooth()

# The graph above suggests a linearly decreasing relationship between the 
# Vacc_rate and the mortality variables.

## CALCULATE CORRELATION 
# It's also possible to compute the correlation coefficient between the two
# variables using R function cor():
cor(data$Doses_100, data$Deaths_100000)

# The correlation coefficient measures the level of the association between 
# two variables x and y. the exetream cases is between -1 to +1, which means 
# a perfect correlation. if It's value ranges between -1 (the relation is negative, 
# or perfect negative, increasing one of the value decreasing the other, whic is this case)
# correlation: when x increases, y decreases) and +1 (would be diagonal line, perfect positive)
# correlation: when x increases, y increases).

# A value closer to 0 suggests a weak relationship between the variables or low correlation.
# A Low correlation (-0.2 < x < 0.2) probably suggests that much of variation of
# the outcome variable (y) is not explained by the predictor (x). In such case,
# we should probably look for better predictor variables.

# In our example, the correlation coefficient is large enough, -0.7, so we can 
# cpmtomie by building a linear model of y as a funtion of x. 

# Computation of the LNR
model <- lm(Deaths_100000 ~ Doses_100, data = data) 
model

## Regression line
stat_smooth()
  ggplot(data, aes(Doses_100, Deaths_100000)) +
    geom_point() +
    stat_smooth(method = lm)
summary(model)

# The summary outputs shows 6 componentsm including:
# Call. shows the function call used to compute the regression model
# Residuals = provide a quick view of the distribution of the residuals 
# Coefficients = shows the regression beta coefficients and their 
# Significance = predictor variables, that are significantly associated the 
# outcome variable, are marked by stars.
# Residual Standard Error (RSE), R-Squared (R2), and the F-Statistics that are 
# used to check how well the model fits to our data. 
#
## THE MODEL ACCURACY
# Coefficients significance 
# The coefficients table, in the model 1 statistical summary, shows 
  # the estimates of the beta coefficients
  # the standard errors (SE), which defines the accuracy of beta coefficients
  # the t-statistic and the associated P-value
# The F-Statistic and the P-value


#Residual Standar Error:
sigma(model)*100/mean(data$Doses_100)
sigma(model)*100/mean(data$Deaths_100000)

# THE CONFIDENCE INTERVALS
# The standard error measurs the variability/accuracy of the beta
# It can be used to compute the confidence intervals of the coefficient 

confint(model)
# we have got pretty fine confidence margine (2.5% and 97%) for each one of these
# extremes, and with this 

##############################################################

# Week 25 - 2nd  Part of Question
#LINEAR REGRESSION
install.packages(c("tidyverse", "ggpubr"))
library(tidyverse)
library(ggpubr)
theme_set(theme_pubr())

#Set the working directory - The location of the .txt file
setwd("/Users/imac/Desktop/M2_Activity")
getwd()
dir()

# Read the text file into R as a data frame
data<- read.table("Data.txt", header = TRUE, sep = "\t")
head(data)

# Introduce the Factor "Vaccin Ratio" with categories "High & Low" by adding 
# a Column using forward-pipe operator, %>%, if doses greater than 202 will 
# concider as high, else means smaller than 202 will concider low. 

install.packages("diplyr")
library(dplyr)
data <- data %>%
  mutate("Vacc_ratio" = if_else(Doses_100>202, "High", "Low"))
head(data)

# %>% is called the forward-pipe operator in R. It provides a mechanism for 
# chaining commands with a new forward-Pipe operatore, %>%. This operator will
# forward a value. or the result of an expression, into the next function or 
# the next call expression


#######
# CHECK THE DATA
#
group_by(data, Vacc_ratio) %>%
  summarise(
    count = n(),
    mean = mean(Deaths_100000, na.rm = TRUE),
    sd = sd(Deaths_100000, na.rm = TRUE)
  )

# Show the levels: factors -> categories
# named the factor, Vacc_ratio, then state the categories, “High” and “Low”
levels(data$Vacc_ratio)
data$Vacc_ratio <- ordered(data$Vacc_ratio, 
                           levels = c("High", "Low"))
levels(data$Vacc_ratio)

## VISUALIZE DATA
install.packages("ggplot2")
library("ggplot2")
install.packages("ggpubr")
library("ggpubr")


ggboxplot(data, x = "Vacc_ratio", y = "Deaths_100000",
          color = "Vacc_ratio", palette = c("#00AFBB", "#E7B800", "#FC4EO7"),
          order = c("High", "Low"),
          ylab = "Deaths_100000", xlab = "Vacc_ratio")

# Mean plots, plot deaths by groups of low or high vaccination ratio, to add
# error bars: mean_se (other values include: mean_sd, mean_ci, median_igr, ...)
library("ggpubr")
ggline(data, x = "Vacc_ratio", y = "Deaths_100000",
       add = c("mean_se", "jitter"),
       order = c("High", "Law"),
       ylab = "Deaths_100000", xlab = "Vacc_ratio")

# If you still want to use R vase graphs, type the following scripts:
# Box plot
boxplot(Deaths_100000~Vacc_ratio, data = data,
        xlab = "Vacc_ratio", ylab = "Deaths_100000",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))

# Compute the analysis of variance (res=results of aov=analysis of variance)
res.aov <- aov(Deaths_100000 ~ Vacc_ratio, data = data)

# Check ANOVA assumptions: test validity?
# The ANOVA test assumes homogeneous variances (leventeTest()),
# normal distribution.
install.packages("car")
library(car)
leveneTest(Deaths_100000 ~ Vacc_ratio, data = data)
plot(res.aov, 1)

# Run Shapiro-wilk test
aov_residuals <- residuals(object = res.aov)
shapiro.test(x = aov_residuals)
plot(res.aov, 2)

# Lets see the results:
summary(res.aov)

# PostHoc
TukeyHSD(res.aov)





