# import library
library(ggplot2)
library(dplyr)
library(ggthemes)
library(hrbrthemes)

# read data
data <- read.csv("Churn_Modelling.csv")

# data preprocessing
data <- na.omit(data)  #delete Na
data <- subset(data, Geography != "")
data <- distinct(data, CustomerId, .keep_all = TRUE)  #delete duplicate data

# data visualisation
## plot 1
### Geography vs Balance vs Gender vs Exited boxplot
ggplot(data, aes(x = Geography, y = Balance, color = interaction(Gender, Exited))) +
  geom_tufteboxplot(median.type = "point", whisker.type = "line",
    stat="boxplot", outlier.shape = 18, outlier.size = 1) + 
  labs(
    x = "Geography", 
    y = "Account Balance", 
    title = "Account Balance Distribution by Gender, Geography, and Churn Status", 
    subtitle = "Theme is theme_ipsum()", 
    caption = "Fig. 1", 
    color = "Gender and Churn Status"
  ) +
  scale_color_manual(
    values = c("Male.0" = "blue", "Male.1" = "darkblue", 
               "Female.0" = "red", "Female.1" = "darkred"), 
    labels = c("Male.0" = "Male.NotExited", "Male.1" = "Male.Exited", 
               "Female.0" = "Female.NotExited", "Female.1" = "Female.Exited")
  ) + 
  theme_ipsum()

## plot 2
### NumOfProducts vs Age boxplot
ggplot(data = data, aes(x = factor(NumOfProducts), y = Age, colour = factor(Exited))) +
  geom_tufteboxplot(median.type = "point", whisker.type = "line", 
    stat="boxplot", outlier.shape = 16, outlier.size = 1) + 
  labs(
    x = "NumOfProducts", 
    y = "Age", 
    title = "Impact of Number of Products on Customer Age and Churn", 
    subtitle = "Theme is theme_ipsum()", 
    caption = "Fig. 2",
    colour = "Exited"
  ) + 
  scale_color_manual(values = c("0" = "#5467a7", "1" = "#ff5100")) +
  theme_ipsum()

## plot 3
### Age vs Exited proportion point linear regression
#### Calculate the proportion of Exited for each IsActiveMember
data_summary <- data %>%
  group_by(IsActiveMember, Age) %>%
  summarize(Exited = mean(Exited), .groups = 'drop')
data_summary

ggplot(data = data_summary, aes(x = Age, y = Exited)) + 
  geom_point(color = "black", size = 1) + 
  geom_smooth(method = "lm", color = "black", se = FALSE) + 
  facet_grid(cols = vars(IsActiveMember), 
    labeller = labeller(IsActiveMember = c("0" = "Not Active", 
                                           "1" = "Active"))) + 
  labs(
    x = "Age", 
    y = "Exited Proportion", 
    title = "Churn Proportion Distribution by Age and IsActiveMember", 
    subtitle = "Theme is theme_ipsum()", 
    caption = "Fig. 3",
    colour = "Exited"
  ) + 
  theme_ipsum()