# This script uses the following packages; install before running if necessary

# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("psych")

library(tidyr)      #call necessary libraries
library(ggplot2)
library(scales)   
library(dplyr)
library(psych)

bea <- read.csv("bea.csv")

SuperLab <- bea[, c("WHITE9", "BLACK9", "ASIAN9", "HISPAN9", "CDIVISION", "UNEMP9")]   #subsetting 'bea' table for ease of viewing

#convert columns from wide to long using dplyr's pivot_longer function
SuperLab_Long <- SuperLab %>%
select(WHITE9, BLACK9, ASIAN9, HISPAN9, CDIVISION, UNEMP9) %>%
    pivot_longer(
        cols = c(WHITE9, BLACK9, ASIAN9, HISPAN9),
            names_to = "Race",
            values_to = "Race_Percentage"
)

#normalize percentanges to represent total racial compositions per census area
SuperLab_normalized <- SuperLab_Long %>%
    group_by(CDIVISION) %>%
    mutate(Proportion = Race_Percentage / sum(Race_Percentage)) %>%
    ungroup()

#plot stacked bar graph for racial compositions per census division (Figure 1)
ggplot(SuperLab_normalized, aes(x = CDIVISION, y = Proportion, fill = Race)) +
    geom_bar(stat = "identity", position = "stack") +
    ggtitle("Racial Composition by Census Division") +
    scale_y_continuous(labels = percent_format()) +
    labs(
        x = "Census Division",
        y = "Racial Composition (%)",
        fill = "Race"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

#plot box plots for percentage unemployed per census division (Figure 2)
ggplot(SuperLab_normalized, aes(x = CDIVISION, y = UNEMP9)) + 
    geom_boxplot(color="mediumslateblue", fill="lightsteelblue", alpha=0.2) + 
    ggtitle("Percentage Unemployed per Census Division") +
    labs(y= "Unemployment Rates (%)", x = "Census Regions") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

#transform racial percentages by removing non-white data
SuperLab_mutated <- SuperLab %>%
    mutate(NonWhite = 100 - WHITE9)

#calculate means of both variables per division
division_stats <- SuperLab_mutated %>%
group_by(CDIVISION) %>%
    summarise(
        Mean_NonWhite = mean(NonWhite, na.rm = TRUE),
        Mean_Unemployment = mean(UNEMP9, na.rm = TRUE)
)

#run Pearson's correlation
cor.test(division_stats$Mean_NonWhite, division_stats$Mean_Unemployment, method = 'pearson')

#run linear regression
ph.lm = lm(division_stats$Mean_Unemployment~division_stats$Mean_NonWhite)
summary(ph.lm)