
# Assignment 1

# Read in Dataset
kaggle_survey <- read.csv(file="kaggle_survey_2022.csv", header=TRUE, sep=",")


# Listwise Deletion
kaggle_survey2 <- na.omit(kaggle_survey)

#######################################################

# Univariable graph – Categorical Data
# bar chart by gender

library(ggplot2)

ggplot(kaggle_survey2, aes(x=Q3)) +
  geom_bar(fill="cornflowerblue",
           color="black") +
  labs(x="Gender",
       y="Frequency",
       title="Participants by Gender")

#######################################################

# Univariable graph – Quantitative Data
# histogram by time taken on survey

library(ggplot2)

ggplot(kaggle_survey2, aes(x=Duration_seconds)) +
  xlim(0, 2000) +
  geom_histogram(fill="indianred",
                 stat="count",
                 color="indianred",
                 bins=20) +
  labs(title="Time Taken on Survey",
       x="Time (seconds)",
       y="Frequency")

#######################################################

# Bivariable Graph – Categorical-Categorical
# grouped bar chart by gender and whether they're currently a student

library(ggplot2)

ggplot(kaggle_survey2,
       aes(x=Q3,
           fill=Q5)) +
  geom_bar(position="dodge") +
  labs(title="Participants by Gender and Student Status",
       x="Gender",
       y="Frequency",
       fill = "Currently a Student?") +
  coord_flip()

#######################################################

# Bivariable Graph – Quantitative-Categorical
# Mean/SEM plot of time taken grouped by gender

library(ggplot2)
library(dplyr)

plotdata <- kaggle_survey2 %>%
  group_by(Q3) %>%
  summarize(n = n(),
            mean = mean(Duration_seconds),
            sd = sd(Duration_seconds),
            se = sd/sqrt(n),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))

ggplot(plotdata,
       aes(x = Q3,
           y = mean,
           group = 1)) +
  geom_point(size = 3) +
  geom_line() + 
  geom_errorbar(aes(ymin= mean - se,
                    ymax = mean + se),
                width = 0.1) +
  labs(title="Time Taken by Participants Grouped by Gender",
       x="Gender",
       y="Average Time (seconds)")
         


