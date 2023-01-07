# Final project visualizations

library(readr)
salary_prediction <- read_csv("eda_data.csv")

library(dplyr)
salary_prediction1 <- select(salary_prediction, "Job Title" == "Data Scientist")
