# Final project visualizations

setwd("/Users/beckyblake/Desktop/R-visualizations")

library(readr)
salary_prediction <- read_csv("eda_data.csv")

library(dplyr)
job_titles <- select(salary_prediction, 'Job Title')

#####################################################################
# Word cloud visualization

##Install Packages
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

##Load Require Library

library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)

job_titles2 <- data.frame(job_titles)

## Calculate Corpus
job_titles2.Corpus<-Corpus(VectorSource(job_titles2$Job.Title))

# Generate word cloud
wordcloud(job_titles2.Corpus,max.words = 100,min.freq = 2, random.order=FALSE, random.color=TRUE,
          rot.per=0.35, colors=brewer.pal(8, "Dark2")) +
  text(x=0.5, y=0.83, "Frequency of job titles")

#####################################################################
# Scatterplot of age and salary
library(ggplot2)
data(salary_prediction)

ggplot(salary_prediction,
       aes(x=age, y=avg_salary)) +
  xlim(16, 100) +
  geom_point(color="cornflowerblue") +
  labs(title="Scatterplot of age as a predictor of salary",
       x="Age (years)",
       y="Average salary (thousands of dollars)",
       caption="Figure 2: scatterplot of age and average salary") +
  geom_smooth(method="lm")
  

#####################################################################
# Cleveland dot plot 

plotdata <- salary_prediction %>%
  group_by(job_state) %>%
  summarize(n = n(),
            mean=mean(avg_salary),
            sd = sd(avg_salary),
            se = sd/sqrt(n),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))

ggplot(plotdata,
       aes(x=mean,
           y=reorder(job_state, mean)),
       group = 1) +
  geom_point(color="blue",
             size=2) +
  geom_segment(aes(x=25, 
                   xend=mean,
                   y=reorder(job_state, mean),
                   yend=reorder(job_state, mean)),
                   color="lightgray") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title="Average salary of participants grouped by state",
       x = "Average salary (thousands of dollars)",
       y="US State (abbreviated)",
       caption="Figure 3: Cleveland dot plot of average salary by state")

