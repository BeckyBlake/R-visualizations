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
       caption="Figure 1: scatterplot of age and average salary") +
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

#####################################################################

# Scatterplot of satisfaction and salary

data(salary_prediction)

ggplot(salary_prediction,
       aes(x=Rating,
           y=avg_salary)) +
  geom_point(color="cornflowerblue") +
  geom_smooth(method="lm") +
  labs(title="Job satisfaction as a predictor of salary",
       x="Rating (5 point scale)",
       y="Average salary (thousands of USD)",
       caption="Figure 2: Scatterplot with best fit line of job satisfaciton and salary")

#####################################################################
# bar chart based on industry
df <- data.frame(salary_prediction)

plotdata2 <- df %>%
  group_by(Industry) %>%
  summarize(mean_salary = mean(avg_salary))

plotdata2 <- filter(plotdata2, Industry != -1 &
                      mean_salary >= 120)

ggplot(plotdata2,
       aes(x=Industry,
           y=mean_salary)) +
  geom_bar(stat="Identity",
           fill="cornflowerblue") +
  coord_flip() +
  theme_classic() +
  labs(title="Industries with highest average salaries",
       y = "Mean salary (thousands of USD)",
       caption="Figure 3: Bar chart of average salaries based on industry")

#####################################################################
# bar chart of python knowledge
df3 <- salary_prediction %>%
  mutate(python_yn = ifelse(python_yn == 1,
                            "Yes",
                            "No"),
         R_yn = ifelse(R_yn == 1,
                       "Yes",
                       "No"),
         spark = ifelse(spark == 1,
                        "Yes",
                        "No"),
         aws = ifelse(aws == 1,
                      "Yes",
                      "No"),
         excel = ifelse(excel == 1,
                        "Yes",
                        "No"))


plotdata3 <- df3 %>%
  group_by(python_yn) %>%
  summarize(mean_salary = mean(avg_salary))

ggplot(plotdata3,
       aes(x=python_yn,
           y=mean_salary)) +
  geom_bar(stat="Identity",
           fill="cornflowerblue") +
  theme_classic() +
  labs(title="Knowledge of Python as predictor of salary",
       y = "Mean salary (thousands of USD)",
       x="Knowledge of Python?",
       caption="Figure 4: Bar chart of Python knowledge and mean salary")

#####################################################################
# bar chart of R knowledge

plotdata3 <- df3 %>%
  group_by(R_yn) %>%
  summarize(mean_salary = mean(avg_salary))

ggplot(plotdata3,
       aes(x=R_yn,
           y=mean_salary)) +
  geom_bar(stat="Identity",
           fill="cornflowerblue") +
  theme_classic() +
  labs(title="Knowledge of R as predictor of salary",
       y = "Mean salary (thousands of USD)",
       x="Knowledge of R?",
       caption="Figure 5: Bar chart of R knowledge and mean salary")

#####################################################################
# Bar chart of spark knowledge
plotdata3 <- df3 %>%
  group_by(spark) %>%
  summarize(mean_salary = mean(avg_salary))

ggplot(plotdata3,
       aes(x=spark,
           y=mean_salary)) +
  geom_bar(stat="Identity",
           fill="cornflowerblue") +
  theme_classic() +
  labs(title="Knowledge of Spark as predictor of salary",
       y = "Mean salary (thousands of USD)",
       x="Knowledge of Spark?",
       caption="Figure 6: Bar chart of Spark knowledge and mean salary")

#####################################################################
# Bar chart of aws knowledge

plotdata3 <- df3 %>%
  group_by(aws) %>%
  summarize(mean_salary = mean(avg_salary))

ggplot(plotdata3,
       aes(x=aws,
           y=mean_salary)) +
  geom_bar(stat="Identity",
           fill="cornflowerblue") +
  theme_classic() +
  labs(title="Knowledge of AWS as predictor of salary",
       y = "Mean salary (thousands of USD)",
       x="Knowledge of AWS?",
       caption="Figure 7: Bar chart of AWS knowledge and mean salary")
#####################################################################
# Bar chart of excel knowledge

plotdata3 <- df3 %>%
  group_by(excel) %>%
  summarize(mean_salary = mean(avg_salary))

ggplot(plotdata3,
       aes(x=excel,
           y=mean_salary)) +
  geom_bar(stat="Identity",
           fill="cornflowerblue") +
  theme_classic() +
  labs(title="Knowledge of Excel as predictor of salary",
       y = "Mean salary (thousands of USD)",
       x="Knowledge of Excel?",
       caption="Figure 8: Bar chart of Excel knowledge and mean salary")
#####################################################################
