# Advanced Visualizations II

# read in data
library(readr)
books2 <- read_csv("books2.csv")

# filter by author
library(dplyr)
books3 <- filter(books2, author == "Karen Kingsbury")

########################################################
# Time dependent graph
library(ggplot2)
library(scales)

larry_brown <- read_csv("books_copy2.csv")

ggplot(larry_brown, aes(x=date_published, y=average_rating)) +
  geom_line() +
  theme_classic() +
  labs(title="Average rating of Larry Brown books",
       subtitle="1980 to 2007",
       x="Date published",
       y="Average rating (5 point scale)")

########################################################
# Correlation Plot
library(ggcorrplot)

data(books3, package="mosaicData")

# select numeric variables
df <- dplyr::select_if(books3, is.numeric)

# calculate correlations
r <- cor(df, use="complete.obs")
round(r, 2)
ggcorrplot(r,
           hc.order=TRUE,
           type="lower",
           lab=TRUE) +
  labs(title="Correlation between numeric variables of Karen Kingsbury books")

########################################################
# Graph of Choice - Histogram
ggplot(books3, aes(x=five_star_ratings)) +
  geom_histogram(fill="indianred3",
                 color = "black",
                 bins=20) +
  labs(title="Histogram of five-star ratings of Karen Kingsbury books",
       y="Number of books",
       x="Five-star ratings count",
       subtitle="Number of bins = 20") +
  theme_classic()


########################################################
# Multi-panel graph

library(dplyr)
books4 <- filter(books2, author == "Karen Kingsbury" |
                   author == "Georgette Heyer" |
                   author == "Jim Butcher")

ggplot(books4, aes(x=average_rating)) +
  geom_histogram(fill = "cornflowerblue",
                 color= "white") +
  facet_wrap(~author, ncol = 1) +
  labs(title="Average rating histograms by author",
       x = "Average rating (5 point scale)",
       y="Number of books") +
  theme_bw() 
