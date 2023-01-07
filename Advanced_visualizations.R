# Assignment 2

# read in dataset
library(readr)
books2 <- read_csv("books2.csv")

# filter by author
library(dplyr)
books3 <- filter(books2, author == "Karen Kingsbury")


library(GGally)
# prepare data
data(books3, package="ggplot2")

#################################################################
# Scatterplot Matrix
library(dplyr)
df <- books3 %>%
  mutate(log_reviewct = log(review_count),
         log_avgrating = log(average_rating)) %>%
  select(log_reviewct, rating_count, log_avgrating)

# create scatterplot matrix
ggpairs(df) +
  labs(title = "Relationships between review count, average rating, and rating count",
       caption="Figure 1: Scatterplot matrix of Karen Kingsbury books")

#################################################################
# Bubble Chart
library(ggplot2)
ggplot(books3,
       aes(x = review_count, y = average_rating, size = rating_count)) +
  geom_point(fill="cornflowerblue",
             color="black",
             alpha = 0.5,
             shape=21) + 
  labs(title = "Average rating by number of reviews and total ratings",
       x = "Number of reviews",
       y = "Average rating (5 point scale)",
       size = "Rating count",
       caption="Figure 2: Bubble chart of ratings and reviews of Karen Kingsbury books") +
  theme_minimal()

#################################################################
# Heatmap
library(superheat)
data(books3)
books4 <- select(books3, rating_count, review_count, average_rating, five_star_ratings, four_star_ratings, three_star_ratings, two_star_ratings, one_star_ratings, number_of_pages)
rownames(books4) <- books3$title
superheat(books4,
          scale=TRUE,
          left.label.text.size = 3,
          bottom.label.text.size=3,
          bottom.label.size = 0.05,
          title="Heatmap of books by Karen Kingsbury",
          title.size = 8) 

#################################################################
# Radar Chart

install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar")

data(books3, package="ggplot2")
library(ggradar)
library(scales)
library(dplyr)
library(ggplot2)

plotdata <- books3 %>%
  filter(title %in% c("A Time to Embrace", "Take Two", "Just Beyond the Clouds")) %>%
  select(title, average_rating, review_count, five_star_ratings, one_star_ratings) %>%
  rename(group=title) %>%
  mutate_at(vars(-group),
            funs(rescale))
plotdata

# generate radar chart
ggradar(plotdata) + 
  labs(title="Ratings comparison between 3 Karen Kingsbury books",
       caption="Figure 4: Radar chart of Kingsbury ratings data")

#################################################################
# Waterfall Chart

category <- c("One star ratings", "Two star ratings", "Three star ratings", "Four star ratings", "Five star ratings")
value <- c(23, 93, 518, 1290, 2255)
ratings <- data.frame(category, value)

library(ggplot2)
library(waterfalls)

waterfall(ratings,
          calc_total=TRUE,
          total_rect_color="goldenrod1",
          total_axis_text = "Total number\nof ratings",
          total_rect_text_color = "black") +
  labs(title="Star ratings of 'A Time to Embrace' by Karen Kingsbury",
       x="Rating type (5 star scale)",
       y="Number of ratings",
       caption="Figure 5: Waterfall chart of the number of each type of rating") + 
  theme_minimal()
