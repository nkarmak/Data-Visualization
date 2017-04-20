# Clear work environment
rm(list = ls())

library(ggplot2)


# Read in data set into a dataframe
df <- read.csv("movie_metadata.csv", header = TRUE , sep = ",")

# Exploratory Data Analysis
summary(df)

g1 <- ggplot(df, aes(gross, movie_facebook_likes)) + labs(tiitle = "Likes Vs Gross Earning", x = "Gross Earning", y = "Facebook Likes")
g1 + geom_point() + geom_smooth(method = 'lm', se = FALSE)

summary(lm(df$gross~df$movie_facebook_likes))

g2 <- ggplot(df, aes(budget, movie_facebook_likes)) + labs(tiitle = "Likes Vs Budget", x = "Budget", y = "Facebook Likes")
g2 + geom_point() + geom_smooth(method = 'lm', se = FALSE)

hist(df$duration, breaks = 100, main = "Histogram of Movie Duration", xlab = "Duration")