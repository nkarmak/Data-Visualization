# Clear work environment
rm(list = ls())

library(ggplot2)
library(qgraph)
library(psych)
library(factoextra)
library(corrplot)

# Read in data set into a dataframe
df <- read.csv("movie_metadata.csv", header = TRUE , sep = ",")

# Exploratory Data Analysis
summary(df)

g1 <- ggplot(df, aes(gross, movie_facebook_likes)) + labs(tiitle = "Likes Vs Gross Earning", x = "Gross Earning", y = "Facebook Likes")
g1 + geom_point() + geom_smooth(method = 'lm', se = FALSE)

g2 <- ggplot(df, aes(budget, movie_facebook_likes)) + labs(tiitle = "Likes Vs Budget", x = "Budget", y = "Facebook Likes")
g2 + geom_point() + geom_smooth(method = 'lm', se = FALSE)

g3 <- ggplot(df, aes(title_year, movie_facebook_likes)) + labs(tiitle = "Likes Vs Year", x = "Year", y = "Facebook Likes")
g3 + geom_point() 

g4 <- ggplot(df, aes(title_year, imdb_score)) + labs(tiitle = "IMDB Score Vs Year", x = "Year", y = "IMDB Scores")
g4 + geom_point() 

g5 <- ggplot(df, aes(facenumber_in_poster, imdb_score)) + labs(tiitle = "IMDB Score Vs Poster Faces", x = "Poster Faces", y = "IMDB Scores")
g5 + geom_point() 

g6 <- ggplot(df, aes(title_year, facenumber_in_poster)) + labs(tiitle = "Poster Faces Vs Year", x = "Year", y = "IMDB Scores")
g6 + geom_point() 

g7 <- ggplot(df, aes(movie_facebook_likes, imdb_score)) + labs(tiitle = "IMDB Score Vs Likes", x = "IMDB Score", y = "Likes")
g7 + geom_point() 

hist(df$duration, breaks = 100, main = "Histogram of Movie Duration", xlab = "Duration")
hist(df$movie_facebook_likes, breaks = 100, main = "Histogram of Total Facebook Likes", xlab = "Total Facebook Likes")

# Removing columns without numeric values and rows with NA
nums <- sapply(df, is.numeric)
df <- as.data.frame(df[,nums])
df <- df[complete.cases(df),]

#Correlation plot of numberic variables
corrplot(cor(df), method = "circle")

# Principal Component Analysis
qg_boxes_pca <- qgraph.pca(df, factors = 5)
fit <- princomp(df, cor = TRUE)
summary(fit)
loadings(fit) # pc loadings 
plot(fit,type = "lines") # scree plot 
fit$scores # the principal components
biplot(fit)

#kmeans clustering
km.res <- kmeans(df, 3, iter.max = 20, nstart = 200)
fviz_cluster(km.res, data = df)
km.res$tot.withinss/km.res$totss




