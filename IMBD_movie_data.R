# Clear work environment
rm(list = ls())

library(ggplot2)
library(qgraph)
library(psych)
library(factoextra)
library(corrplot)

# Read in data set into a dataframe
df <- read.csv("movie_metadata.csv", header = TRUE , sep = ",")

#View top 5 rows of raw data
head(df)

# Cleaning Movie_Title column
df$movie_title <- gsub('.{2}$', '', df$movie_title)

# Exploratory Data Analysis
summary(df)

# Some Scatterplots and Histrograms to visualize interesting variables
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

## Removing columns without numeric values and rows with NA
#Finding indices with numeric columns
nums <- sapply(df, is.numeric)
# New dataframe with only numeric columns
df2 <- as.data.frame(df[,nums])
# Removing NA values and Serial Number column from both dataframes
df <- df[complete.cases(df),]
df <- df[-1]
df2 <- df2[complete.cases(df2),]
df2 <- df2[-1]

#Correlation plot 
corrplot(cor(df2), method = "circle")

# Scale the data frame
df3 <- scale(df2)

# kmeans clustering
km.res <- kmeans(df3, 3, iter.max = 20, nstart = 200)
fviz_cluster(km.res, data = df3)
# Percetage of variation explained by 3 clusters
km.res$tot.withinss/km.res$totss
km.res$centers
centers <- km.res$centers[km.res$cluster, ] # "centers" is a data frame of 3 centers but the length of iris dataset so we can canlculate distance difference easily.
distances <- sqrt(rowSums((df3 - centers)^2))
outliers <- order(distances, decreasing = T)[1:20]

# View The names of 10 movies which are outliers
print(outliers) 
print(df$movie_title[outliers])

# Remove outliers from data and re-run kmeans
df3 <- df3[-outliers,]
df <- df[-outliers,]

#Re-run kmeans
km.res <- kmeans(df3, 3, iter.max = 20, nstart = 200)
fviz_cluster(km.res, data = df3)
km.res$tot.withinss/km.res$totss

# Principal Component Analysis
qg_boxes_pca <- qgraph.pca(df3, factors = 5)
fit <- princomp(df2, cor = TRUE)
summary(fit)
loadings(fit) # pc loadings 
plot(fit,type = "lines") # scree plot 
fit$scores # the principal components
biplot(fit)
pca_fit <- prcomp(df2, scale = TRUE)
pca_fit
summary(pca_fit)

# Common Factor Analysis usinf varimax rotation
FA_fit = factanal(df2, factors = 10, rotation = "varimax", na.action = na.omit)
FA_fit$loadings

# Plot loadings against one another
load = FA_fit$loadings[,1:2]
plot(load, type = "n") # set up plot 
text(load,labels = names(df2),cex = .7) # add variable names
