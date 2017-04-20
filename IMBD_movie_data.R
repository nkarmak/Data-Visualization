# Clear work environment
rm(list = ls())

# Read in data set into a dataframe
df<-read.csv("movie_metadata.csv", header = TRUE , sep = ",")

# Exploratory Data Analysis
summary(df)

