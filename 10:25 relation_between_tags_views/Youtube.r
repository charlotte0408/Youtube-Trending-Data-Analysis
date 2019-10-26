
library("dplyr")
library("ggplot2")

ytdata <- read.csv(file="USvideos.csv", header=TRUE, sep=",")

names(ytdata)

head(ytdata)

head(ytdata$tags)

tagsdata <- ytdata[c("views","tags")]

head(tagsdata)

strsplit(tagsdata[[2,2]], " ")

tagsdata[[2,2]]

typeof(tagsdata[2,2])
