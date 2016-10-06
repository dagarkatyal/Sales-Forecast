setwd("~/Personal/R-Work/Sales Forecast")

#load the libraries
#install.packages("data.table")
#install.packages("DataCombine")
library(data.table)
library(DataCombine)

xy <- fread(input = "xy_best.csv")
xy <- as.data.frame(xy)
dim(xy);dim(na.omit(xy))

xy <-  as.data.frame(na.omit(xy))
y <- xy$y
xx <- xy[,2:ncol(xy)] 


pc <- prcomp(xx, center = TRUE, scale = FALSE)
summary(pc)

plot(cumsum(pc$sdev^2/sum(pc$sdev^2))) #cumulative explained variance

pc.use <- 6 # best result
#####################3

xy_new <- as.data.frame(cbind(y,pc$x[,1:pc.use]))
fit <- lm(data = xy_new)
summary(fit)
