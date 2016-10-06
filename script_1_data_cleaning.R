setwd("~/Personal/R-Work/Sales Forecast")

library(data.table)
library(DataCombine)


##########################################################################
#this function takes a vector and  convert it into numeric vector 
#by trimming white spaces and removing COmma
##########################################################################
remove_comma <- function(x = NULL){
  x <- trimws(x)
  out <-  as.numeric(gsub(pattern = ",",replacement = "",x = x))
  out
}



#Cleaning the Y data 
y_data <- fread(input = "y_data.csv")
str(y_data)
y_data <- y_data[3:nrow(y_data),]
names(y_data)[1] <- "Month"
y_data <- subset(y_data,select = c("Month", "CL 8 Production  N.A."))
names(y_data)[2] <- "Y_CL_8_NA_ENGINE_SALES"
y_data$Month <- as.Date(paste("01-",y_data$Month,sep = "" ),format = "%d-%b-%y")
str(y_data)
y_col_names <- names(y_data)
y_col_names <- as.data.frame(y_col_names)
y_col_names$index <- as.character(y_col_names$y_col_names)
y_col_names$index[2] <- "y"
names(y_col_names)[1] <- "variable"
names(y_data)[2] <- "y"

#convert data to numeric
y_data <- as.data.frame(y_data)
class(y_data)
for(i in 2:ncol(y_data)){
  y_data[,i] <- remove_comma(y_data[,i])
}
write.csv(x = y_data,file = "y_data_cleaned.csv",row.names = FALSE,na="")


#Cleaning the X data 
x_data <- fread(input = "x_data.csv")
x_data <- x_data[2:nrow(x_data),]
names(x_data)[1] <- "Month"
Column_Names <- names(x_data)
Column_Names <- as.data.frame(Column_Names)
Column_Names$index <- Column_Names$Column_Names 
Column_Names$index <- as.character(Column_Names$index)
str(Column_Names)
for( i in 2:nrow(Column_Names)){
  names(x_data)[i] <- paste("x_",i-1,sep = "")
  Column_Names[i,2] <- paste("x_",i-1,sep = "")
}
names(Column_Names)[1] <- "variable"
final_column <- rbind(y_col_names,Column_Names)
final_column<- subset(final_column,index != "Month")
#write the column names in the file for future reference
write.csv(x = final_column,file = "column_index_dictionary.csv",row.names = FALSE)

x_data$Month[1:60] <-as.Date(paste("01-",x_data$Month[1:60],sep = "" ),format = "%d-%b-%y")
x_data$Month[61:nrow(x_data)] <- as.Date(paste(x_data$Month[61:nrow(x_data)],"-01",sep = "" ),format = "%y-%b-%d")
class(x_data$Month) <- "Date"
#write the cleaned file for x_vector
x_data <- as.data.frame(x_data)
#convert data to numeric
for(i in 2:ncol(x_data)){
  x_data[,i] <- remove_comma(x_data[,i])
}
write.csv(x = x_data,file = "x_data_cleaned.csv",row.names = FALSE,na="")





##
#now the x and y files have been cleaned we can mearge them and save them as xy_cleaned
#merge the files
#load the files
y_data_cleaned <- fread(input = "y_data_cleaned.csv")
x_data_cleaned <- fread(input = "x_data_cleaned.csv")

#join the data
xy <- merge(y_data_cleaned,x_data_cleaned,all.x = TRUE,by = "Month")
xy <- as.data.frame(xy)
write.csv(x = xy,file = "xy_cleaned.csv",row.names = FALSE,na = "")

rm(list=ls())
