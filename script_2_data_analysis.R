setwd("~/Personal/R-Work/Sales Forecast")

#load the libraries
#install.packages("data.table")
#install.packages("DataCombine")
library(data.table)
library(DataCombine)

##########################################################################
#this function takes a dataframe and the list of columns 
# that it has to create lag on and creates new comuns with lag 
##########################################################################
calculate_lag_columns <- function(x, columns_to_lag, lag_try = -3){
  for(i in columns_to_lag){
    for(j in lag_try:-1){
      x <- slide(data = x,Var = i,slideBy = j)
    }
  }
  x
}

##########################################################################
#this function takes Y variable and a dataframe of x variables 
# and computes the max correlating x variables against y variable.
#this returns the column names of the x variables which is highly correlated 
##########################################################################
best_correlation <- function(y, x_i_i)
{
  corr_df = as.data.frame(t(cor(y,x_i_i,use = "complete.obs")))
  max_corr=max(abs(corr_df))
  max_corr_col_name <-row.names(corr_df)[abs(corr_df$V1) == max_corr]
  max_corr_col_name
}

##########################################################################
###Script starts here..
xy <- fread(input = "xy_cleaned.csv")
xy <- as.data.frame(xy)


#create a data frame with all new lag columns
columns_to_lag <- names(xy)[3:length(names(xy))]
lag_try = -6
xy_lag <- calculate_lag_columns(x = xy,columns_to_lag = columns_to_lag,lag_try = lag_try)
write.csv(x = xy_lag,file = "xy_lag.csv",row.names = F,na="")



####calculate the best lag..

col_names_xy <- names(xy)
col_names_xy <- grep(pattern = "X_",x = col_names_xy,ignore.case = TRUE,value = TRUE)
col_names_all = names(xy_lag)
max_corr_names <- NULL
for(name in col_names_xy)
{
  additional_cols <- grep(pattern = paste(name,"-",sep = ""),x = col_names_all,ignore.case = TRUE,value = TRUE)
  df<- subset(xy_lag,select = c(name,additional_cols))
  max_corr_names[name]  <- best_correlation(y = xy_lag$y,x_i_i = df) 
}

#subset the xy_lag dataframe with the columns with max correlation
xy_best <- xy_lag[c("y",max_corr_names)]
write.csv(x = xy_best,file = "xy_best.csv",row.names = F,na="")
