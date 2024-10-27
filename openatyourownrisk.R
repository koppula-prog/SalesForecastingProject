library(ggplot2)
library(readr)
library(tidyverse)
library(dplyr)
library(janitor)
library(naniar)
library(mapview)
library(sf)
install.packages("sqldf")
library(sqldf)

getwd()
setwd ("C:\\Users\\saikrishna\\Documents\\McDannielSchoolWork\\")
data <- read.csv("train.csv")

names(data)

storesales <- data %>% group_by(Store, Date) %>% summarise(sum.Sales = sum(Sales))

storesales

storehistogram <- storesales %>% group_by(Store) %>% summarise(totalsales = sum(sum.Sales))

storehistogram

top20store = sqldf("select * from storehistogram order by totalsales desc limit 20")

top20store$Store = as.character(top20store$Store)

library(scales)
ggplot(top20store, aes(x= reorder(Store,totalsales), y=totalsales)) + geom_bar(stat = "identity") + scale_y_continuous(labels = comma) +
  labs(title = "Sales by top 20 stores", x = "Stores", y= "Sales")

storesales <- storesales[storesales$Store==262,]
ts_262 <-select(storesales, Date, sum.Sales,-Store)
ts_262 = subset(storesales, select = -c(Store) )

ts_262$Date<-as.Date(ts_262$Date)
ggplot(ts_262, aes(x = Date, y = sum.Sales)) +
  geom_line() +
  labs(title = "Time Series of Sales", x = "Date", y = "Sales") +
  theme_minimal()

ts_262<-ts_262 %>% rename(ds = Date, y = sum.Sales)
model_prophet<-prophet(ts_262)

future<- make_future_dataframe(model_prophet, periods = 42)
forecast<- predict(model_prophet, future)

forecast



# Select only relevant columns from the forecast

forecast_plot_data <- forecast %>%
  
  select(ds, yhat) %>%
  
  rename(Forecasted = yhat)



# Rename `y` in the original data to `Actual`

ts_262 <- ts_262 %>% rename(y = sum.Sales)



# Join actual data with forecast data by `ds` (date)


plot_data <- left_join(forecast_plot_data, ts_262, by = "ds")



# Plotting the data

ggplot(plot_data, aes(x = ds)) +
  
  geom_line(aes(y = y, color = "Actual"), size = 1) +
  
  geom_line(aes(y = Forecasted, color = "Forecasted"), size = 1, linetype = "dashed") +
  
  labs(title = "Actual vs Forecasted Sales",
       
       x = "Date",
       
       y = "Sales") +
  
  scale_color_manual(values = c("Actual" = "blue", "Forecasted" = "red")) +
  
  theme_minimal()

test <- read.csv("test.csv")
test_262 <- test[test$Store==262,] 

write.csv(plot_data,'predicted_data.csv')

test_262 = subset(test_262, select = -c(Store) )
train_rmse<- plot_data[plot_data$y == NA]
train_rmse$error <- train





plot_ly(storehistogram, x = ~Store, y = ~totalsales, type = "bar", name = 'total store sales')


  group_by(country, year) %>%
  summarise(sum.amount = sum(amount)) %>%

summary(data)

data$data[is.na(data$reviews_per_month)]
