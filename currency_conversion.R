

############################
# CURRENCY CONVERSIONS #
############################

# note: this can be applied to country specific data

# note: date data needs to be in the format (Month-YR)
library(zoo)

# set working directory
setwd("~/Desktop/gfi")

# path
data_path = "~/Desktop/gfi"

# files
filenames <- list.files(path=data_path,
                        pattern=".*csv")

# trade data
trade_files <- filenames[grep("trade_src",filenames)]
trade_data1 <- read.csv(trade_files[[1]]); trade_data2 <- read.csv(trade_files[[2]])
trade_data3 <- read.csv(trade_files[[3]]); trade_data4 <- read.csv(trade_files[[4]])
trade_data5 <- read.csv(trade_files[[5]]); trade_data6 <- read.csv(trade_files[[6]])

# currency
currency <- read.csv(filenames[grep("currency",filenames)])
currency <- currency[order(currency$id),]
exchange_rate <- read.csv(filenames[grep("exchange_rate",filenames)])
exchange_rate <- exchange_rate[order(exchange_rate$currency_id),]
exchange_rate <- exchange_rate[exchange_rate$currency_id%in%currency$id[currency$code%in%c("EUR","JPY")],]
# merge exchange rate with currency 
currency_convers <- merge(exchange_rate,currency[,c(1,3)],by.x="currency_id",by.y="id")[,c(1,3:4,6)]
currency_convers <- currency_convers[order(currency_convers$code,currency_convers$month),]
                                                                   
# conversion
conversion <- function(x) {
merge(x[x$currency%in%c("EUR","JPY"),][order(x$currency[x$currency%in%c("EUR","JPY")],x$month[x$currency%in%c("EUR","JPY")]),],currency_convers,
      by.x=c("currency","month"),by.y=c("code","month"),all.x=TRUE,all.y=FALSE)
  }
# bind
bind <- function(x,x_convers) {
  rbind(x[x$currency%in%c("USD"),],x_convers[,1:ncol(x)])
  }
                                                                   
# conversion & bind - trade1
trade_convers1 <- conversion(trade_data1); trade_convers1$value <- trade_convers1$value*trade_convers1$rate
trade_data1 <- bind(trade_data1,trade_convers1); trade_data1$currency = "USD"
write.csv(trade_data1,trade_files[[2]])
# conversion & bind - trade2
trade_convers2 <- conversion(trade_data2); trade_convers2$value <- trade_convers2$value*trade_convers2$rate
trade_data2 <- bind(trade_data2,trade_convers2); trade_data2$currency = "USD"
write.csv(trade_data2,trade_files[[2]])
# conversion & bind - trade3
trade_convers3 <- conversion(trade_data3); trade_convers3$value <- trade_convers3$value*trade_convers3$rate
trade_data3 <- bind(trade_data3,trade_convers3); trade_data3$currency = "USD"
write.csv(trade_data3,trade_files[[2]])
# conversion & bind - trade4
trade_convers4 <- conversion(trade_data4); trade_convers4$value <- trade_convers4$value*trade_convers4$rate
trade_data4 <- bind(trade_data4,trade_convers4); trade_data4$currency = "USD"
write.csv(trade_data4,trade_files[[2]])
# conversion & bind - trade5
trade_convers5 <- conversion(trade_data5); trade_convers5$value <- trade_convers5$value*trade_convers5$rate
trade_data5 <- bind(trade_data5,trade_convers5); trade_data5$currency = "USD"
write.csv(trade_data5,trade_files[[2]])
# conversion & bind - trade6
trade_convers6 <- conversion(trade_data6); trade_convers6$value <- trade_convers6$value*trade_convers6$rate
trade_data6 <- bind(trade_data6,trade_convers6); trade_data6$currency = "USD"
write.csv(trade_data6,trade_files[[6]])

                                     
