
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
trade_data <- do.call(rbind,lapply(filenames[grep("trade",filenames)],read.csv))
# unique currencies
unique(trade_data$currency) # EUR, JPY, USD

# convert month data
trade_data$month <- as.yearmon(trade_data$month)

# currency
currency <- read.csv(filenames[grep("currency",filenames])
currency <- currency[order(currency$id),]
exchange_rate <- read.csv(filenames[grep("exchange_rate",filenames])
exchange_rate <- exchange_rate[order(exchange_rate$currency_id),]exchange_rate[exchange_rate$currency_id%in%currency$id[currency$code%in%c("EUR","JPY")],]
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
                                                                   
# conversion & bind - trade
trade_convers <- conversion(trade_data); trade_convers$value <- trade_convers$value*trade_convers$rate
trade_data <- bind(trade_data,trade_convers)
                                                                   
                                                                   
