
############################
# CURRENCY CONVERSIONS #
############################

# note: date data needs to be in the form (Month-YR)

# set working directory
setwd("~/Desktop/gfi")

# files
filenames <- list.files(path="~/Desktop/gfi",
                        pattern=".*csv")

# trade files
trade_files <- lapply(filenames[grep("trade",filenames)],read.csv)
# trade data
trade_data <- do.call(rbind,trade_files)
# unique currencies
unique(trade_data$currency) # EUR, JPY, USD

# specific country files - ethiopia
ethiopia <- read.csv(filenames[grep("ethiopia",filenames)])
# specific country files - netherlands
netherlands <- read.csv(filenames[grep("netherlands",filenames)])
# specific country files - south africa
south_africa <- read.csv(filenames[grep("south_africa",filenames)])
# specific country files - usa
usa <- read.csv(filenames[grep("usa",filenames)])

# currency
currency <- read.csv(filenames[3])
currency <- currency[order(currency$id),]
# exchange rate
exchange_rate <- read.csv(filenames[7])
exchange_rate <- exchange_rate[order(exchange_rate$currency_id),]
exchange_rate <- exchange_rate[exchange_rate$currency_id%in%currency$id[currency$code%in%c("EUR","JPY")],]
# merge exchange rate with currency 
currency_convers <- merge(exchange_rate,currency[,c(1,3)],by.x="currency_id",by.y="id")[,c(1,3:4,6)]
currency_convers <- currency_convers[order(currency_convers$code,currency_convers$month),]

# conversion
conversion <- function(x) {
  merge(x[x$currency%in%c("EUR","JPY"),][order(x$currency[x$currency%in%c("EUR","JPY")],
                                                             x$month[x$currency%in%c("EUR","JPY")]),],currency_convers,
        by.x=c("currency","month"),by.y=c("code","month"),all.x=TRUE,all.y=FALSE)
}
# conversions - ethiopia
ethiopia_convers <- conversion(ethiopia)
ethiopia_convers$value <- ethiopia_convers$value*ethiopia_convers$rate
# conversions - netherlands
netherlands_convers <- conversion(netherlands)
netherlands_convers$value <- netherlands_convers$value*netherlands_convers$rate
# conversions - south_africa
south_africa_convers <- conversion(south_africa)
south_africa_convers$value <- south_africa_convers$value*south_africa_convers$rate
# conversions - usa
usa_convers <- conversion(usa)
usa_convers$value <- usa_convers$value*usa_convers$rate
# conversions - trade
trade_convers <- conversion(trade_data)
trade_convers$value <- trade_convers$value*usa_convers$rate

# bind
bind <- function(x,x_convers) {
  rbind(x[x$currency%in%c("USD"),],x_convers[,1:ncol(x)])
}
# bind - ethiopia
ethiopia <- bind(ethiopia,ethiopia_convers)
# bind - netherlands
netherlands <- bind(netherlands,netherlands_convers)
# bind - south_africa
south_africa <- bind(south_africa,south_africa_convers)
# bind - usa
usa <- bind(usa,usa_convers)
# bind - trade
trade_data <- bind(trade_data,trade_convers)

