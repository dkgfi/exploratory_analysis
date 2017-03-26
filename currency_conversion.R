
############################
# CURRENCY CONVERSIONS #
############################

# note: date data needs to be in the format (Month-YR)

# set working directory
###setwd("~/Desktop/gfi")

# path
###data_path = "~/Desktop/gfi"

# files
filenames <- list.files(path=data_path,
                        pattern=".*csv")

# trade data
trade_data <- do.call(rbind,lapply(filenames[grep("trade",filenames)],read.csv))
# unique currencies
unique(trade_data$currency) # EUR, JPY, USD

# specific country files - ethiopia, netherlands, south africa, usa
ethiopia <- read.csv(filenames[grep("ethiopia",filenames)])
netherlands <- read.csv(filenames[grep("netherlands",filenames)])
south_africa <- read.csv(filenames[grep("south_africa",filenames)])
usa <- read.csv(filenames[grep("usa",filenames)])

# currency
currency <- read.csv(filenames[grep("currency",filenames])
currency <- currency[order(currency$id),]
# exchange rate
exchange_rate <- read.csv(filenames[grep("exchange_rate",filenames])
exchange_rate <- exchange_rate[order(exchange_rate$currency_id),]exchange_rate[exchange_rate$currency_id%in%currency$id[currency$code%in%c("EUR","JPY")],]
# merge exchange rate with currency 
currency_convers <- merge(exchange_rate,currency[,c(1,3)],by.x="currency_id",by.y="id")[,c(1,3:4,6)]
currency_convers <- currency_convers[order(currency_convers$code,currency_convers$month),]

# conversion
conversion <- function(x) {
  merge(x[x$currency%in%c("EUR","JPY"),][order(x$currency[x$currency%in%c("EUR","JPY")],
                                                             x$month[x$currency%in%c("EUR","JPY")]),],currency_convers,
        by.x=c("currency","month"),by.y=c("code","month"),all.x=TRUE,all.y=FALSE)
}
# bind
bind <- function(x,x_convers) {
  rbind(x[x$currency%in%c("USD"),],x_convers[,1:ncol(x)])
}
# conversion & bind - ethiopia
ethiopia_convers <- conversion(ethiopia); ethiopia_convers$value <- ethiopia_convers$value*ethiopia_convers$rate
ethiopia <- bind(ethiopia,ethiopia_convers)
# conversion & bind - netherlands
netherlands_convers <- conversion(netherlands); netherlands_convers$value <- netherlands_convers$value*netherlands_convers$rate
netherlands <- bind(netherlands,netherlands_convers)
# conversion & bind - south_africa
south_africa_convers <- conversion(south_africa); south_africa_convers$value <- south_africa_convers$value*south_africa_convers$rate
south_africa <- bind(south_africa,south_africa_convers)
# conversion & bind - usa
usa_convers <- conversion(usa); usa_convers$value <- usa_convers$value*usa_convers$rate
usa <- bind(usa,usa_convers)
# conversion & bind - trade
trade_convers <- conversion(trade_data); trade_convers$value <- trade_convers$value*usa_convers$rate
trade_data <- bind(trade_data,trade_convers)

